# app.R
# NF_FLU Influenza Dashboard (filesystem-based)
# - Runs table + selected run details below (no tabs)
# - Samplesheet is authoritative for sample counting & listing
# - "Passed samples" primarily from read_count/pass_read_count_samples_mqc.tsv
# - Integrity checks: samplesheet vs nextclade/subtyping/merged-fastq
#
# Tested assumptions based on your data:
# - samplesheet.csv header: sample,fastq_1,fastq_2
# - read_count file: results/read_count/pass_read_count_samples_mqc.tsv with Sample + Read count

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(DT)
library(purrr)
library(fs)

# ---------------- CONFIG ----------------

# For testing: use backup root
# (Production later: /data/NF/NF_FLU)
DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/backup_NF_FLU_backup")
# DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/NF_FLU")  # production

BASE_URL <- Sys.getenv("NF_FLU_BASE_URL", unset = "")
if (BASE_URL == "") BASE_URL <- NULL

# Influenza samples (matches your usage: starts with 326)
SAMPLE_ID_REGEX <- Sys.getenv("NF_FLU_SAMPLE_ID_REGEX", unset = "^326")

REFRESH_MS <- as.integer(Sys.getenv("NF_FLU_REFRESH_MS", unset = "60000"))

# ---------------- helpers ----------------

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && a != "") a else b
rtrim_slash <- function(x) sub("/+$", "", x)

path_to_url <- function(fs_path) {
  if (is.null(BASE_URL)) return(NULL)
  rel <- path_rel(fs_path, start = DATA_ROOT)
  rel <- gsub("\\\\", "/", rel)
  paste0(rtrim_slash(BASE_URL), "/", rel)
}

# Resolve results/ directory for a run.
# Supports:
#  A) <RUN>/<PIPELINE_RUN>/results
#  B) <RUN>/results
resolve_results_dir <- function(run_dir) {
  direct <- path(run_dir, "results")
  if (dir_exists(direct)) {
    return(list(
      run_dir = run_dir,
      pipeline_dir = run_dir,
      results_dir = direct,
      mode = "direct",
      mtime = file_info(direct)$modification_time
    ))
  }

  subs <- dir_ls(run_dir, type = "directory", recurse = FALSE)
  if (length(subs) == 0) return(NULL)

  candidates <- keep(subs, ~dir_exists(path(.x, "results")))
  if (length(candidates) == 0) return(NULL)

  mt <- map_dfr(candidates, ~{
    info <- file_info(.x)
    tibble(pipeline_dir = .x, mtime = info$modification_time)
  }) %>% arrange(desc(mtime))

  chosen <- mt$pipeline_dir[[1]]
  list(
    run_dir = run_dir,
    pipeline_dir = chosen,
    results_dir = path(chosen, "results"),
    mode = "nested",
    mtime = mt$mtime[[1]]
  )
}

# Find samplesheet path (supports both your backup structure and generated one)
find_samplesheet_path <- function(pipeline_dir, results_dir) {
  candidates <- c(
    path(pipeline_dir, "samplesheet.csv"),
    path(results_dir, "pipeline_info", "samplesheet.fixed.csv"),
    path(results_dir, "pipeline_info", "samplesheet.csv"),
    path(pipeline_dir, "pipeline_info", "samplesheet.fixed.csv")
  )
  hit <- candidates[file_exists(candidates)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

# Read samplesheet: EXPECTS a column named 'sample'
# Returns tibble(sample_id, fastq_1, fastq_2, ...)
read_samplesheet <- function(samplesheet_path) {
  if (is.na(samplesheet_path) || !file_exists(samplesheet_path)) return(NULL)

  ss <- suppressWarnings(read_csv(samplesheet_path, show_col_types = FALSE))
  if (!("sample" %in% names(ss))) {
    # fallback: try common names
    nms <- names(ss)
    cand <- intersect(nms, c("sample_id", "Sample", "SampleID", "id"))
    if (length(cand) >= 1) {
      ss <- ss %>% rename(sample = all_of(cand[[1]]))
    } else {
      return(NULL)
    }
  }

  ss %>% rename(sample_id = sample) %>% mutate(sample_id = as.character(sample_id))
}

# nf-flu outputs
read_subtyping <- function(results_dir) {
  f <- path(results_dir, "subtyping_report", "subtype_results.csv")
  if (!file_exists(f)) return(NULL)
  suppressWarnings(read_csv(f, show_col_types = FALSE))
}

read_nextclade <- function(results_dir) {
  f <- path(results_dir, "nextclade", "nextclade.tsv")
  if (!file_exists(f)) return(NULL)
  suppressWarnings(read_tsv(f, show_col_types = FALSE))
}

# Read pass read-count table (authoritative "passed samples" list)
# Expected path: results/read_count/pass_read_count_samples_mqc.tsv
read_pass_readcount <- function(results_dir) {
  f <- path(results_dir, "read_count", "pass_read_count_samples_mqc.tsv")
  if (!file_exists(f)) return(NULL)

  # It's typically tab-delimited, but sometimes looks space-aligned.
  # We'll try read_tsv first; if we don't get 2 columns, fallback to whitespace.
  rc <- suppressWarnings(read_tsv(f, show_col_types = FALSE))
  if (ncol(rc) < 2) {
    rc <- suppressWarnings(read_delim(f, delim = "\\s+", show_col_types = FALSE, trim_ws = TRUE))
  }

  # normalize column names
  nms <- names(rc)
  # likely: "Sample" and "Read count" (or "Read.count")
  sample_col <- intersect(nms, c("Sample", "sample", "SAMPLE"))
  read_col <- intersect(nms, c("Read count", "Read_count", "Read.count", "read_count", "ReadCount"))

  if (length(sample_col) == 0) {
    # fallback: first column
    sample_col <- nms[1]
  } else {
    sample_col <- sample_col[1]
  }

  if (length(read_col) == 0) {
    # fallback: second column
    read_col <- nms[min(2, length(nms))]
  } else {
    read_col <- read_col[1]
  }

  rc %>%
    transmute(
      sample_id = as.character(.data[[sample_col]]),
      read_count = suppressWarnings(as.numeric(.data[[read_col]]))
    ) %>%
    filter(!is.na(sample_id) & sample_id != "") %>%
    distinct(sample_id, .keep_all = TRUE)
}

standardize_sample_id <- function(df) {
  if (is.null(df)) return(df)
  nms <- names(df)
  cand <- intersect(nms, c("sample", "sample_id", "seqName", "name", "sequence_name", "SequenceName"))
  if (length(cand) >= 1) df <- df %>% rename(sample_id = all_of(cand[[1]]))
  if (!("sample_id" %in% names(df))) df$sample_id <- NA_character_
  df %>% mutate(sample_id = as.character(sample_id))
}

# Normalize IDs from outputs if something accidentally appends _1/_2
# Safe for your FLI-1 examples because those end with "-1", not "_1"
normalize_id <- function(x) {
  x <- as.character(x)
  str_replace(x, "_[12]$", "")
}

extract_HN <- function(sub_df) {
  if (is.null(sub_df)) return(NULL)
  sub_df <- standardize_sample_id(sub_df) %>% mutate(sample_id = normalize_id(sample_id))
  nms <- names(sub_df)

  type_col <- intersect(nms, c("influenza_type", "type", "virus_type", "InfluenzaType"))
  sub_col  <- intersect(nms, c("subtype", "subtype_prediction", "prediction", "Subtype"))

  out <- sub_df
  if (length(type_col) >= 1) out <- out %>% rename(influenza_type = all_of(type_col[[1]]))
  if (length(sub_col)  >= 1) out <- out %>% rename(subtype = all_of(sub_col[[1]]))

  h_col <- intersect(nms, c("H", "H_subtype", "HA", "ha_subtype", "H_type"))
  n_col <- intersect(nms, c("N", "N_subtype", "NA", "na_subtype", "N_type"))
  if (length(h_col) >= 1) out <- out %>% rename(H = all_of(h_col[[1]]))
  if (length(n_col) >= 1) out <- out %>% rename(N = all_of(n_col[[1]]))

  if (!("H" %in% names(out)) || !("N" %in% names(out))) {
    if ("subtype" %in% names(out)) {
      st <- as.character(out$subtype)
      if (!("H" %in% names(out))) out$H <- str_extract(st, "H\\d+")
      if (!("N" %in% names(out))) out$N <- str_extract(st, "N\\d+")
      if (!("influenza_type" %in% names(out))) out$influenza_type <- str_extract(st, "^[AB]")
    }
  }

  out
}

extract_nextclade <- function(nx_df) {
  if (is.null(nx_df)) return(NULL)
  nx_df <- standardize_sample_id(nx_df) %>% mutate(sample_id = normalize_id(sample_id))
  nms <- names(nx_df)

  out <- nx_df

  clade_col <- intersect(nms, c("clade", "Clade"))
  subcl_col  <- intersect(nms, c("subclade", "Subclade"))
  if (length(clade_col) >= 1) out <- out %>% rename(clade = all_of(clade_col[[1]]))
  if (length(subcl_col)  >= 1) out <- out %>% rename(subclade = all_of(subcl_col[[1]]))

  qc_status_col <- intersect(nms, c("qc.overallStatus", "qc_overallStatus", "overallStatus"))
  qc_score_col  <- intersect(nms, c("qc.overallScore", "qc_overallScore", "overallScore"))
  if (length(qc_status_col) >= 1) out <- out %>% rename(qc_status = all_of(qc_status_col[[1]]))
  if (length(qc_score_col)  >= 1) out <- out %>% rename(qc_score = all_of(qc_score_col[[1]]))

  keep_cols <- intersect(names(out), c("sample_id", "clade", "subclade", "qc_status", "qc_score"))
  if (length(keep_cols) >= 2) out <- out %>% select(all_of(keep_cols))
  out
}

# Infer sample IDs from merged fastq names in results/fastq/
infer_samples_from_merged_fastq <- function(results_dir) {
  fastq_dir <- path(results_dir, "fastq")
  if (!dir_exists(fastq_dir)) return(character(0))

  files <- dir_ls(fastq_dir, type = "file", glob = "*.merged.fastq.gz")
  if (length(files) == 0) return(character(0))

  bases <- path_file(files)
  samp <- str_match(bases, "^(.*)_[12]\\.merged\\.fastq\\.gz$")[, 2]
  samp <- samp[!is.na(samp)]
  unique(samp)
}

# Compare expected sample set with observed set
compare_sets <- function(expected, observed) {
  list(
    missing = sort(setdiff(expected, observed)),
    extra   = sort(setdiff(observed, expected))
  )
}

# Compute run-level metrics using samplesheet as truth
compute_run_metrics <- function(pipeline_dir, results_dir) {
  ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
  ss <- read_samplesheet(ss_path)

  ss_samples <- character(0)
  if (!is.null(ss) && "sample_id" %in% names(ss)) {
    ss_samples <- unique(na.omit(as.character(ss$sample_id)))
  }

  ss_match <- ss_samples[str_detect(ss_samples, SAMPLE_ID_REGEX)]
  match_n <- length(ss_match)

  # Passed samples: primarily from pass_read_count_samples_mqc.tsv
  rc <- read_pass_readcount(results_dir)
  pass_n <- 0L

  if (!is.null(rc)) {
    # passed = in samplesheet matching set and has read_count > 0
    pass_n <- rc %>%
      filter(sample_id %in% ss_match, !is.na(read_count), read_count > 0) %>%
      summarise(n = n()) %>% pull(n)
    pass_n <- as.integer(pass_n %||% 0L)
  } else {
    # fallback to nextclade QC if rc doesn't exist
    nx <- extract_nextclade(read_nextclade(results_dir))
    if (!is.null(nx) && "qc_status" %in% names(nx)) {
      nx2 <- nx %>%
        mutate(qc_status_l = tolower(as.character(qc_status)),
               is_pass = qc_status_l %in% c("good", "pass", "passed"))
      pass_n <- sum(nx2$sample_id %in% ss_match & nx2$is_pass, na.rm = TRUE)
    } else {
      pass_n <- 0L
    }
  }

  # Integrity checks
  nx <- extract_nextclade(read_nextclade(results_dir))
  sub <- extract_HN(read_subtyping(results_dir))
  mfq <- infer_samples_from_merged_fastq(results_dir)

  nx_samples  <- if (!is.null(nx)) unique(na.omit(as.character(nx$sample_id))) else character(0)
  sub_samples <- if (!is.null(sub)) unique(na.omit(as.character(sub$sample_id))) else character(0)

  # Only judge integrity for expected matching samples
  expected <- unique(ss_match)

  has_ss <- !is.na(ss_path) && file_exists(ss_path) && length(ss_samples) > 0
  expected_n <- length(expected)

  has_nx  <- file_exists(path(results_dir, "nextclade", "nextclade.tsv"))
  has_sub <- file_exists(path(results_dir, "subtyping_report", "subtype_results.csv"))
  has_mfq <- dir_exists(path(results_dir, "fastq"))
  has_rc  <- file_exists(path(results_dir, "read_count", "pass_read_count_samples_mqc.tsv"))

  nx_cmp  <- compare_sets(expected, intersect(nx_samples, expected))
  sub_cmp <- compare_sets(expected, intersect(sub_samples, expected))
  mfq_cmp <- compare_sets(expected, intersect(mfq, expected))
  rc_cmp  <- if (!is.null(rc)) compare_sets(expected, intersect(rc$sample_id, expected)) else list(missing = character(0), extra = character(0))

  # Also track "extra" samples in outputs not in expected (useful diagnostic)
  nx_extra  <- setdiff(nx_samples, expected)
  sub_extra <- setdiff(sub_samples, expected)
  mfq_extra <- setdiff(mfq, expected)
  rc_extra  <- if (!is.null(rc)) setdiff(rc$sample_id, expected) else character(0)

  issue <- FALSE
  issue_msg <- ""

  if (!has_ss) {
    issue <- TRUE
    issue_msg <- "No/empty samplesheet"
  } else if (expected_n == 0) {
    issue <- FALSE
    issue_msg <- ""
  } else {
    missing_total <- 0L
    extra_total <- 0L

    if (has_nx)  missing_total <- missing_total + length(nx_cmp$missing)
    if (has_sub) missing_total <- missing_total + length(sub_cmp$missing)
    if (has_mfq) missing_total <- missing_total + length(mfq_cmp$missing)
    if (has_rc)  missing_total <- missing_total + length(rc_cmp$missing)

    if (has_nx)  extra_total <- extra_total + length(nx_extra)
    if (has_sub) extra_total <- extra_total + length(sub_extra)
    if (has_mfq) extra_total <- extra_total + length(mfq_extra)
    if (has_rc)  extra_total <- extra_total + length(rc_extra)

    if (missing_total > 0 || extra_total > 0) {
      issue <- TRUE
      issue_msg <- paste0("Mismatch (missing=", missing_total, ", extra=", extra_total, ")")
    }
  }

  list(
    samplesheet_path = ss_path,
    ss_match_samples = expected,
    match_n = match_n,
    pass_n = as.integer(pass_n),
    issue = issue,
    issue_msg = issue_msg
  )
}

# Create run registry dataframe
list_runs <- function() {
  if (!dir_exists(DATA_ROOT)) return(tibble())

  run_dirs <- dir_ls(DATA_ROOT, type = "directory", recurse = FALSE)

  rows <- map(run_dirs, function(rd) {
    resolved <- resolve_results_dir(rd)

    if (is.null(resolved)) {
      tibble(
        run = basename(rd),
        status = "no_results",
        run_dir = rd,
        pipeline_dir = NA_character_,
        results_dir = NA_character_,
        updated = file_info(rd)$modification_time,
        match_samples = 0L,
        passed_samples = 0L,
        issue = TRUE,
        issue_msg = "No results",
        multiqc_path = NA_character_,
        multiqc_url = NA_character_,
        samplesheet_path = NA_character_
      )
    } else {
      results_dir <- resolved$results_dir
      pipeline_dir <- resolved$pipeline_dir

      multiqc_path <- path(results_dir, "MultiQC", "multiqc_report.html")
      mq_url <- if (file_exists(multiqc_path)) path_to_url(multiqc_path) else NULL

      has_nextclade <- file_exists(path(results_dir, "nextclade", "nextclade.tsv"))
      has_subtyping <- file_exists(path(results_dir, "subtyping_report", "subtype_results.csv"))
      status <- if (has_nextclade && has_subtyping) "done" else "partial"

      metrics <- compute_run_metrics(pipeline_dir, results_dir)

      tibble(
        run = basename(rd),
        status = status,
        run_dir = rd,
        pipeline_dir = pipeline_dir,
        results_dir = results_dir,
        updated = resolved$mtime,
        match_samples = metrics$match_n,
        passed_samples = metrics$pass_n,
        issue = metrics$issue,
        issue_msg = metrics$issue_msg,
        multiqc_path = if (file_exists(multiqc_path)) multiqc_path else NA_character_,
        multiqc_url = mq_url %||% NA_character_,
        samplesheet_path = metrics$samplesheet_path
      )
    }
  })

  bind_rows(rows) %>% arrange(desc(updated))
}

# ---------------- UI ----------------

ui <- fluidPage(
  titlePanel("NF_FLU Influenza Dashboard"),
  tags$style(HTML("
    .small-note { color: #666; font-size: 12px; }
  ")),
  fluidRow(
    column(
      12,
      h3("Runs"),
      fluidRow(
        column(
          6,
          checkboxInput(
            "include_empty_runs",
            label = "Include runs without any matching samples (based on samplesheet)",
            value = FALSE
          ),
          div(class = "small-note", sprintf("Matching regex: %s", SAMPLE_ID_REGEX))
        ),
        column(
          6,
          checkboxInput(
            "include_no_pass_runs",
            label = "Include runs without any passed samples (read_count or Nextclade QC)",
            value = FALSE
          ),
          div(class = "small-note", "Passed: in pass_read_count_samples_mqc.tsv (fallback: nextclade qc.overallStatus=='good').")
        )
      ),
      DTOutput("runs_tbl"),
      hr(),
      uiOutput("run_header"),
      DTOutput("samples_tbl")
    )
  )
)

# ---------------- SERVER ----------------

server <- function(input, output, session) {

  runs_r <- reactiveVal(list_runs())

  observe({
    invalidateLater(REFRESH_MS, session)
    runs_r(list_runs())
  })

  filtered_runs <- reactive({
    runs <- runs_r()
    if (nrow(runs) == 0) return(runs)

    if (!isTRUE(input$include_empty_runs)) {
      runs <- runs %>% filter(match_samples > 0)
    }
    if (!isTRUE(input$include_no_pass_runs)) {
      runs <- runs %>% filter(passed_samples > 0)
    }
    runs
  })

  output$runs_tbl <- renderDT({
    runs <- filtered_runs()

    if (nrow(runs) == 0) {
      return(datatable(
        data.frame(Message = "No runs match the current filters."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    disp <- runs %>%
      transmute(
        ` ` = case_when(
          issue ~ "\U0001F6D1",              # 🛑
          status == "done" ~ "\U0001F7E2",   # 🟢
          status == "partial" ~ "\U0001F7E1",# 🟡
          status == "no_results" ~ "\U000026AA", # ⚪
          TRUE ~ "\U0001F534"               # 🔴
        ),
        Run = run,
        Status = status,
        Matching = match_samples,
        Passed = passed_samples,
        Integrity = ifelse(issue, issue_msg, "OK"),
        Updated = format(updated, "%Y-%m-%d %H:%M"),
        MultiQC = ifelse(
          !is.na(multiqc_url),
          paste0('<a href="', multiqc_url, '" target="_blank">MultiQC</a>'),
          ifelse(!is.na(multiqc_path), "Local", "—")
        )
      )

    datatable(
      disp,
      escape = FALSE,
      selection = "single",
      options = list(pageLength = 15, autoWidth = TRUE)
    ) %>%
      formatStyle(
        "Status",
        backgroundColor = styleEqual(
          c("done", "partial", "no_results"),
          c("#dff0d8", "#fcf8e3", "#eeeeee")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Matching",
        backgroundColor = styleInterval(
          c(0, 5),
          c("#f2dede", "#fcf8e3", "#dff0d8")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Passed",
        backgroundColor = styleInterval(
          c(0, 5),
          c("#f2dede", "#fcf8e3", "#dff0d8")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Integrity",
        backgroundColor = JS("function(value){ return (value === 'OK') ? '#dff0d8' : '#f2dede'; }"),
        fontWeight = "bold"
      )
  })

  selected_run <- reactive({
    runs <- filtered_runs()
    idx <- input$runs_tbl_rows_selected
    if (length(idx) != 1) return(NULL)
    runs[idx, , drop = FALSE]
  })

  output$run_header <- renderUI({
    sr <- selected_run()
    if (is.null(sr)) return(NULL)

    mq <- if (!is.na(sr$multiqc_url)) {
      tags$a(href = sr$multiqc_url, target = "_blank", "Open MultiQC report")
    } else if (!is.na(sr$multiqc_path)) {
      tags$span("MultiQC (local path): ", tags$code(sr$multiqc_path))
    } else {
      tags$span("MultiQC: —")
    }

    ss <- if (!is.na(sr$samplesheet_path)) {
      tags$span("Samplesheet: ", tags$code(sr$samplesheet_path))
    } else {
      tags$span("Samplesheet: —")
    }

    integrity <- if (isTRUE(sr$issue)) {
      tags$p(tags$b("Integrity: "), tags$span(style = "color:#a94442; font-weight:700;", sr$issue_msg))
    } else {
      tags$p(tags$b("Integrity: "), tags$span(style = "color:#3c763d; font-weight:700;", "OK"))
    }

    tags$div(
      h3(paste0("Run: ", sr$run)),
      tags$p(
        tags$b("Status: "), sr$status,
        " | ", tags$b("Matching: "), sr$match_samples,
        " | ", tags$b("Passed: "), sr$passed_samples
      ),
      integrity,
      tags$p(mq),
      tags$p(ss),
      tags$p(tags$small(tags$code(sr$results_dir)))
    )
  })

  output$samples_tbl <- renderDT({
    sr <- selected_run()
    if (is.null(sr) || is.na(sr$results_dir) || is.na(sr$pipeline_dir)) return(datatable(data.frame()))

    results_dir <- sr$results_dir[[1]]
    pipeline_dir <- sr$pipeline_dir[[1]]

    # Authoritative samples from samplesheet
    ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
    ss <- read_samplesheet(ss_path)

    if (is.null(ss) || !"sample_id" %in% names(ss)) {
      return(datatable(
        data.frame(Message = "No readable samplesheet found for this run."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    ss_match <- ss %>%
      distinct(sample_id, .keep_all = TRUE) %>%
      mutate(sample_id = as.character(sample_id)) %>%
      filter(str_detect(sample_id, SAMPLE_ID_REGEX))

    if (nrow(ss_match) == 0) {
      return(datatable(
        data.frame(Message = "Samplesheet found, but no samples match the influenza sample regex for this dashboard."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    # Outputs
    sub <- extract_HN(read_subtyping(results_dir))
    nx  <- extract_nextclade(read_nextclade(results_dir))
    rc  <- read_pass_readcount(results_dir)

    # Start from samplesheet only, then join outputs
    df <- ss_match %>%
      left_join(sub, by = "sample_id") %>%
      left_join(nx, by = "sample_id") %>%
      left_join(rc, by = "sample_id") %>%
      mutate(
        has_subtyping = !is.na(H) | !is.na(N) | !is.na(subtype) | !is.na(influenza_type),
        has_nextclade = !is.na(clade) | !is.na(subclade) | !is.na(qc_status),
        has_reads = !is.na(read_count),
        passed_reads = !is.na(read_count) & read_count > 0
      )

    # Display columns
    show_cols <- c("sample_id",
                   intersect(names(df), c("read_count", "passed_reads",
                                          "influenza_type", "H", "N", "subtype",
                                          "clade", "subclade", "qc_status", "qc_score",
                                          "has_reads", "has_subtyping", "has_nextclade")))
    show_cols <- unique(show_cols)
    disp <- df %>% select(all_of(show_cols))

    dt <- datatable(
      disp,
      options = list(pageLength = 25, autoWidth = TRUE),
      rownames = FALSE
    )

    # Highlight booleans and QC
    if ("qc_status" %in% names(disp)) {
      dt <- dt %>% formatStyle(
        "qc_status",
        backgroundColor = styleEqual(
          c("good", "mediocre", "bad", "fail", "warning", "unknown"),
          c("#dff0d8", "#fcf8e3", "#f2dede", "#f2dede", "#fcf8e3", "#eeeeee")
        ),
        fontWeight = "bold"
      )
    }

    if ("passed_reads" %in% names(disp)) {
      dt <- dt %>% formatStyle(
        "passed_reads",
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#dff0d8", "#f2dede")),
        fontWeight = "bold"
      )
    }

    for (col in c("has_reads", "has_subtyping", "has_nextclade")) {
      if (col %in% names(disp)) {
        dt <- dt %>% formatStyle(
          col,
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#dff0d8", "#f2dede")),
          fontWeight = "bold"
        )
      }
    }

    # Highlight unusual H types
    if ("H" %in% names(disp)) {
      dt <- dt %>% formatStyle(
        "H",
        backgroundColor = styleEqual(c("H5", "H7"), c("#f2dede", "#f2dede")),
        fontWeight = styleEqual(c("H5", "H7"), c("bold", "bold"))
      )
    }

    dt
  })
}

shinyApp(ui, server)
