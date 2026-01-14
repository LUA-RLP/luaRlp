# app.R
# NF_FLU Influenza Dashboard (filesystem-based)
#
# Core goals:
# - Runs table + selected run details below (no tabs)
# - Counts & filtering based on SAMPLESHEET (authoritative)
# - Integrity checks: samplesheet vs outputs (nextclade/subtyping/merged-fastq)
# - Highlighting in UI

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(DT)
library(purrr)
library(fs)

# ---------------- CONFIG ----------------

DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/NF_FLU")

BASE_URL <- Sys.getenv("NF_FLU_BASE_URL", unset = "")
if (BASE_URL == "") BASE_URL <- NULL

# "Influenza sample IDs" (matches your logic: samples beginning with 326)
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
#  A) /data/NF/NF_FLU/<RUN>/<PIPELINE_RUN>/results
#  B) /data/NF/NF_FLU/<RUN>/results
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

# Find samplesheet path (supports both your backup structure and our generated one)
find_samplesheet_path <- function(pipeline_dir, results_dir) {
  candidates <- c(
    path(pipeline_dir, "samplesheet.csv"),
    path(results_dir, "pipeline_info", "samplesheet.fixed.csv"),
    path(results_dir, "pipeline_info", "samplesheet.csv")
  )
  hit <- candidates[file_exists(candidates)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

# Read samplesheet and standardize sample_id column
read_samplesheet <- function(samplesheet_path) {
  if (is.na(samplesheet_path) || !file_exists(samplesheet_path)) return(NULL)
  ss <- suppressWarnings(read_csv(samplesheet_path, show_col_types = FALSE))

  # Guess sample column
  nms <- names(ss)
  cand <- intersect(nms, c("sample", "sample_id", "id", "sample_name", "Sample", "SampleID"))
  if (length(cand) >= 1) {
    ss <- ss %>% rename(sample_id = all_of(cand[[1]]))
  } else {
    # if no obvious column, still return but sample_id will be NA
    ss$sample_id <- NA_character_
  }
  ss
}

# nf-flu: read outputs
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

standardize_sample_id <- function(df) {
  if (is.null(df)) return(df)
  nms <- names(df)
  cand <- intersect(nms, c("sample", "sample_id", "seqName", "name", "sequence_name", "SequenceName"))
  if (length(cand) >= 1) {
    df <- df %>% rename(sample_id = all_of(cand[[1]]))
  } else {
    df$sample_id <- NA_character_
  }
  df
}

# Normalize IDs from outputs in case something accidentally appends _1/_2
# This is SAFE for your examples because samples are "FLI-1" (dash), not "FLI_1".
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

# Infer sample IDs from merged fastq names
# results/fastq/<sample>_1.merged.fastq.gz and <sample>_2.merged.fastq.gz
infer_samples_from_merged_fastq <- function(results_dir) {
  fastq_dir <- path(results_dir, "fastq")
  if (!dir_exists(fastq_dir)) return(character(0))

  files <- dir_ls(fastq_dir, type = "file", glob = "*.merged.fastq.gz")
  if (length(files) == 0) return(character(0))

  bases <- path_file(files)
  # extract sample from <sample>_[12].merged.fastq.gz
  samp <- str_match(bases, "^(.*)_[12]\\.merged\\.fastq\\.gz$")[, 2]
  samp <- samp[!is.na(samp)]
  unique(samp)
}

# Compute run-level counts & integrity using samplesheet as truth
compute_run_metrics <- function(pipeline_dir, results_dir) {
  ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
  ss <- read_samplesheet(ss_path)

  ss_samples <- character(0)
  if (!is.null(ss) && "sample_id" %in% names(ss)) {
    ss_samples <- unique(na.omit(as.character(ss$sample_id)))
  }

  # matching influenza samples based on samplesheet
  match_samples <- sum(str_detect(ss_samples, SAMPLE_ID_REGEX), na.rm = TRUE)

  # outputs
  nx <- extract_nextclade(read_nextclade(results_dir))
  sub <- extract_HN(read_subtyping(results_dir))
  mfq <- infer_samples_from_merged_fastq(results_dir)

  nx_samples  <- if (!is.null(nx)) unique(na.omit(as.character(nx$sample_id))) else character(0)
  sub_samples <- if (!is.null(sub)) unique(na.omit(as.character(sub$sample_id))) else character(0)

  # Quality passed samples among matching samples (default: qc_status good/pass/passed)
  passed_samples <- 0L
  if (!is.null(nx) && "qc_status" %in% names(nx) && length(ss_samples) > 0) {
    nx2 <- nx %>%
      mutate(
        qc_status_l = tolower(as.character(qc_status)),
        is_pass = qc_status_l %in% c("good", "pass", "passed")
      )
    # only those in samplesheet AND matching regex
    ss_match <- ss_samples[str_detect(ss_samples, SAMPLE_ID_REGEX)]
    passed_samples <- sum(nx2$sample_id %in% ss_match & nx2$is_pass, na.rm = TRUE)
  } else {
    # fallback: if no nextclade qc_status, we can't define "passed" well
    passed_samples <- 0L
  }

  # Integrity checks:
  # Compare only for matching samples (reduces noise if samplesheet also contains non-326 runs)
  ss_match <- ss_samples[str_detect(ss_samples, SAMPLE_ID_REGEX)]
  ss_set <- unique(ss_match)

  # If no samplesheet or empty: treat as "no_samplesheet"
  has_ss <- !is.na(ss_path) && file_exists(ss_path) && length(ss_samples) > 0

  compare_sets <- function(expected, observed) {
    list(
      missing = sort(setdiff(expected, observed)),
      extra   = sort(setdiff(observed, expected))
    )
  }

  # Normalize observed sets (already normalized in extract_*)
  nx_set  <- sort(intersect(nx_samples, ss_samples))  # keep only those possibly relevant
  sub_set <- sort(intersect(sub_samples, ss_samples))
  mfq_set <- sort(intersect(mfq, ss_samples))

  # For integrity we want: expected (ss_set) vs observed (nx/sub/mfq)
  nx_cmp  <- compare_sets(ss_set, intersect(nx_samples, ss_set))
  sub_cmp <- compare_sets(ss_set, intersect(sub_samples, ss_set))
  mfq_cmp <- compare_sets(ss_set, intersect(mfq, ss_set))

  # If outputs have samples not in ss_set, that's also important → "extra"
  # We'll compute extras against expected too:
  nx_extra  <- setdiff(nx_samples, ss_set)
  sub_extra <- setdiff(sub_samples, ss_set)
  mfq_extra <- setdiff(mfq, ss_set)

  # Issue flag: only meaningful if we have a samplesheet and at least one expected sample
  expected_n <- length(ss_set)
  issue <- FALSE
  issue_msg <- ""

  if (!has_ss) {
    issue <- TRUE
    issue_msg <- "No/empty samplesheet"
  } else if (expected_n == 0) {
    # has samplesheet, but no matching samples -> not an error per se
    issue <- FALSE
    issue_msg <- ""
  } else {
    # Flag if any expected samples are missing from any of the 3 sources (when source exists),
    # OR if there are "extra" samples in outputs.
    # (We only judge "missing" if the corresponding file exists / folder exists.)
    has_nx  <- file_exists(path(results_dir, "nextclade", "nextclade.tsv"))
    has_sub <- file_exists(path(results_dir, "subtyping_report", "subtype_results.csv"))
    has_mfq <- dir_exists(path(results_dir, "fastq"))

    missing_total <- 0L
    extra_total <- 0L

    if (has_nx)  missing_total <- missing_total + length(nx_cmp$missing)
    if (has_sub) missing_total <- missing_total + length(sub_cmp$missing)
    if (has_mfq) missing_total <- missing_total + length(mfq_cmp$missing)

    if (has_nx)  extra_total <- extra_total + length(nx_extra)
    if (has_sub) extra_total <- extra_total + length(sub_extra)
    if (has_mfq) extra_total <- extra_total + length(mfq_extra)

    if (missing_total > 0 || extra_total > 0) {
      issue <- TRUE
      issue_msg <- paste0("Mismatch (missing=", missing_total, ", extra=", extra_total, ")")
    }
  }

  list(
    samplesheet_path = ss_path,
    ss_samples = ss_samples,
    ss_match_samples = ss_set,
    match_n = match_samples,
    pass_n = passed_samples,
    issue = issue,
    issue_msg = issue_msg
  )
}

# Create run registry dataframe
list_runs <- function() {
  if (!dir_exists(DATA_ROOT)) return(tibble())

  run_dirs <- dir_ls(DATA_ROOT, type = "directory", recurse = FALSE)
  run_dirs <- run_dirs[!basename(run_dirs) %in% c("logs", "runs", "shiny-dev")]

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

      # MultiQC
      multiqc_path <- path(results_dir, "MultiQC", "multiqc_report.html")
      mq_url <- if (file_exists(multiqc_path)) path_to_url(multiqc_path) else NULL

      # Status
      has_nextclade <- file_exists(path(results_dir, "nextclade", "nextclade.tsv"))
      has_subtyping <- file_exists(path(results_dir, "subtyping_report", "subtype_results.csv"))
      status <- if (has_nextclade && has_subtyping) "done" else "partial"

      # Metrics from samplesheet + integrity checks
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
            label = "Include runs without any quality-passed samples (Nextclade QC)",
            value = FALSE
          ),
          div(class = "small-note", "Pass definition: nextclade qc.overallStatus == 'good' (or pass/passed).")
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
          issue ~ "\U0001F6D1",             # 🛑
          status == "done" ~ "\U0001F7E2",  # 🟢
          status == "partial" ~ "\U0001F7E1",# 🟡
          status == "no_results" ~ "\U000026AA", # ⚪
          TRUE ~ "\U0001F534"              # 🔴
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
        "Integrity",
        backgroundColor = styleEqual(c("OK"), c("#dff0d8")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Integrity",
        backgroundColor = styleEqual(c("OK"), c("#dff0d8")),
        fontWeight = "bold"
      ) %>%
      # make non-OK integrity stand out
      formatStyle(
        "Integrity",
        backgroundColor = styleEqual("OK", "#dff0d8"),
        color = styleEqual("OK", "black")
      ) %>%
      formatStyle(
        "Integrity",
        backgroundColor = styleInterval(0, c("#f2dede", "#dff0d8")) # doesn't apply well to strings, harmless
      ) %>%
      # Status coloring
      formatStyle(
        "Status",
        backgroundColor = styleEqual(
          c("done", "partial", "no_results"),
          c("#dff0d8", "#fcf8e3", "#eeeeee")
        ),
        fontWeight = "bold"
      ) %>%
      # Matching & Passed color ramps
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
      # If Integrity != OK -> red background
      formatStyle(
        "Integrity",
        backgroundColor = JS(
          "function(value, row, index) { return (value === 'OK') ? '#dff0d8' : '#f2dede'; }"
        ),
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
        " | ", tags$b("Matching samples: "), sr$match_samples,
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

    # Authoritative sample list from samplesheet
    ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
    ss <- read_samplesheet(ss_path)

    if (is.null(ss) || !"sample_id" %in% names(ss)) {
      return(datatable(
        data.frame(Message = "No readable samplesheet found for this run."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    ss_samples <- unique(na.omit(as.character(ss$sample_id)))
    ss_match <- ss_samples[str_detect(ss_samples, SAMPLE_ID_REGEX)]
    if (length(ss_match) == 0) {
      return(datatable(
        data.frame(Message = "Samplesheet found, but no samples match the influenza sample regex for this dashboard."),
        options = list(dom = "t"),
        rownames = FALSE
      ))
    }

    # Outputs
    sub <- extract_HN(read_subtyping(results_dir))
    nx  <- extract_nextclade(read_nextclade(results_dir))

    # Start with samplesheet rows only
    base <- tibble(sample_id = ss_match)

    df <- base %>%
      left_join(sub, by = "sample_id") %>%
      left_join(nx, by = "sample_id") %>%
      mutate(
        has_subtyping = !is.na(H) | !is.na(N) | !is.na(subtype) | !is.na(influenza_type),
        has_nextclade = !is.na(clade) | !is.na(subclade) | !is.na(qc_status)
      )

    # Choose columns to show
    show_cols <- intersect(
      names(df),
      c("sample_id", "influenza_type", "H", "N", "subtype", "clade", "subclade", "qc_status", "qc_score", "has_subtyping", "has_nextclade")
    )
    disp <- df %>% select(all_of(show_cols))

    dt <- datatable(
      disp,
      options = list(pageLength = 25, autoWidth = TRUE),
      rownames = FALSE
    )

    # QC coloring
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

    # Highlight missing outputs
    if ("has_subtyping" %in% names(disp)) {
      dt <- dt %>% formatStyle(
        "has_subtyping",
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#dff0d8", "#f2dede")),
        fontWeight = "bold"
      )
    }
    if ("has_nextclade" %in% names(disp)) {
      dt <- dt %>% formatStyle(
        "has_nextclade",
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#dff0d8", "#f2dede")),
        fontWeight = "bold"
      )
    }

    # Highlight unusual H types (example: H5/H7)
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
