# app.R
# NF_FLU Influenza Dashboard
#
# Philosophy:
# - Samplesheet is authoritative for "what samples exist".
# - Missing outputs are expected (e.g., no influenza / failed analysis) and not an error.
# - "Passed" primarily derived from pass_read_count_samples_mqc.tsv.
# - App should never crash on a single weird run -> tryCatch + validate.

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(DT)
library(purrr)
library(fs)

# ---------------- CONFIG ----------------

# Testing root (as requested)
# DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/backup_NF_FLU_backup")
# Production later:
DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/NF_FLU")

BASE_URL <- Sys.getenv("NF_FLU_BASE_URL", unset = "")
if (BASE_URL == "") BASE_URL <- NULL

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

# Resolve results/ dir for a run: supports
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

read_samplesheet <- function(samplesheet_path) {
  if (is.na(samplesheet_path) || !file_exists(samplesheet_path)) return(NULL)
  ss <- suppressWarnings(read_csv(samplesheet_path, show_col_types = FALSE))

  # Your samplesheet has: sample,fastq_1,fastq_2
  if (!("sample" %in% names(ss))) return(NULL)

  ss %>%
    rename(sample_id = sample) %>%
    mutate(sample_id = as.character(sample_id)) %>%
    distinct(sample_id, .keep_all = TRUE)
}

read_nextclade <- function(results_dir) {
  f <- path(results_dir, "nextclade", "nextclade.tsv")
  if (!file_exists(f)) return(NULL)
  suppressWarnings(read_tsv(f, show_col_types = FALSE))
}

read_subtyping <- function(results_dir) {
  f <- path(results_dir, "subtyping_report", "subtype_results.csv")
  if (!file_exists(f)) return(NULL)
  suppressWarnings(read_csv(f, show_col_types = FALSE))
}

standardize_sample_id <- function(df) {
  if (is.null(df)) return(df)
  nms <- names(df)
  cand <- intersect(nms, c("sample", "sample_id", "seqName", "name", "sequence_name", "SequenceName"))
  if (length(cand) >= 1) df <- df %>% rename(sample_id = all_of(cand[[1]]))
  if (!("sample_id" %in% names(df))) df$sample_id <- NA_character_
  df %>% mutate(sample_id = as.character(sample_id))
}

# Safe normalization: strip trailing _1/_2 ONLY (not -1 etc)
normalize_id <- function(x) {
  x <- as.character(x)
  str_replace(x, "_[12]$", "")
}

extract_nextclade <- function(nx_df) {
  if (is.null(nx_df)) return(NULL)
  nx_df <- standardize_sample_id(nx_df) %>% mutate(sample_id = normalize_id(sample_id))
  nms <- names(nx_df)

  out <- nx_df
  if ("clade" %in% nms) {
    # ok
  } else {
    clade_col <- intersect(nms, c("Clade"))
    if (length(clade_col) >= 1) out <- out %>% rename(clade = all_of(clade_col[[1]]))
  }

  subcl_col <- intersect(nms, c("subclade", "Subclade"))
  if (length(subcl_col) >= 1 && !("subclade" %in% names(out))) out <- out %>% rename(subclade = all_of(subcl_col[[1]]))

  qc_status_col <- intersect(nms, c("qc.overallStatus", "qc_overallStatus", "overallStatus"))
  if (length(qc_status_col) >= 1) out <- out %>% rename(qc_status = all_of(qc_status_col[[1]]))

  qc_score_col <- intersect(nms, c("qc.overallScore", "qc_overallScore", "overallScore"))
  if (length(qc_score_col) >= 1) out <- out %>% rename(qc_score = all_of(qc_score_col[[1]]))

  keep_cols <- intersect(names(out), c("sample_id", "clade", "subclade", "qc_status", "qc_score"))
  if (length(keep_cols) >= 2) out <- out %>% select(all_of(keep_cols))
  out
}

extract_subtyping <- function(sub_df) {
  if (is.null(sub_df)) return(NULL)
  sub_df <- standardize_sample_id(sub_df) %>% mutate(sample_id = normalize_id(sample_id))
  nms <- names(sub_df)

  out <- sub_df

  # common fields
  type_col <- intersect(nms, c("influenza_type", "type", "virus_type", "InfluenzaType"))
  sub_col  <- intersect(nms, c("subtype", "subtype_prediction", "prediction", "Subtype"))
  if (length(type_col) >= 1 && !("influenza_type" %in% names(out))) out <- out %>% rename(influenza_type = all_of(type_col[[1]]))
  if (length(sub_col)  >= 1 && !("subtype" %in% names(out))) out <- out %>% rename(subtype = all_of(sub_col[[1]]))

  h_col <- intersect(nms, c("H", "H_subtype", "HA", "ha_subtype", "H_type"))
  n_col <- intersect(nms, c("N", "N_subtype", "NA", "na_subtype", "N_type"))
  if (length(h_col) >= 1 && !("H" %in% names(out))) out <- out %>% rename(H = all_of(h_col[[1]]))
  if (length(n_col) >= 1 && !("N" %in% names(out))) out <- out %>% rename(N = all_of(n_col[[1]]))

  # Parse subtype if needed
  if ("subtype" %in% names(out)) {
    if (!("H" %in% names(out))) out$H <- str_extract(as.character(out$subtype), "H\\d+")
    if (!("N" %in% names(out))) out$N <- str_extract(as.character(out$subtype), "N\\d+")
    if (!("influenza_type" %in% names(out))) out$influenza_type <- str_extract(as.character(out$subtype), "^[AB]")
  }

  keep_cols <- intersect(names(out), c("sample_id", "influenza_type", "H", "N", "subtype"))
  if (length(keep_cols) >= 2) out <- out %>% select(all_of(keep_cols))
  out
}

read_pass_readcount <- function(results_dir) {
  f <- path(results_dir, "read_count", "pass_read_count_samples_mqc.tsv")
  if (!file_exists(f)) return(NULL)

  rc <- suppressWarnings(read_tsv(f, show_col_types = FALSE))
  if (ncol(rc) < 2) {
    rc <- suppressWarnings(read_delim(f, delim = "\\s+", show_col_types = FALSE, trim_ws = TRUE))
  }
  if (ncol(rc) < 2) return(NULL)

  # Try to find columns
  nms <- names(rc)
  sample_col <- intersect(nms, c("Sample", "sample", "SAMPLE"))
  read_col <- intersect(nms, c("Read count", "Read_count", "Read.count", "read_count", "ReadCount"))

  sample_col <- if (length(sample_col) >= 1) sample_col[[1]] else nms[[1]]
  read_col   <- if (length(read_col) >= 1) read_col[[1]] else nms[[2]]

  rc %>%
    transmute(
      sample_id = as.character(.data[[sample_col]]),
      read_count = suppressWarnings(as.numeric(.data[[read_col]]))
    ) %>%
    filter(!is.na(sample_id) & sample_id != "") %>%
    distinct(sample_id, .keep_all = TRUE)
}

# Run-level metrics (NO regex guesswork)
compute_run_metrics <- function(pipeline_dir, results_dir) {
  ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
  ss <- read_samplesheet(ss_path)

  # samplesheet missing is the only true "integrity" issue
  has_samplesheet <- !is.null(ss) && nrow(ss) > 0

  sample_n <- if (has_samplesheet) nrow(ss) else 0L

  rc <- read_pass_readcount(results_dir)
  passed_n <- 0L
  if (!is.null(rc) && has_samplesheet) {
    passed_n <- rc %>%
      filter(sample_id %in% ss$sample_id, !is.na(read_count), read_count > 0) %>%
      summarise(n = n()) %>% pull(n)
    passed_n <- as.integer(passed_n %||% 0L)
  }

  # missing outputs are informative, not errors
  nx <- extract_nextclade(read_nextclade(results_dir))
  sub <- extract_subtyping(read_subtyping(results_dir))

  missing_nextclade_n <- if (has_samplesheet) {
    if (is.null(nx)) sample_n else sum(!(ss$sample_id %in% nx$sample_id))
  } else NA_integer_

  missing_subtyping_n <- if (has_samplesheet) {
    if (is.null(sub)) sample_n else sum(!(ss$sample_id %in% sub$sample_id))
  } else NA_integer_

  list(
    samplesheet_path = ss_path,
    sample_n = sample_n,
    passed_n = passed_n,
    has_samplesheet = has_samplesheet,
    missing_nextclade_n = missing_nextclade_n,
    missing_subtyping_n = missing_subtyping_n
  )
}

list_runs <- function() {
  if (!dir_exists(DATA_ROOT)) return(tibble())
  run_dirs <- dir_ls(DATA_ROOT, type = "directory", recurse = FALSE)

  rows <- map(run_dirs, function(rd) {
    resolved <- resolve_results_dir(rd)

    if (is.null(resolved)) {
      return(tibble(
        run = basename(rd),
        status = "no_results",
        run_dir = rd,
        pipeline_dir = NA_character_,
        results_dir = NA_character_,
        updated = file_info(rd)$modification_time,
        sample_n = 0L,
        passed_n = 0L,
        samplesheet_path = NA_character_,
        missing_nextclade_n = NA_integer_,
        missing_subtyping_n = NA_integer_,
        has_samplesheet = FALSE,
        multiqc_path = NA_character_,
        multiqc_url = NA_character_
      ))
    }

    results_dir <- resolved$results_dir
    pipeline_dir <- resolved$pipeline_dir

    has_nextclade <- file_exists(path(results_dir, "nextclade", "nextclade.tsv"))
    has_subtyping <- file_exists(path(results_dir, "subtyping_report", "subtype_results.csv"))
    status <- if (has_nextclade && has_subtyping) "done" else "partial"

    multiqc_path <- path(results_dir, "MultiQC", "multiqc_report.html")
    mq_url <- if (file_exists(multiqc_path)) path_to_url(multiqc_path) else NULL

    met <- compute_run_metrics(pipeline_dir, results_dir)

    tibble(
      run = basename(rd),
      status = status,
      run_dir = rd,
      pipeline_dir = pipeline_dir,
      results_dir = results_dir,
      updated = resolved$mtime,
      sample_n = met$sample_n,
      passed_n = met$passed_n,
      samplesheet_path = met$samplesheet_path,
      missing_nextclade_n = met$missing_nextclade_n,
      missing_subtyping_n = met$missing_subtyping_n,
      has_samplesheet = met$has_samplesheet,
      multiqc_path = if (file_exists(multiqc_path)) multiqc_path else NA_character_,
      multiqc_url = mq_url %||% NA_character_
    )
  })

  bind_rows(rows) %>% arrange(desc(updated))
}

# ---------------- UI ----------------

ui <- fluidPage(
  titlePanel("NF_FLU Influenza Dashboard"),
  fluidRow(
    column(
      12,
      h3("Runs"),
      fluidRow(
        column(
          6,
          checkboxInput(
            "include_empty_runs",
            label = "Include runs without any samples (missing/empty samplesheet)",
            value = FALSE
          )
        ),
        column(
          6,
          checkboxInput(
            "include_no_pass_runs",
            label = "Include runs without any quality-passed samples",
            value = FALSE
          )
        )
      ),
      DTOutput("runs_tbl"),
      hr(),
      uiOutput("run_header"),
      DTOutput("samples_tbl"),
      hr(),
      tags$div(style = "color:#666; font-size:12px;",
               "App author: Emanuel Heitlinger")
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
      runs <- runs %>% filter(has_samplesheet, sample_n > 0)
    }
    if (!isTRUE(input$include_no_pass_runs)) {
      runs <- runs %>% filter(passed_n > 0)
    }
    runs
  })

  output$runs_tbl <- renderDT({
    runs <- filtered_runs()
    validate(need(nrow(runs) > 0, "No runs match the current filters."))

    disp <- runs %>%
      transmute(
        ` ` = case_when(
          !has_samplesheet ~ "\U0001F6D1",        # 🛑 (only real error)
          status == "done" ~ "\U0001F7E2",        # 🟢
          status == "partial" ~ "\U0001F7E1",     # 🟡
          status == "no_results" ~ "\U000026AA",  # ⚪
          TRUE ~ "\U0001F534"                     # 🔴
        ),
        Run = run,
        Status = status,
        Samples = sample_n,
        Passed = passed_n,
        Missing_nextclade = missing_nextclade_n,
        Missing_subtyping = missing_subtyping_n,
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
        "Passed",
        backgroundColor = styleInterval(c(0, 5), c("#f2dede", "#fcf8e3", "#dff0d8")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Samples",
        backgroundColor = styleInterval(c(0, 5), c("#f2dede", "#fcf8e3", "#dff0d8")),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Missing_nextclade",
        backgroundColor = styleInterval(c(0, 1, 5), c("#dff0d8", "#fcf8e3", "#fcf8e3", "#eeeeee"))
      ) %>%
      formatStyle(
        "Missing_subtyping",
        backgroundColor = styleInterval(c(0, 1, 5), c("#dff0d8", "#fcf8e3", "#fcf8e3", "#eeeeee"))
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

    warn <- NULL
    if (!isTRUE(sr$has_samplesheet)) {
      warn <- tags$p(tags$span(style = "color:#a94442; font-weight:700;",
                               "ERROR: Missing/empty samplesheet (cannot build sample table)."))
    }

    tags$div(
      h3(paste0("Run: ", sr$run)),
      tags$p(
        tags$b("Status: "), sr$status,
        " | ", tags$b("Samples: "), sr$sample_n,
        " | ", tags$b("Passed: "), sr$passed_n,
        " | ", tags$b("Missing nextclade: "), sr$missing_nextclade_n %||% "—",
        " | ", tags$b("Missing subtyping: "), sr$missing_subtyping_n %||% "—"
      ),
      warn,
      tags$p(mq),
      tags$p(ss),
      tags$p(tags$small(tags$code(sr$results_dir)))
    )
  })

  output$samples_tbl <- renderDT({
    sr <- selected_run()
    validate(need(!is.null(sr), "Select a run above."))

    # Hardening: never let a bad run crash the UI
    tryCatch({

      results_dir <- sr$results_dir[[1]]
      pipeline_dir <- sr$pipeline_dir[[1]]

      validate(need(!is.na(results_dir) && dir_exists(results_dir), "Run has no readable results directory."))

      ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
      ss <- read_samplesheet(ss_path)
      validate(need(!is.null(ss) && nrow(ss) > 0, "No readable samplesheet for this run."))

      # outputs (optional)
      nx  <- extract_nextclade(read_nextclade(results_dir))
      sub <- extract_subtyping(read_subtyping(results_dir))
      rc  <- read_pass_readcount(results_dir)

      df <- ss %>%
        select(sample_id, everything()) %>%
        left_join(rc, by = "sample_id") %>%
        left_join(sub, by = "sample_id") %>%
        left_join(nx, by = "sample_id") %>%
        mutate(
          has_reads = !is.na(read_count),
          has_subtyping = !is.na(H) | !is.na(N) | !is.na(subtype) | !is.na(influenza_type),
          has_nextclade = !is.na(clade) | !is.na(subclade) | !is.na(qc_status),
          passed_reads = !is.na(read_count) & read_count > 0
        )

      # Keep the run-table readable
      # (You can add fastq_1/fastq_2 later as hidden columns if you want)
      show_cols <- intersect(
        names(df),
        c("sample_id", "read_count", "passed_reads",
          "influenza_type", "H", "N", "subtype",
          "clade", "subclade", "qc_status", "qc_score",
          "has_reads", "has_subtyping", "has_nextclade")
      )
      disp <- df %>% select(all_of(show_cols))

      dt <- datatable(
        disp,
        options = list(pageLength = 25, autoWidth = TRUE),
        rownames = FALSE
      )

      # Highlight missing outputs (informative, not "error")
      for (col in c("has_reads", "has_subtyping", "has_nextclade")) {
        if (col %in% names(disp)) {
          dt <- dt %>% formatStyle(
            col,
            backgroundColor = styleEqual(c(TRUE, FALSE), c("#dff0d8", "#fcf8e3")),
            fontWeight = "bold"
          )
        }
      }

      if ("passed_reads" %in% names(disp)) {
        dt <- dt %>% formatStyle(
          "passed_reads",
          backgroundColor = styleEqual(c(TRUE, FALSE), c("#dff0d8", "#f2dede")),
          fontWeight = "bold"
        )
      }

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

      if ("H" %in% names(disp)) {
        dt <- dt %>% formatStyle(
          "H",
          backgroundColor = styleEqual(c("H5", "H7"), c("#f2dede", "#f2dede")),
          fontWeight = styleEqual(c("H5", "H7"), c("bold", "bold"))
        )
      }

      dt

    }, error = function(e) {
      message("[nf-flu shiny] samples_tbl error: ", conditionMessage(e))
      datatable(
        data.frame(
          Message = c(
            "An error has occurred. Check your logs or contact the app author for clarification.",
            paste0("Details: ", conditionMessage(e)),
            "App author: Emanuel Heitlinger"
          )
        ),
        options = list(dom = "t"),
        rownames = FALSE
      )
    })
  })
}

shinyApp(ui, server)
