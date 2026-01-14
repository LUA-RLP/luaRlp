# app.R (FIXED NULL-COALESCE + VERY VERBOSE)
# App author: Emanuel Heitlinger

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(DT)
library(purrr)
library(fs)
library(tibble)

# ---------------- CONFIG ----------------

DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/backup_NF_FLU_backup")
# Production later:
# DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/NF_FLU")

BASE_URL <- Sys.getenv("NF_FLU_BASE_URL", unset = "")
if (BASE_URL == "") BASE_URL <- NULL

REFRESH_MS <- as.integer(Sys.getenv("NF_FLU_REFRESH_MS", unset = "120000")) # 2 min

# ---------------- helpers ----------------

# NULL-coalescer: SAFE FOR TIBBLES (only checks is.null)
`%||%` <- function(a, b) if (is.null(a)) b else a

# scalar-string coalescer: for character paths/urls
`%||str%` <- function(a, b) {
  if (is.null(a) || length(a) < 1) return(b)
  x <- as.character(a[[1]])
  if (is.na(x) || x == "") b else x
}

rtrim_slash <- function(x) sub("/+$", "", x)

scalar_chr <- function(x) {
  if (is.null(x) || length(x) < 1) return(NA_character_)
  as.character(x[[1]])
}
scalar_lgl <- function(x) {
  if (is.null(x) || length(x) < 1) return(FALSE)
  isTRUE(x[[1]])
}

dbg <- function(...) message(sprintf("[nf-flu shiny] %s", paste0(..., collapse = "")))

path_to_url <- function(fs_path) {
  if (is.null(BASE_URL)) return(NULL)
  rel <- path_rel(fs_path, start = DATA_ROOT)
  rel <- gsub("\\\\", "/", rel)
  paste0(rtrim_slash(BASE_URL), "/", rel)
}

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
  if (!("sample" %in% names(ss))) return(NULL)

  ss %>%
    rename(sample_id = sample) %>%
    mutate(sample_id = as.character(sample_id)) %>%
    distinct(sample_id, .keep_all = TRUE)
}

normalize_id <- function(x) {
  x <- as.character(x)
  str_replace(x, "_[12]$", "")
}

standardize_sample_id <- function(df) {
  if (is.null(df)) return(df)
  nms <- names(df)
  cand <- intersect(nms, c("sample", "sample_id", "seqName", "name", "sequence_name", "SequenceName"))
  if (length(cand) >= 1) df <- df %>% rename(sample_id = all_of(cand[[1]]))
  if (!("sample_id" %in% names(df))) df$sample_id <- NA_character_
  df %>% mutate(sample_id = as.character(sample_id))
}

# Empty tables so joins never break
empty_rc  <- function() tibble(sample_id = character(), read_count = numeric())
empty_nx  <- function() tibble(sample_id = character(), clade = character(), subclade = character(),
                               qc_status = character(), qc_score = numeric())
empty_sub <- function() tibble(sample_id = character(), influenza_type = character(),
                               H = character(), N = character(), subtype = character())

read_nextclade_raw <- function(results_dir) {
  f <- path(results_dir, "nextclade", "nextclade.tsv")
  if (!file_exists(f)) return(NULL)
  suppressWarnings(read_tsv(f, show_col_types = FALSE))
}

read_subtyping_raw <- function(results_dir) {
  f <- path(results_dir, "subtyping_report", "subtype_results.csv")
  if (!file_exists(f)) return(NULL)
  suppressWarnings(read_csv(f, show_col_types = FALSE))
}

read_pass_readcount_raw <- function(results_dir) {
  f <- path(results_dir, "read_count", "pass_read_count_samples_mqc.tsv")
  if (!file_exists(f)) return(NULL)

  rc <- suppressWarnings(read_tsv(f, show_col_types = FALSE))
  if (is.null(rc) || ncol(rc) < 2) {
    rc <- suppressWarnings(read_delim(f, delim = "\\s+", show_col_types = FALSE, trim_ws = TRUE))
  }
  if (is.null(rc) || ncol(rc) < 2) return(NULL)
  rc
}

extract_readcount <- function(rc_raw) {
  if (is.null(rc_raw) || ncol(rc_raw) < 2) return(NULL)

  nms <- names(rc_raw)
  sample_col <- intersect(nms, c("Sample", "sample", "SAMPLE"))
  read_col <- intersect(nms, c("Read count", "Read_count", "Read.count", "read_count", "ReadCount"))

  sample_col <- if (length(sample_col) >= 1) sample_col[[1]] else nms[[1]]
  read_col   <- if (length(read_col) >= 1) read_col[[1]] else nms[[2]]

  rc_raw %>%
    transmute(
      sample_id = normalize_id(as.character(.data[[sample_col]])),
      read_count = suppressWarnings(as.numeric(.data[[read_col]]))
    ) %>%
    filter(!is.na(sample_id) & sample_id != "") %>%
    distinct(sample_id, .keep_all = TRUE)
}

extract_nextclade <- function(nx_df) {
  if (is.null(nx_df)) return(NULL)
  nx_df <- standardize_sample_id(nx_df) %>% mutate(sample_id = normalize_id(sample_id))
  out <- nx_df

  if (!("clade" %in% names(out))) {
    cc <- intersect(names(out), c("Clade"))
    if (length(cc) >= 1) out <- out %>% rename(clade = all_of(cc[[1]]))
  }
  if (!("subclade" %in% names(out))) {
    sc <- intersect(names(out), c("Subclade"))
    if (length(sc) >= 1) out <- out %>% rename(subclade = all_of(sc[[1]]))
  }

  qc_status_col <- intersect(names(out), c("qc.overallStatus", "qc_overallStatus", "overallStatus"))
  if (length(qc_status_col) >= 1 && !("qc_status" %in% names(out))) out <- out %>% rename(qc_status = all_of(qc_status_col[[1]]))

  qc_score_col <- intersect(names(out), c("qc.overallScore", "qc_overallScore", "overallScore"))
  if (length(qc_score_col) >= 1 && !("qc_score" %in% names(out))) out <- out %>% rename(qc_score = all_of(qc_score_col[[1]]))

  keep_cols <- intersect(names(out), c("sample_id", "clade", "subclade", "qc_status", "qc_score"))
  if (length(keep_cols) >= 2) out <- out %>% select(all_of(keep_cols))
  out
}

extract_subtyping <- function(sub_df) {
  if (is.null(sub_df)) return(NULL)
  sub_df <- standardize_sample_id(sub_df) %>% mutate(sample_id = normalize_id(sample_id))
  out <- sub_df
  nms <- names(out)

  type_col <- intersect(nms, c("influenza_type", "type", "virus_type", "InfluenzaType"))
  sub_col  <- intersect(nms, c("subtype", "subtype_prediction", "prediction", "Subtype"))
  if (length(type_col) >= 1 && !("influenza_type" %in% names(out))) out <- out %>% rename(influenza_type = all_of(type_col[[1]]))
  if (length(sub_col)  >= 1 && !("subtype" %in% names(out))) out <- out %>% rename(subtype = all_of(sub_col[[1]]))

  h_col <- intersect(nms, c("H", "H_subtype", "HA", "ha_subtype", "H_type"))
  n_col <- intersect(nms, c("N", "N_subtype", "NA", "na_subtype", "N_type"))
  if (length(h_col) >= 1 && !("H" %in% names(out))) out <- out %>% rename(H = all_of(h_col[[1]]))
  if (length(n_col) >= 1 && !("N" %in% names(out))) out <- out %>% rename(N = all_of(n_col[[1]]))

  if ("subtype" %in% names(out)) {
    if (!("H" %in% names(out))) out$H <- str_extract(as.character(out$subtype), "H\\d+")
    if (!("N" %in% names(out))) out$N <- str_extract(as.character(out$subtype), "N\\d+")
    if (!("influenza_type" %in% names(out))) out$influenza_type <- str_extract(as.character(out$subtype), "^[AB]")
  }

  keep_cols <- intersect(names(out), c("sample_id", "influenza_type", "H", "N", "subtype"))
  if (length(keep_cols) >= 2) out <- out %>% select(all_of(keep_cols))
  out
}

# FAST run listing
list_runs_fast <- function() {
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
        has_samplesheet = FALSE,
        samplesheet_path = NA_character_,
        multiqc_path = NA_character_,
        multiqc_url = NA_character_
      ))
    }

    results_dir <- resolved$results_dir
    pipeline_dir <- resolved$pipeline_dir

    has_nextclade <- file_exists(path(results_dir, "nextclade", "nextclade.tsv"))
    has_subtyping <- file_exists(path(results_dir, "subtyping_report", "subtype_results.csv"))
    status <- if (has_nextclade && has_subtyping) "done" else "partial"

    ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
    has_ss <- (!is.na(ss_path) && file_exists(ss_path))

    multiqc_path <- path(results_dir, "MultiQC", "multiqc_report.html")
    mq_url <- if (file_exists(multiqc_path)) path_to_url(multiqc_path) else NULL

    tibble(
      run = basename(rd),
      status = status,
      run_dir = rd,
      pipeline_dir = pipeline_dir,
      results_dir = results_dir,
      updated = resolved$mtime,
      has_samplesheet = has_ss,
      samplesheet_path = ss_path,
      multiqc_path = if (file_exists(multiqc_path)) multiqc_path else NA_character_,
      multiqc_url = mq_url
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
        column(6, checkboxInput("include_empty_runs",
                                "Include runs without any samples (missing/empty samplesheet)", FALSE)),
        column(6, checkboxInput("include_no_pass_runs",
                                "Include runs without any quality-passed samples (applies after selecting a run)", FALSE))
      ),
      DTOutput("runs_tbl"),
      hr(),
      uiOutput("run_header"),
      DTOutput("samples_tbl"),
      hr(),
      tags$div(style = "color:#666; font-size:12px;", "App author: Emanuel Heitlinger")
    )
  )
)

# ---------------- SERVER ----------------

server <- function(input, output, session) {

  runs_r <- reactiveVal(tibble())

  refresh_runs <- function() {
    out <- tryCatch(list_runs_fast(), error = function(e) {
      dbg("list_runs_fast error: ", conditionMessage(e))
      tibble()
    })
    runs_r(out)
  }

  observeEvent(TRUE, { refresh_runs() }, once = TRUE)

  observe({
    invalidateLater(REFRESH_MS, session)
    refresh_runs()
  })

  filtered_runs <- reactive({
    runs <- runs_r()
    if (nrow(runs) == 0) return(runs)
    if (!isTRUE(input$include_empty_runs)) runs <- runs %>% filter(has_samplesheet)
    runs
  })

  output$runs_tbl <- renderDT({
    runs <- filtered_runs()
    validate(need(nrow(runs) > 0, "No runs found (or none match filters)."))

    disp <- runs %>%
      transmute(
        ` ` = case_when(
          !has_samplesheet ~ "\U0001F6D1",        # 🛑
          status == "done" ~ "\U0001F7E2",        # 🟢
          status == "partial" ~ "\U0001F7E1",     # 🟡
          status == "no_results" ~ "\U000026AA",  # ⚪
          TRUE ~ "\U0001F534"
        ),
        Run = run,
        Status = status,
        Updated = format(updated, "%Y-%m-%d %H:%M"),
        MultiQC = ifelse(
          !is.na(multiqc_url),
          paste0('<a href="', multiqc_url, '" target="_blank">MultiQC</a>'),
          ifelse(!is.na(multiqc_path), "Local", "—")
        )
      )

    datatable(disp, escape = FALSE, selection = "single",
              options = list(pageLength = 20, autoWidth = TRUE))
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

    run_name <- scalar_chr(sr$run)
    results_dir <- scalar_chr(sr$results_dir)
    pipeline_dir <- scalar_chr(sr$pipeline_dir)
    has_ss <- scalar_lgl(sr$has_samplesheet)

    dbg("Selected run: ", run_name)
    dbg("  results_dir=", results_dir)
    dbg("  pipeline_dir=", pipeline_dir)
    dbg("  has_samplesheet=", has_ss)

    ss_path <- find_samplesheet_path(pipeline_dir, results_dir)

    mq_url <- scalar_chr(sr$multiqc_url)
    mq_path <- scalar_chr(sr$multiqc_path)

    mq <- if (!is.na(mq_url) && mq_url != "") {
      tags$a(href = mq_url, target = "_blank", "Open MultiQC report")
    } else if (!is.na(mq_path) && mq_path != "") {
      tags$span("MultiQC (local path): ", tags$code(mq_path))
    } else {
      tags$span("MultiQC: —")
    }

    tags$div(
      h3(paste0("Run: ", run_name)),
      tags$p(tags$b("Status: "), scalar_chr(sr$status)),
      tags$p(mq),
      tags$p("Samplesheet: ", tags$code(ss_path %||str% "—")),
      if (!has_ss) tags$p(tags$span(style="color:#a94442; font-weight:700;",
                                    "ERROR: Missing/empty samplesheet (cannot build sample table).")) else NULL,
      tags$p(tags$small(tags$code(results_dir)))
    )
  })

  output$samples_tbl <- renderDT({
    sr <- selected_run()
    validate(need(!is.null(sr), "Select a run above."))

    tryCatch({
      run_name <- scalar_chr(sr$run)
      results_dir <- scalar_chr(sr$results_dir)
      pipeline_dir <- scalar_chr(sr$pipeline_dir)

      dbg("samples_tbl start for run ", run_name)

      validate(need(!is.na(results_dir) && nzchar(results_dir) && dir_exists(results_dir),
                    "Run has no readable results directory."))

      ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
      dbg("  samplesheet_path=", ss_path)

      ss <- read_samplesheet(ss_path)
      dbg("  samplesheet rows=", if (is.null(ss)) "NULL" else nrow(ss))
      validate(need(!is.null(ss) && nrow(ss) > 0, "No readable samplesheet for this run."))

      rc_raw <- read_pass_readcount_raw(results_dir)
      dbg("  readcount raw=", if (is.null(rc_raw)) "NULL" else paste0("ncol=", ncol(rc_raw), " nrow=", nrow(rc_raw)))
      rc <- extract_readcount(rc_raw)
      rc <- rc %||% empty_rc()
      dbg("  readcount parsed rows=", nrow(rc))

      sub_raw <- read_subtyping_raw(results_dir)
      dbg("  subtyping raw=", if (is.null(sub_raw)) "NULL" else paste0("ncol=", ncol(sub_raw), " nrow=", nrow(sub_raw)))
      sub <- extract_subtyping(sub_raw)
      sub <- sub %||% empty_sub()
      dbg("  subtyping parsed rows=", nrow(sub))

      nx_raw <- read_nextclade_raw(results_dir)
      dbg("  nextclade raw=", if (is.null(nx_raw)) "NULL" else paste0("ncol=", ncol(nx_raw), " nrow=", nrow(nx_raw)))
      nx <- extract_nextclade(nx_raw)
      nx <- nx %||% empty_nx()
      dbg("  nextclade parsed rows=", nrow(nx))

      df <- ss %>%
        select(sample_id) %>%
        left_join(rc,  by = "sample_id") %>%
        left_join(sub, by = "sample_id") %>%
        left_join(nx,  by = "sample_id") %>%
        mutate(
          has_reads = !is.na(read_count),
          has_subtyping = !is.na(H) | !is.na(N) | !is.na(subtype) | !is.na(influenza_type),
          has_nextclade = !is.na(clade) | !is.na(subclade) | !is.na(qc_status),
          passed_reads = !is.na(read_count) & read_count > 0
        )

      dbg("  joined df rows=", nrow(df), " cols=", ncol(df))

      if (!isTRUE(input$include_no_pass_runs) && "passed_reads" %in% names(df)) {
        validate(need(any(df$passed_reads, na.rm = TRUE),
                      "This run has zero passed samples (per read_count). Enable the checkbox to include it."))
      }

      show_cols <- intersect(
        names(df),
        c("sample_id", "read_count", "passed_reads",
          "influenza_type", "H", "N", "subtype",
          "clade", "subclade", "qc_status", "qc_score",
          "has_reads", "has_subtyping", "has_nextclade")
      )
      disp <- df %>% select(all_of(show_cols))

      dt <- datatable(disp, options = list(pageLength = 25, autoWidth = TRUE), rownames = FALSE)

      for (col in c("has_reads", "has_subtyping", "has_nextclade")) {
        if (col %in% names(disp)) {
          dt <- dt %>% formatStyle(col,
                                   backgroundColor = styleEqual(c(TRUE, FALSE), c("#dff0d8", "#fcf8e3")),
                                   fontWeight = "bold")
        }
      }

      if ("passed_reads" %in% names(disp)) {
        dt <- dt %>% formatStyle("passed_reads",
                                 backgroundColor = styleEqual(c(TRUE, FALSE), c("#dff0d8", "#f2dede")),
                                 fontWeight = "bold")
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

      dt

    }, error = function(e) {
      dbg("samples_tbl ERROR: ", conditionMessage(e))
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
