# app.R
# Minimal nf-flu Shiny dashboard: run overview -> per-run sample table
# Reads:
#   results/nextclade/nextclade.tsv
#   results/subtyping_report/subtype_results.csv
#   results/MultiQC/multiqc_report.html (link)
#
# Supports both layouts:
#   A) /data/NF/NF_FLU/<RUN>/<PIPELINE_RUN>/results/...
#   B) /data/NF/NF_FLU/<RUN>/results/...

library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(DT)
library(purrr)
library(fs)

# ---- CONFIG ----

# Root directory containing run folders
## DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/NF_FLU")
DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/backup_NF_FLU_backup")

# If your webserver exposes DATA_ROOT at some URL, set BASE_URL.
# Example: BASE_URL <- "https://internal.example.org/nf_flu"
# If NULL, we'll show the filesystem path instead of a clickable URL.
BASE_URL <- Sys.getenv("NF_FLU_BASE_URL", unset = "")
if (BASE_URL == "") BASE_URL <- NULL

# ---- helpers ----

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a) && a != "") a else b

# Try to resolve a "results root" for a run directory.
# Returns a list with:
#   run_dir, results_dir, pipeline_dir (may equal run_dir), mode ("direct" or "nested"), mtime
resolve_results_dir <- function(run_dir) {
  # Case B: results directly under run_dir
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

  # Case A: nested pipeline runs; choose newest dir that contains results/
  subs <- dir_ls(run_dir, type = "directory", recurse = FALSE)
  if (length(subs) == 0) return(NULL)

  candidates <- keep(subs, ~dir_exists(path(.x, "results")))
  if (length(candidates) == 0) return(NULL)

  mt <- map_dfr(candidates, ~{
    info <- file_info(.x)
    tibble(pipeline_dir = .x, mtime = info$modification_time)
  }) %>% arrange(desc(mtime))

  chosen <- mt$pipeline_dir[[1]]
  return(list(
    run_dir = run_dir,
    pipeline_dir = chosen,
    results_dir = path(chosen, "results"),
    mode = "nested",
    mtime = mt$mtime[[1]]
  ))
}

# Build a web URL for a filesystem path under DATA_ROOT if BASE_URL is configured
path_to_url <- function(fs_path) {
  if (is.null(BASE_URL)) return(NULL)
  # Ensure consistent separators
  rel <- path_rel(fs_path, start = DATA_ROOT)
  # Convert backslashes if any
  rel <- gsub("\\\\", "/", rel)
  paste0(rtrim_slash(BASE_URL), "/", rel)
}

rtrim_slash <- function(x) sub("/+$", "", x)

# Read subtyping results
read_subtyping <- function(results_dir) {
  f <- path(results_dir, "subtyping_report", "subtype_results.csv")
  if (!file_exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE)
}

# Read nextclade results
read_nextclade <- function(results_dir) {
  f <- path(results_dir, "nextclade", "nextclade.tsv")
  if (!file_exists(f)) return(NULL)
  read_tsv(f, show_col_types = FALSE)
}

# Try to standardize sample id column from nf-flu outputs
standardize_sample_id <- function(df) {
  if (is.null(df)) return(df)
  nms <- names(df)

  # common: "sample", "sample_id", "seqName", "name"
  cand <- intersect(nms, c("sample", "sample_id", "seqName", "name", "sequence_name", "SequenceName"))
  if (length(cand) >= 1) {
    df <- df %>% rename(sample_id = all_of(cand[[1]]))
  } else {
    # fallback: keep as-is; user will see NA joins
    df$sample_id <- NA_character_
  }

  df
}

# Derive H/N/type columns from subtyping table if present
extract_HN <- function(sub_df) {
  if (is.null(sub_df)) return(NULL)

  sub_df <- standardize_sample_id(sub_df)

  # Heuristics: nf-flu "subtype_results.csv" often has columns with subtype prediction
  # We'll look for likely fields.
  nms <- names(sub_df)

  # Typical possibilities
  # - "influenza_type" or "type"
  # - "H_subtype" / "N_subtype"
  # - "subtype" like "A/H1N1"
  type_col <- intersect(nms, c("influenza_type", "type", "virus_type", "InfluenzaType"))
  sub_col  <- intersect(nms, c("subtype", "subtype_prediction", "prediction", "Subtype"))

  out <- sub_df

  if (length(type_col) >= 1) out <- out %>% rename(influenza_type = all_of(type_col[[1]]))
  if (length(sub_col)  >= 1) out <- out %>% rename(subtype = all_of(sub_col[[1]]))

  # If H/N exist directly
  h_col <- intersect(nms, c("H", "H_subtype", "HA", "ha_subtype", "H_type"))
  n_col <- intersect(nms, c("N", "N_subtype", "NA", "na_subtype", "N_type"))
  if (length(h_col) >= 1) out <- out %>% rename(H = all_of(h_col[[1]]))
  if (length(n_col) >= 1) out <- out %>% rename(N = all_of(n_col[[1]]))

  # If H/N not present but subtype like "A/H1N1" or "H3N2" exists, parse it
  if (!("H" %in% names(out)) || !("N" %in% names(out))) {
    if ("subtype" %in% names(out)) {
      st <- as.character(out$subtype)
      # extract H\d+ and N\d+ patterns
      out$H <- out$H %||% str_extract(st, "H\\d+")
      out$N <- out$N %||% str_extract(st, "N\\d+")
      # extract type A/B if present in subtype string
      if (!("influenza_type" %in% names(out))) {
        out$influenza_type <- str_extract(st, "^[AB]") %||% NA_character_
      }
    }
  }

  out
}

# Derive clade/subclade/QC from nextclade table
extract_nextclade <- function(nx_df) {
  if (is.null(nx_df)) return(NULL)

  nx_df <- standardize_sample_id(nx_df)
  nms <- names(nx_df)

  # nextclade typically has: clade, subclade (or maybe "subclade" not always)
  clade_col <- intersect(nms, c("clade", "Clade"))
  subcl_col  <- intersect(nms, c("subclade", "Subclade"))

  out <- nx_df
  if (length(clade_col) >= 1) out <- out %>% rename(clade = all_of(clade_col[[1]]))
  if (length(subcl_col)  >= 1) out <- out %>% rename(subclade = all_of(subcl_col[[1]]))

  # QC columns in nextclade often: qc.overallStatus, qc.overallScore, or similar
  qc_status_col <- intersect(nms, c("qc.overallStatus", "qc_overallStatus", "overallStatus"))
  qc_score_col  <- intersect(nms, c("qc.overallScore", "qc_overallScore", "overallScore"))

  if (length(qc_status_col) >= 1) out <- out %>% rename(qc_status = all_of(qc_status_col[[1]]))
  if (length(qc_score_col)  >= 1) out <- out %>% rename(qc_score  = all_of(qc_score_col[[1]]))

  # Keep only a useful subset (you can expand later)
  keep_cols <- intersect(names(out), c("sample_id", "clade", "subclade", "qc_status", "qc_score"))
  if (length(keep_cols) >= 2) out <- out %>% select(all_of(keep_cols))

  out
}

# Create a run registry dataframe
list_runs <- function() {
  if (!dir_exists(DATA_ROOT)) return(tibble())

  run_dirs <- dir_ls(DATA_ROOT, type = "directory", recurse = FALSE)

  # exclude obvious non-run dirs if you have them (logs, scripts etc.)
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
        mtime = file_info(rd)$modification_time,
        multiqc_path = NA_character_,
        multiqc_url = NA_character_
      )
    } else {
      results_dir <- resolved$results_dir
      multiqc_path <- path(results_dir, "MultiQC", "multiqc_report.html")
      has_nextclade <- file_exists(path(results_dir, "nextclade", "nextclade.tsv"))
      has_subtyping <- file_exists(path(results_dir, "subtyping_report", "subtype_results.csv"))
      status <- if (has_nextclade && has_subtyping) "done" else "partial"

      mq_url <- if (file_exists(multiqc_path)) path_to_url(multiqc_path) else NULL

      tibble(
        run = basename(rd),
        status = status,
        run_dir = rd,
        pipeline_dir = resolved$pipeline_dir,
        results_dir = results_dir,
        mtime = resolved$mtime,
        multiqc_path = if (file_exists(multiqc_path)) multiqc_path else NA_character_,
        multiqc_url = mq_url %||% NA_character_
      )
    }
  })

  runs <- bind_rows(rows) %>%
    arrange(desc(mtime))

  # sample count: try from subtyping (fast), else from samplesheet if you later add it
  runs$sample_n <- map_int(runs$results_dir, function(rdir) {
    if (is.na(rdir) || !dir_exists(rdir)) return(NA_integer_)
    sub <- read_subtyping(rdir)
    if (!is.null(sub)) return(nrow(sub))
    nx <- read_nextclade(rdir)
    if (!is.null(nx)) return(nrow(nx))
    NA_integer_
  })

  runs
}

# ---- UI ----

ui <- fluidPage(
  titlePanel("NF_FLU Influenza Dashboard"),
  tags$style(HTML("
    .status-done { font-weight: 600; }
    .status-partial { color: #b36b00; font-weight: 600; }
    .status-no_results { color: #777; }
  ")),
  fluidRow(
    column(
      12,
      h3("Runs"),
      DTOutput("runs_tbl"),
      hr(),
      uiOutput("run_header"),
      DTOutput("samples_tbl")
    )
  )
)

# ---- Server ----

server <- function(input, output, session) {

  runs_r <- reactiveVal(list_runs())

  # refresh button could be added later; for now, refresh on load and every minute
  observe({
    invalidateLater(60000, session)
    runs_r(list_runs())
  })

  output$runs_tbl <- renderDT({
    runs <- runs_r()
    if (nrow(runs) == 0) return(datatable(data.frame()))
    disp <- runs %>%
      transmute(
        Run = run,
        Status = status,
        Samples = sample_n,
        Updated = format(mtime, "%Y-%m-%d %H:%M"),
        MultiQC = ifelse(!is.na(multiqc_url), "Open", ifelse(!is.na(multiqc_path), "Local", "—"))
      )

    datatable(
      disp,
      selection = "single",
      options = list(pageLength = 15, autoWidth = TRUE)
    )
  })

  selected_run <- reactive({
    runs <- runs_r()
    idx <- input$runs_tbl_rows_selected
    if (length(idx) != 1) return(NULL)
    runs[idx, , drop = FALSE]
  })

  output$run_header <- renderUI({
    sr <- selected_run()
    if (is.null(sr)) return(NULL)

    mq <- if (!is.na(sr$multiqc_url)) {
      tags$a(href = sr$multiqc_url, target = "_blank", "MultiQC report")
    } else if (!is.na(sr$multiqc_path)) {
      tags$span("MultiQC (local path): ", tags$code(sr$multiqc_path))
    } else {
      tags$span("MultiQC: —")
    }

    tags$div(
      h3(paste0("Run: ", sr$run)),
      tags$p(tags$b("Status: "), sr$status, " | ", tags$b("Samples: "), sr$sample_n %||% "—"),
      tags$p(mq),
      tags$p(tags$small(tags$code(sr$results_dir)))
    )
  })

  output$samples_tbl <- renderDT({
    sr <- selected_run()
    if (is.null(sr) || is.na(sr$results_dir)) return(datatable(data.frame()))

    results_dir <- sr$results_dir[[1]]

    sub <- extract_HN(read_subtyping(results_dir))
    nx  <- extract_nextclade(read_nextclade(results_dir))

    if (is.null(sub) && is.null(nx)) return(datatable(data.frame()))

    # join by sample_id
    df <- full_join(sub, nx, by = "sample_id")

    # choose columns to show (expand later)
    show_cols <- intersect(
      names(df),
      c("sample_id", "influenza_type", "H", "N", "subtype", "clade", "subclade", "qc_status", "qc_score")
    )
    if (length(show_cols) == 0) show_cols <- names(df)

    disp <- df %>% select(all_of(show_cols))

    dt <- datatable(
      disp,
      options = list(pageLength = 25, autoWidth = TRUE),
      rownames = FALSE
    )

    # simple coloring rules (can be refined)
    if ("qc_status" %in% names(disp)) {
      dt <- dt %>% formatStyle(
        "qc_status",
        backgroundColor = styleEqual(
          c("good", "mediocre", "bad", "fail", "warning", "unknown"),
          c("#dff0d8", "#fcf8e3", "#f2dede", "#f2dede", "#fcf8e3", "#eeeeee")
        )
      )
    }
    if ("influenza_type" %in% names(disp)) {
      dt <- dt %>% formatStyle(
        "influenza_type",
        fontWeight = "bold"
      )
    }
    dt
  })

}

shinyApp(ui, server)
