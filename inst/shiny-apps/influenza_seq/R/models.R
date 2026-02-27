# R/models.R

# ---- Run listing (fast) ----
list_runs_fast <- function(data_root, base_url) {
  if (!fs::dir_exists(data_root)) return(tibble::tibble())
  run_dirs <- fs::dir_ls(data_root, type = "directory", recurse = FALSE)

  rows <- purrr::map(run_dirs, function(rd) {
    resolved <- resolve_results_dir(rd)
    if (is.null(resolved)) {
      return(tibble::tibble(
        run = basename(rd),
        status = "no_results",
        run_dir = rd,
        pipeline_dir = NA_character_,
        results_dir = NA_character_,
        updated = fs::file_info(rd)$modification_time,
        has_samplesheet = FALSE,
        samplesheet_path = NA_character_,
        multiqc_path = NA_character_,
        multiqc_url = NA_character_
      ))
    }

    results_dir <- resolved$results_dir
    pipeline_dir <- resolved$pipeline_dir

    has_nextclade <- fs::file_exists(fs::path(results_dir, "nextclade", "nextclade.tsv"))
    has_subtyping <- fs::file_exists(fs::path(results_dir, "subtyping_report", "subtype_results.csv"))
    status <- if (has_nextclade && has_subtyping) "done" else "partial"

    ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
    has_ss <- (!is.na(ss_path) && fs::file_exists(ss_path))

    multiqc_path <- fs::path(results_dir, "MultiQC", "multiqc_report.html")
    mq_url <- if (fs::file_exists(multiqc_path)) path_to_url(multiqc_path, data_root, base_url) else NULL

    tibble::tibble(
      run = basename(rd),
      status = status,
      run_dir = rd,
      pipeline_dir = pipeline_dir,
      results_dir = results_dir,
      updated = resolved$mtime,
      has_samplesheet = has_ss,
      samplesheet_path = ss_path,
      multiqc_path = if (fs::file_exists(multiqc_path)) multiqc_path else NA_character_,
      multiqc_url = mq_url
    )
  })

  dplyr::bind_rows(rows) %>% dplyr::arrange(dplyr::desc(updated))
}

# ---- Build sample-level joined table for ONE run ----
build_sample_table <- function(pipeline_dir, results_dir) {
  ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
  ss <- read_samplesheet(ss_path)
  if (is.null(ss) || nrow(ss) == 0) return(NULL)

  rc <- parse_readcount(read_pass_readcount_raw(results_dir))
  sub <- parse_subtyping(read_subtyping_raw(results_dir))
  nx_raw <- read_nextclade_raw(results_dir)

    # NEW: reduce nextclade.tsv (multi-row per sample/segment) to 1 row per sample_id,
    # preferring the HA segment for clade/subclade and QC.
    nx <- parse_nextclade_ha(nx_raw)

    # optional debug: how many samples got clade/subclade
    ss_ids <- ss$sample_id
    dbg("nextclade (HA) rows=", nrow(nx),
    " matching samplesheet=", sum(nx$sample_id %in% ss_ids, na.rm = TRUE),
    " with clade=", sum(!is.na(nx$clade) & nzchar(nx$clade), na.rm = TRUE),
    " with subclade=", sum(!is.na(nx$subclade) & nzchar(nx$subclade), na.rm = TRUE))


  df <- ss %>%
    dplyr::select(sample_id) %>%
    dplyr::left_join(rc,  by = "sample_id") %>%
    dplyr::left_join(sub, by = "sample_id") %>%
    dplyr::left_join(nx,  by = "sample_id")

df <- ensure_cols(df, c(
  "read_count","influenza_type","H","N","subtype",
  "clade","subclade","qc_status","qc_score","coverage"
))
  df %>%
    dplyr::mutate(
      has_reads = !is.na(read_count) & read_count > 0,
      has_hn = !is.na(H) | !is.na(N),
      has_subtype = !is.na(subtype) | !is.na(influenza_type),

      # IMPORTANT: count nextclade only if clade/subclade exists
      clade    = dplyr::na_if(as.character(clade), ""),
      subclade = dplyr::na_if(as.character(subclade), ""),

      has_nextclade = (!is.na(clade) & nzchar(clade)) |
        (!is.na(subclade) & nzchar(subclade)),

      # optional: QC present even if clade/subclade missing
      has_nextclade_qc = !is.na(qc_status),

      passed_reads = !is.na(read_count) & read_count > 0
    )
}

# ---- Cached run-level summary counts ----
# Keyed by results_dir + mtime of results_dir
.run_counts_cache <- new.env(parent = emptyenv())

run_sample_counts_cached <- function(pipeline_dir, results_dir) {
  if (is.na(results_dir) || !fs::dir_exists(results_dir)) {
    return(tibble::tibble(
      n_samples = 0, n_reads = 0, n_hn = 0, n_subtype = 0, n_nextclade = 0
    ))
  }

  mtime <- tryCatch(fs::file_info(results_dir)$modification_time, error = function(e) NA)
  cache_key <- paste0(results_dir, "::", as.character(mtime))

  if (exists(cache_key, envir = .run_counts_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .run_counts_cache, inherits = FALSE))
  }

  # compute
  df <- build_sample_table(pipeline_dir, results_dir)

  out <- if (is.null(df) || nrow(df) == 0) {
    tibble::tibble(
      n_samples = 0, n_reads = 0, n_hn = 0, n_subtype = 0, n_nextclade = 0
    )
  } else {
    tibble::tibble(
      n_samples   = nrow(df),
      n_reads     = sum(df$has_reads, na.rm = TRUE),
      n_hn        = sum(df$has_hn, na.rm = TRUE),
      n_subtype   = sum(df$has_subtype, na.rm = TRUE),
      n_nextclade = sum(df$has_nextclade, na.rm = TRUE)
    )
  }

  assign(cache_key, out, envir = .run_counts_cache)
  out
}
