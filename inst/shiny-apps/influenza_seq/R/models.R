# R/models.R

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

build_sample_table <- function(pipeline_dir, results_dir) {
  ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
  ss <- read_samplesheet(ss_path)
  if (is.null(ss) || nrow(ss) == 0) return(NULL)

  rc <- parse_readcount(read_pass_readcount_raw(results_dir))
  sub <- parse_subtyping(read_subtyping_raw(results_dir))
  nx  <- parse_nextclade(read_nextclade_raw(results_dir))

  df <- ss %>%
    dplyr::select(sample_id) %>%
    dplyr::left_join(rc,  by = "sample_id") %>%
    dplyr::left_join(sub, by = "sample_id") %>%
    dplyr::left_join(nx,  by = "sample_id")

  df <- ensure_cols(df, c("read_count","influenza_type","H","N","subtype","clade","subclade","qc_status","qc_score"))

  df %>%
    dplyr::mutate(
      has_reads = !is.na(read_count),
      has_subtyping = !is.na(H) | !is.na(N) | !is.na(subtype) | !is.na(influenza_type),
      has_nextclade = !is.na(clade) | !is.na(subclade) | !is.na(qc_status),
      passed_reads = !is.na(read_count) & read_count > 0
    )
}
