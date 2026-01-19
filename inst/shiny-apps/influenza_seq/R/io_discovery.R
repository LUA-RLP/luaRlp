# R/io_discovery.R

resolve_results_dir <- function(run_dir) {
  direct <- fs::path(run_dir, "results")
  if (fs::dir_exists(direct)) {
    return(list(
      run_dir = run_dir,
      pipeline_dir = run_dir,
      results_dir = direct,
      mode = "direct",
      mtime = fs::file_info(direct)$modification_time
    ))
  }

  subs <- fs::dir_ls(run_dir, type = "directory", recurse = FALSE)
  if (length(subs) == 0) return(NULL)

  candidates <- purrr::keep(subs, ~fs::dir_exists(fs::path(.x, "results")))
  if (length(candidates) == 0) return(NULL)

  mt <- purrr::map_dfr(candidates, ~{
    info <- fs::file_info(.x)
    tibble::tibble(pipeline_dir = .x, mtime = info$modification_time)
  }) %>% dplyr::arrange(dplyr::desc(mtime))

  chosen <- mt$pipeline_dir[[1]]
  list(
    run_dir = run_dir,
    pipeline_dir = chosen,
    results_dir = fs::path(chosen, "results"),
    mode = "nested",
    mtime = mt$mtime[[1]]
  )
}

find_samplesheet_path <- function(pipeline_dir, results_dir) {
  candidates <- c(
    fs::path(pipeline_dir, "samplesheet.csv"),
    fs::path(results_dir, "pipeline_info", "samplesheet.fixed.csv"),
    fs::path(results_dir, "pipeline_info", "samplesheet.csv"),
    fs::path(pipeline_dir, "pipeline_info", "samplesheet.fixed.csv")
  )
  hit <- candidates[fs::file_exists(candidates)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}
