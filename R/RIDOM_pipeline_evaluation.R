
#' RIDOM_comptable_re_evaluate
#'
#' @param old_folder A folder containing RIDOM comparison tables in csv format,
#' these should be "old", e.g. from a previously run, changed pipeline or simply
#' the old state of exports in a routin re-evaluation
#'
#' @param new_folder A second folder containing RIDOM comparison tables in csv
#' format, these should be "new", e.g. from a novel changed pipeline or simply
#' the new state of exports in a routine re-evaluation
#'
#'
#' @param wanted_cols the columns that should be compared. Defaults to
#' "Sample ID", "Perc. Good Targets", "Avg. Coverage (Assembled)", "ST"
#' (all projects but EHEC), "ST Warwick" (project Produktiv_EHEC),
#' "Complex Type", "N50 (Assembled)", "O Type" (project Produktiv_EHEC),
#' "H Type" (project Produktiv_EHEC) and "Serovar" (project Produkiv_Salmonella)
#'
#' @param out_folder output folder for the comparison tables of old vs. new. By
#' default this will be set to "old_folder_VS_new_folder_timestamp/" in the
#' current working directory.
#'
#' @return csv files are written into a folder: One file for each Assembly
#' (SOP), EHEC, Salmonella, Listeria, MRSA and Legionella (each an own PRM),
#' the corresponding dataframes are returned within a list.
#'
#' @export
#'
#' @seealso \code{\link[=vignette]{vignette("QM_pipeline_evaluation")}}
#'
#' @examples
#'Re_Eval <- RIDOM_comptable_re_evaluate(
#'old_folder =
#'"O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/2025_03_19/",
#'new_folder =
#'  "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/2025_03_20/")
#'
RIDOM_comptable_re_evaluate <-
  function (old_folder, new_folder,
            out_folder = paste0(basename(old_folder),
                                "_VS_",
                                basename(new_folder),
                                "_",
                                format(Sys.time(),
                                       "%Y-%m-%d_%H-%M-%S"),
                                "/"),
            wanted_cols = c(
              "Sample ID",
              "Perc. Good Targets",
              "Avg. Coverage (Assembled)",
              "ST", "ST Warwick",
              "Complex Type",
              "N50 (Assembled)",
              "O Type", "H Type",
              "Serovar")){
    read_file_folder <- function (path) {
      message("Reading: ")
      files <- list.files(path = path, pattern = "*.csv", full.names = TRUE)
      dfs <- lapply(files, function (x) {
        message(x)
        suppressMessages(
          suppressWarnings(
            readr::read_delim(x, delim = ";",
                              locale = readr::locale(decimal_mark = "."),
                              show_col_types = FALSE,
                              name_repair = "minimal")
          )
        )
      })
      dfs <- lapply(dfs, function(x) x[colnames(x)%in%wanted_cols])
      names(dfs) <- basename(files)
      dfs
    }
    old_dfs <- read_file_folder(path = old_folder)
    new_dfs <- read_file_folder(path = new_folder)

    ass_stats <- combine_assembly_stats(old_dfs, new_dfs)
    ehec_stats <- combine_ehec_stats(old_dfs, new_dfs)
    salmonella_stats <- combine_salmonella_stats(old_dfs, new_dfs)
    other_stats <- lapply(c("Legionella", "MRSA", "Listeria"), function(x) {
      combine_general_stats(old_dfs, new_dfs, what = x)
    })
    names(other_stats) <- c("Legionella", "MRSA", "Listeria")
    all_stats <- c(list(Assembly = ass_stats,
                        EHEC = ehec_stats,
                        Salmonella = salmonella_stats),
                   other_stats)
    if(!dir.exists(out_folder)){
      message("Ordner ", getwd(), out_folder, " wird angelegt.")
      dir.create(out_folder)
    }
    message("Writing evaluation tables comparing old and new results to:")
    for(i in seq_along(all_stats)){
      path <- paste0(out_folder,  names(all_stats)[[i]], ".csv ")
      if(file.exists(path)){
        stop("Datei ", path, "ist bereits vorhanden! ",
             "Soll die SELBE Evaluierung wirklich wiederholt werden? ",
             "Lösche die Datei oder den Ordner für eine Wiederholung oder
             speichere in einem neuen Ordner für eine NEUE Evaluierung!")
      }
      readr::write_csv2(all_stats[[i]], path)
      message(path)
    }
    all_stats
  }



#' combine_assembly_stats
#'
#' @param old The dataframes derived from old RIDOM comparison tables
#' @param new The dataframes derived from new RIDOM comparison tables
#'
#' @return A dataframe showing assembly comparisons. The difference of the N50
#' values of old vs. new assemblies are given percent.
#'
#' @importFrom magrittr %>%
#'
combine_assembly_stats <- function(old, new) {
  ass_list <- lapply(seq_along(old), function(i){
    comb <- old[[i]] %>%
      dplyr::inner_join(new[[i]], by = "Sample ID",  suffix = c("_old", "_new")) %>%
      dplyr::select(.data$`Sample ID`, .data$`N50 (Assembled)_old`,
                    .data$`N50 (Assembled)_new`) %>%
      dplyr::mutate(diff_perc = (.data$`N50 (Assembled)_new` -
                                   .data$`N50 (Assembled)_old`) /
                      .data$`N50 (Assembled)_old`)
  })
  do.call("rbind", ass_list)
}

#' combine_ehec_stats
#'
#' @param old The dataframes derived from old RIDOM comparison tables
#' @param new The dataframes derived from new RIDOM comparison tables
#'
#' @return a dataframe with comparisons for EHEC
#'
#' @importFrom magrittr %>%
#'
combine_ehec_stats <- function(old, new) {
  oldEHEC <- old[[grep("EHEC", names(old))]] %>% tidyr::as_tibble()
  newEHEC <- new[[grep("EHEC", names(new))]] %>% tidyr::as_tibble()
  comb <- oldEHEC %>%
    dplyr::inner_join(newEHEC, by = "Sample ID",  suffix = c("_old", "_new")) %>%
    dplyr::mutate(
      Targets_Eval = sign(.data$`Perc. Good Targets_old` -
                            .data$`Perc. Good Targets_new`),
      ST_Eval = as.numeric(!is.na(.data$`ST Warwick_old`) -
                             !is.na(.data$`ST Warwick_new`)),
      CT_Eval = as.numeric(!is.na(.data$`Complex Type_old`) -
                             !is.na(.data$`Complex Type_new`)),
      HT_Eval = as.numeric(!is.na(.data$`H Type_old`) -
                             !is.na(.data$`H Type_new`)),
      OT_Eval = as.numeric(!is.na(.data$`O Type_old`) -
                             !is.na(.data$`O Type_new`))
    ) %>%
   dplyr::select(.data$`Sample ID`, .data$Targets_Eval, .data$ST_Eval,
                 .data$CT_Eval, .data$HT_Eval, .data$OT_Eval)
  comb
}

#' combine_salmonella_stats
#'
#' @param old The dataframes derived from old RIDOM comparison tables
#' @param new The dataframes derived from new RIDOM comparison tables
#'
#' @return a dataframe with comparisons for Salmonella
#'
#' @importFrom magrittr %>%
#'
combine_salmonella_stats <- function(old, new) {
  oldSal <- old[[grep("Salmonella", names(old))]] %>% tidyr::as_tibble()
  newSal <- new[[grep("Salmonella", names(new))]] %>% tidyr::as_tibble()
  comb <- oldSal %>%
    dplyr::inner_join(newSal, by = "Sample ID",  suffix = c("_old", "_new")) %>%
    dplyr::mutate(
      Targets_Eval = sign(.data$`Perc. Good Targets_old` -
                            .data$`Perc. Good Targets_new`),
      ST_Eval = as.numeric(!is.na(.data$`ST_old`) - !is.na(.data$`ST_new`)),
      CT_Eval = as.numeric(!is.na(.data$`Complex Type_old`) -
                             !is.na(.data$`Complex Type_new`)),
      SV_Eval = as.numeric(!is.na(.data$`Serovar_old`) -
                             !is.na(.data$`Serovar_new`))
    ) %>%
    dplyr::select(.data$`Sample ID`, .data$Targets_Eval, .data$ST_Eval,
                  .data$CT_Eval, .data$SV_Eval)
  comb
}


#' combine_general_stats
#'
#' @param old The dataframes derived from old RIDOM comparison tables
#' @param new The dataframes derived from new RIDOM comparison tables
#'
#' @param what either Listeria, MRSA or Legionella
#'
#' @return a data frame containg comparison statistics for what
#'
#' @importFrom magrittr %>%
#'
combine_general_stats <- function(old, new, what) {
  oldThis <- old[[grep(what, names(old))]] %>% tidyr::as_tibble()
  newThis <- new[[grep(what, names(new))]] %>% tidyr::as_tibble()
  comb <- oldThis %>%
    dplyr::inner_join(newThis, by = "Sample ID",  suffix = c("_old", "_new")) %>%
    dplyr::mutate(
      Targets_Eval = sign(`Perc. Good Targets_old` - `Perc. Good Targets_new`),
      ST_Eval = as.numeric(!is.na(.data$`ST_old`) - !is.na(.data$`ST_new`)),
      CT_Eval = as.numeric(!is.na(.data$`Complex Type_old`) -
                             !is.na(.data$`Complex Type_new`))
    ) %>%
    dplyr::select(.data$`Sample ID`,
                  .data$Targets_Eval, .data$ST_Eval, .data$CT_Eval)
  comb
}

