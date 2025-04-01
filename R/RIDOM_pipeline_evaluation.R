library(magrittr)

RIDOM_comptable_re_evaluate <- function (old_folder, new_folder,
                                         wanted_cols = c(
                                           "Sample ID",
                                           "Perc. Good Targets",
                                           "Avg. Coverage (Assembled)",
                                           "ST", "ST Warwick",
                                           "Complex Type",
                                           "N50 (Assembled)",
                                           "O Type", "H Type",
                                           "Serovar")){
  read_file_folder <- function (fo = folder) {
    files <- list.files(fo, pattern = "*.csv", full.names = TRUE)
    dfs <- lapply(files, function (x) {
      readr::read_delim(x,  delim = ";",
                        locale = readr::locale(decimal_mark = "."),
                        show_col_types = FALSE)
    })
    dfs <- lapply(dfs, function(x) x[colnames(x)%in%wanted_cols])
    names(dfs) <- basename(files)
    dfs
  }
  old_dfs <- read_file_folder(old_folder)
  new_dfs <- read_file_folder(new_folder)
  list(old=old_dfs, new=new_dfs)
}

foo <- RIDOM_comptable_re_evaluate("O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/2025_03_19/",
                                   "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/2025_03_20/")


combine_assembly_stats <- function(old, new) {
  ass_list <- lapply(seq_along(old), function(i){
    comb <- old[[i]] %>%
      dplyr::inner_join(new[[i]], by = "Sample ID",  suffix = c("_old", "_new")) %>%
      dplyr::select(`Sample ID`, `N50 (Assembled)_old`, `N50 (Assembled)_new`) %>%
      dplyr::mutate(diff_perc = (`N50 (Assembled)_new` - `N50 (Assembled)_old`) /
                      `N50 (Assembled)_old`)
  })
  do.call("rbind", ass_list)
}

combine_ehec_stats <- function(old, new) {
  oldEHEC <- old[[grep("EHEC", names(old))]] %>% tidyr::as_tibble()
  newEHEC <- new[[grep("EHEC", names(new))]] %>% tidyr::as_tibble()
  comb <- oldEHEC %>%
    dplyr::inner_join(newEHEC, by = "Sample ID",  suffix = c("_old", "_new")) %>%
    dplyr::mutate(
      Targets_Eval = sign(`Perc. Good Targets_old` - `Perc. Good Targets_new`),
      ST_Eval = as.numeric(!is.na(`ST Warwick_old`) - !is.na(`ST Warwick_new`)),
      CT_Eval = as.numeric(!is.na(`Complex Type_old`) -
                             !is.na(`Complex Type_new`)),
      HT_Eval = as.numeric(!is.na(`H Type_old`) - !is.na(`H Type_new`)),
      OT_Eval = as.numeric(!is.na(`O Type_old`) - !is.na(`O Type_new`))
    ) %>%
   dplyr::select(`Sample ID`, Targets_Eval, ST_Eval, CT_Eval, HT_Eval, OT_Eval)
  comb
}

combine_salmonella_stats <- function(old, new) {
  oldSal <- old[[grep("Salmonella", names(old))]] %>% tidyr::as_tibble()
  newSal <- new[[grep("Salmonella", names(new))]] %>% tidyr::as_tibble()
  comb <- oldSal %>%
    dplyr::inner_join(newSal, by = "Sample ID",  suffix = c("_old", "_new")) %>%
    dplyr::mutate(
      Targets_Eval = sign(`Perc. Good Targets_old` - `Perc. Good Targets_new`),
      ST_Eval = as.numeric(!is.na(`ST_old`) - !is.na(`ST_new`)),
      CT_Eval = as.numeric(!is.na(`Complex Type_old`) -
                             !is.na(`Complex Type_new`)),
      SV_Eval = as.numeric(!is.na(`Serovar_old`) - !is.na(`Serovar_new`))
    ) %>%
    dplyr::select(`Sample ID`, Targets_Eval, ST_Eval, CT_Eval, SV_Eval)
  comb
}


combine_general_stats <- function(old, new, what) {
  oldThis <- old[[grep(what, names(old))]] %>% tidyr::as_tibble()
  newThis <- new[[grep(what, names(new))]] %>% tidyr::as_tibble()
  comb <- oldThis %>%
    dplyr::inner_join(newThis, by = "Sample ID",  suffix = c("_old", "_new")) %>%
    dplyr::mutate(
      Targets_Eval = sign(`Perc. Good Targets_old` - `Perc. Good Targets_new`),
      ST_Eval = as.numeric(!is.na(`ST_old`) - !is.na(`ST_new`)),
      CT_Eval = as.numeric(!is.na(`Complex Type_old`) -
                             !is.na(`Complex Type_new`))
    ) %>%
    dplyr::select(`Sample ID`,
                  Targets_Eval, ST_Eval, CT_Eval)
  comb
}



Assembly <- combine_assembly_stats(foo[["old"]], foo[["new"]])

EHEC <- combine_ehec_stats(foo[["old"]], foo[["new"]])

Salmonella <- combine_salmonella_stats(foo[["old"]], foo[["new"]])

Listeria <- combine_general_stats(foo[["old"]], foo[["new"]], "Listeria")

MRSA <- combine_general_stats(foo[["old"]], foo[["new"]], "MRSA")

Legionella <- combine_general_stats(foo[["old"]], foo[["new"]], "Legionella")

library(readr)

write_csv2(Assembly,
           "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/Vergleiche/2025_03_19_VS_2025_03_20/Assembly.csv")

write_csv2(EHEC,
           "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/Vergleiche/2025_03_19_VS_2025_03_20/EHEC.csv")

write_csv2(Salmonella,
           "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/Vergleiche/2025_03_19_VS_2025_03_20/Salmonella.csv")

write_csv2(Listeria,
           "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/Vergleiche/2025_03_19_VS_2025_03_20/Listeria.csv")

write_csv2(MRSA,
           "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/Vergleiche/2025_03_19_VS_2025_03_20/MRSA.csv")

write_csv2(Legionella,
           "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/13_QM/Validierung Pipeline/Vergleiche/2025_03_19_VS_2025_03_20/Legionella.csv")


