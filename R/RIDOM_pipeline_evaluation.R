
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
    attr(all_stats, "old_folder") <- old_folder
    attr(all_stats, "new_folder") <- new_folder
    attr(all_stats, "out_folder") <- out_folder
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
      dplyr::mutate(diff_perc = ((.data$`N50 (Assembled)_new` -
                                   .data$`N50 (Assembled)_old`) /
                      .data$`N50 (Assembled)_old`)*100)
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

#' tabulate_pipeline_QM
#'
#' @param a list of tibbles from combine_general_stats
#'
#' @return a list of summarized tibbles
#'
#' @import tidyr
#'
#' @export
#'
tabulate_pipeline_QM <- function(x) {
  ## Slighly special for the Assembly SOP
  SOP <- x[["Assembly"]] %>%
    select(where(is.numeric)) %>%
    summarize(
      "n Proben erhöht"     = sum(diff_perc > 0),
      "n Proben gleich"     = sum(diff_perc == 0),
      "n Proben verringert" = sum(diff_perc < 0),
      "n Proben >= 20% erhöht"     = sum(diff_perc >= 20),
      "n Proben innerhalb 20% verändert"     = sum(abs(diff_perc) < 20 ),
      "n Proben >= 20% verringert" = sum(diff_perc <= -20),
      "Bewertung (arith. Mittel >= -20%)" = ifelse(mean(diff_perc <= -20),
                         "NICHT erfolgreich", "ERFOLGREICH")) %>%
    mutate(across(everything(), as.character)) %>%
    tidyr::pivot_longer(everything(), names_to = "N50 Veränderung", values_to = "Wert")
  ## Same for each Prüfmethode
  PRMs <- lapply(x[names(x) != "Assembly"], function(stab){
    stab %>%
      select(where(is.numeric)) %>%
      summarise(across(
        everything(),
        list(
          "n Proben verbessert" = ~as.character(length(.x[.x>0])),
          "n Proben gleich" = ~as.character(length(.x[.x==0])),
          "n Proben verschlechtert" = ~as.character(length(.x[.x<0])),
          "arith. Mittel Veränderung" =
            ~as.character(mean(.x, na.rm = TRUE)),
          Bewertung =
            ~ifelse(mean(.x, na.rm = TRUE) > 0,
                    "BESTANDEN",
                    ifelse(mean(.x, na.rm = TRUE) == 0,
                           "BESTANDEN", "EINZELWERTKONTROLLE!!!"))
        ),
        .names = "{.col};{.fn}"
      )) %>%
      tidyr::pivot_longer(cols = everything(), names_to = c("variable", "stat"),
                   names_sep = ";") %>%
      tidyr::pivot_wider(names_from = variable, values_from = value)
  })
  out <- c(Assembly = list(SOP), PRMs)
  attr(out, "old_folder") <- attr(x, "old_folder")
  attr(out, "new_folder") <- attr(x, "new_folder")
  attr(out, "out_folder") <- attr(x, "out_folder")
  out
}



create_QMpdf_for_signatures <- function (qm_list = NULL,
                                         old_folder = NULL,
                                         new_folder = NULL,
                                         output_pdf){

  ### Dumping some data on how the QM documents are called... quite the quality
  ### HACK
  qm_meta <- tibble::tibble(
    name   = c("Assembly", "EHEC", "Salmonella", "Legionella",
               "MRSA", "Listeria"),
    title  = c("Assemblierung von „Short-Read“-Sequenz-Daten zur
             Rekonstruktion von Bakteriengenomen",
               "Genotypisierung von Escherichia coli aus Genomsequenzdaten im
             FASTA-Format mittels Hausverfahren",
               "Genotypisierung von Salmonella enterica aus Genomsequenzdaten im
             FASTA-Format mittels Hausverfahren",
               "Genotypisierung von Staphylococcus aureus aus Genomsequenzen im
             FASTA-Format mittels Hausverfahren",
               "Genotypisierung von Legionella pneumophila aus Genomsequenzdaten
             im FASTA-Format mittels Hausverfahren",
               "Genotypisierung von Listeria monocytogenes aus Genomsequenzdaten im FASTA-Format mittels Hausverfahren"),
    code   = c("SOP P 32.6 0004 01",
               "PRM 0 32.6 0004 01",
               "PRM 0 32.6 0003 01",
               "PRM 0 32.6 0007 01",
               "PRM 0 32.6 0006 01",
               "PRM 0 32.6 0005 01")
  )

  write_latex_header <- function(name, meta_df) {
    meta <- meta_df[meta_df$name == name, ]
    c(
      "\\noindent",
      "\\begin{tabular}{|m{3cm}|m{10cm}|m{4cm}|}",
      "\\hline",
      "\\multirow{2}{*}{%",
      "  \\centering",
      paste0(" \\raisebox{-0.8cm}{\\includegraphics[height=1.0cm,keepaspectratio]{",
             system.file("QM_header/logo.png", package = "luaRlp"),
             "}}%"),
      "}",
      "& \\textbf{Landesuntersuchungsamt} & \\RaggedLeft Anhang 2, Seite 1 von 1 \\\\",
      "\\cline{2-3}",
      paste0("& \\textbf{", meta$title, "} & \\RaggedLeft ", meta$code, " \\\\"),
      "\\hline",
      "\\end{tabular}",
      "",
      "\\vspace{1cm}",
      "\\noindent",
      "\\bigskip",
      ""
    )
  }


  ## Step 1: Run the full tabulation
  if(!is.null(old_folder) & !is.null(new_folder)){
  qm_list <- RIDOM_comptable_re_evaluate(old_folder, new_folder) %>%
    tabulate_pipeline_QM()

  }
  temp_rdata <- tempfile(fileext = ".RData")
  save(qm_list, file = temp_rdata)

  # Step 3: Write temporary Rmd file
  temp_rmd <- tempfile(fileext = ".Rmd")
  writeLines(c(
    "---",
    "output: pdf_document",
    "includes:",
    "in_header: null",
    "before_body: null",
    "after_body: null",
    "keep_tex: true",
    "header-includes:",
      "- \\usepackage{booktabs}",
      "- \\usepackage[table]{xcolor}",
      "- \\usepackage{multirow}",
      "- \\usepackage{ragged2e}",
    "fontsize: 10pt",
    "geometry: margin=2cm",
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)",
    "library(dplyr)",
    "library(kableExtra)",
    "library(tidyr)",
    "library(luaRlp)",

    paste0("load(\"", normalizePath(temp_rdata, winslash = "/"), "\")  # Loads qm_list"),

    "```",
    "",
    "```{r tables, results='asis'}",
    "for (name in names(qm_list)) {",
    "  cat(paste(write_latex_header(name, qm_meta), collapse = '\n'))",
    "  cat('Vergleich von',
    '\\n\\n',
    attributes(qm_list)$old_folder,
    '\\n\\n',
    'und',
    '\\n\\n',
    attributes(qm_list)$new_folder,
    '\\n\\n')",
    "  cat(",
    "    qm_list[[name]] %>%",
    "      kable('latex', booktabs = TRUE, linesep = '', caption = name) %>%",
    "      kable_styling(latex_options = c('striped', 'hold_position'))",
    "  )",
    "  cat(
        '\\n\\n',
        'abgelegt in',
        '\\n\\n',
    attributes(qm_list)$out_folder,
    '\\n\\n',
        'Arbeitsverzeichnis:',
        '\\n\\n',
    getwd(),
    '\\n\\n',
    'Akutelle Datei:',
    '\\n\\n',
    output_pdf,
    '\\n\\\\newpage\\n')",
    "}",
    "```"
  ), con = temp_rmd)
  message("using temporary file ", temp_rmd)
  # Step 4: Render to PDF
  message("writing to ", normalizePath(output_pdf, mustWork = FALSE))
  rmarkdown::render(
    input = temp_rmd,
    output_file = normalizePath(output_pdf, mustWork = FALSE),
    quiet = FALSE
  )
}



