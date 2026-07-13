
#' createEpidata
#'
#' @param LIMS_link_file The file (exported from our LIMS) associating our
#' LUA Probennummern (3263-) and SurvNet IDs (Aktenzeichen with licence plate
#' prefix), defaults to a path on our System on which automated LIMS exports are
#' stored
#'
#' @param out The output file in xlsx format (defaults to  SurvNetExport4RIDOM
#' plus the current timestamp wirtten into
#' "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/14_EpiDaten/")
#'
#' @param problems If problematic entries should be reported into a file
#' (a filename or path to a file) should be given. Which entries are regarded as
#' problematic are defined via type_of_problem
#'
#' @param type_of_problem should be "non-matching-LIMS". The currently only
#' available option (also the current default) is to write non-matching LIMS
#' entries to a file (given in the argument "problems").
#'
#' @return Writes a xlsx file compatible with the RIDOM database scheme "LUA RLP
#' default bacteria to "\strong{out}" and a optionally a second csv file to
#' "\strong{problem}". See "Format".
#'
#'
#' @format  The xlsx file written to "out" has the following columns:
#' \describe{
#'   \item{Sample ID}{Character. As "Laborprobennummer" from LIMS}
#'   \item{Alias ID (s)}{Character. This is the \strong{merge key}! As
#'   "Survnet" from SurvNet and as "Aktenzeichen" from LIMS. Prefixed with
#'   Kürzel from geo_standards (merged via Eigetuemer_ID).}
#'   \item{Zip of Isolation}{Character. As Postleitzahl from LIMS}
#'   \item{IdRecord}{Character. Unchanged from SurvNet}
#'   \item{Collection Date}{Date. As Meldedatum from SurvNet}
#'   \item{Gesundheitsamt}{Character. Via merge based on
#'   Eigentuemer_ID from SurvNet (merge key) obtaining the Gesundheitsamt
#'   variable from data("geo_standards").}
#'   \item{Meldelandkreis}{Character. Based on LK_ID from SurvNet, clear names
#'   via merge with data("geo_standards").}
#'   \item{MunicipalityKey}{Character. Unchanged from SurvNet}
#'   \item{Host Age (years)}{Numeric. As AgeComputed from SurvNet}
#'   \item{Host Sex}{Character. Recoded from Geschlecht from SurvNet}
#'   \item{HospStatus}{Character.Recoded from HospitalisierungStatus from
#'   SurvNet}
#'   \item{VerstorbenStatus}{Character. Recoded from VerstorbenStatus from
#'   SurvNet}
#'   \item{Expositionsort}{Character. Derived from multiple exposure variables
#'   from SurvNet (see ?luaRlp:::.combine_exposure)}
#'   \item{Outbreak}{Logical. Recoded from SurvNet by determining
#'   whether AusbruchInfo_InterneRef is filled}
#'   \item{Country of Isolation}{Character. Hard-coded "Germany".}
#'   \item{Lat/Long of Isolation}{Character. The latitude and longitude of the
#'   centroid of the Landkreis derived from data("geo_standards") merged via
#'   "Meldelandkreis" (merge key)}
#'   \item{Project}{Character. The RIDOM project name recoded from
#'   Meldekategorie (Erreger) from SurvNet}
#'   \item{Host}{Character. Hardcoded "homo sapiens" for all but Legionella,
#'   which needs to be entered manually (set to "" - empty)}
#'   \item{Source type}{Character. Hardcoded "clinical/host-associated" for all
#'   but Legionella, which needs to be entered manually (set to "" - empty)}
#'   \item{Source subtype}{Character. Hardcoded "human" for all but Legionella,
#'   which needs to be entered manually (set to "" - empty)}
#'   \item{Specimen type}{Character. Hardcoded "stool" for salmonella and EHEC,
#'   ""- empty for Legionella, MRSA and Listeria}
#'   }
#'
#' @export
#'
#' @importFrom readr write_csv2
#' @importFrom magrittr %>%
#' @importFrom utils read.delim
#' @importFrom writexl write_xlsx
#'
create_Epidata <- function(
    LIMS_link_file =
      "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/14_EpiDaten/LIMS_Export_auto/LIMS_Export.csv"                         ,
         out =
           paste0("O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/14_EpiDaten/LIMS Import/",
                  "SurvNetExport4RIDOM",
                  format(Sys.time(), "%Y-%m-%d_%H-%M-%S"), ".xlsx"),
    SurvNet = import_SurvNet(build_query(Thema = "IMS"), ...),
    problems = NULL,
    type_of_problem = "non-matching-LIMS", ...
){
  SN <- SurvNet
  SN <- merge_info(SN, geo_standards, by = "Eigentuemer_ID", "Kürzel",
                   "Gesundheitsamt", first = TRUE) %>%
    merge_info(., geo_standards, "centroid", "Meldelandkreis", by = "LK_ID") %>%
    .combine_exposure() %>%
    mutate(Aktenzeichen = gsub(" ", "", paste0(.data$Kürzel, " - ",
                                               .data$Aktenzeichen)),
           `Lat/Long of Isolation` = geo2ridom(.data$centroid))%>%
    .cleanup_RIDOM()
  IDs <- read.delim(LIMS_link_file, sep = ";")
  Out_EpiDaten <- inner_join(IDs, SN,
                             by = c("Survnet" = "Aktenzeichen")) %>%
    ## rename variables coming from the LIMS merger!
    rename(`Sample ID` = .data$Laborprobennummer,
           `Alias ID (s)` =.data$Survnet,
           `Zip of Isolation` = .data$Postleitzahl)
  write_xlsx(Out_EpiDaten, out)
  ### optinal reporting of a problems-file
  if(!is.null(problems)){
    if(type_of_problem %in%(c("non-matching-LIMS"))){
      p <- IDs %>%
        filter(!Survnet %in% SN$Aktenzeichen)
      write_csv2(p, problems)
    }else{
      stop("Choose a type of problem to report when requesting problems output")
    }
  }
  return(Out_EpiDaten)
}


#' geo2ridom
#'
#' @param x a POINT geometry from sf
#'
#' @returns a RIDOM compatible geo-coordinate point
geo2ridom <- function(x){
  cento <- sapply(x, paste, collapse=", +")
  paste0("+", cento)
}



#' .combine_exposure
#'
#' Derives a single "Expositionsort" Variable from multiple exposure-related
#' variables avialiable in SurvNet (via luaRlp::import_SurvNet())
#'
#' @param data Usually a tibble generated by \code{\link{import_SurvNet()}}
#'
#' @return A tibble derived the
#' tibble as generated by \code{\link{import_SurvNet()}}, but with
#' Expositionsort standardized
#'
#'@import dplyr
#'@importFrom magrittr %>%
#'
.combine_exposure <- function(data){
  # function starting with ".", only used internally
  # Expositionsort: Combine information from multiple exposure variables into one
  dplyr::mutate(data,
    Expositionsort = case_when(
      !is.na(.data$ExpKont2) ~ "mehrere Orte",
      !is.na(.data$ExpLK1) & .data$ExpBL1 == "Rheinland-Pfalz" ~
        as.character(.data$ExpLK1),
      !is.na(.data$ExpBL1) & .data$ExpLand1 == "Deutschland" ~
        as.character(ExpBL1),
      !is.na(.data$ExpLand1) ~ as.character(.data$ExpLand1),
      !is.na(.data$ExpSubKont1) ~ as.character(.data$ExpSubKont1),
      !is.na(.data$ExpKont1) ~ as.character(.data$ExpKont1),
      TRUE ~ NA_character_
    )
  ) %>%
  dplyr::select(!starts_with("ExpKont"))  # Remove columns from ExpKont1 to ExpKont2
}

#' .cleanup_RIDOM
#'
#' Internal function (not exported), which cleans and prepares SurvNet data
#' for import into RIDOM according to the database scheme defined there.
#'
#' @param data Usually a tibble generated by \code{\link{import_SurvNet()}} and
#' data for Gesundheitsaemter and Landkreise appended
#'
#' @return a tibble
#'
#' @import dplyr
#' @importFrom magrittr %>%
#'
.cleanup_RIDOM <- function(data){
# Final dataset with cleaned and formatted variables
  data %>%
    add_values() %>%
    mutate(
      `Project` = recode(.data$Datensatzkategorie,
                         "EHEC" = "Produktiv_EHEC",
                         "Legionellose" = "Produktiv_Legionella_pneumophila",
                         "Listeriose" = "Produktiv_Listeria_monocytogenes",
                         "Salmonellose" = "Produktiv_Salmonella_enterica",
                         "MRSA" = "Produktiv_MRSA"),
      `Country of Isolation` = "Germany"
    ) %>%
    mutate(## everywhere but Legionella human host as default
      `Host` = ifelse(Datensatzkategorie != "Legionellose", "Homo sapiens",
                      ""),
      `Source type` = ifelse(Datensatzkategorie != "Legionellose",
                           "clinical/host-associated",
                           ""),
      `Source subtype` = ifelse(Datensatzkategorie != "Legionellose", "human",
                                ""),
      `Specimen type` = ifelse(Datensatzkategorie %in% c("Salmonellose", "EHEC"),
                               "stool",
                               "")
        ) %>%
    rename(`Collection Date` = .data$Meldedatum,
           `Host Age (years)` = .data$AlterBerechnet,
           `Host Sex` = .data$Geschlecht,
           `HospStatus` = .data$HospitalisierungStatus,
           `Outbreak` = .data$Ausbruch) %>%
    select(
      .data$Aktenzeichen,
      .data$IdRecord,
      .data$`Collection Date`, .data$Gesundheitsamt, .data$Meldelandkreis,
      .data$MunicipalityKey, .data$`Host Age (years)`, .data$`Host Sex`,
      .data$HospStatus, .data$VerstorbenStatus, .data$Expositionsort,
      .data$Outbreak, .data$`Country of Isolation`,
      .data$`Lat/Long of Isolation`, .data$Project, .data$Host,
      .data$`Source type`, .data$`Source subtype`, .data$`Specimen type`,
      .data$`Eigentuemer_ID`
    )
}

