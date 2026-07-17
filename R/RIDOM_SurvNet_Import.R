
#' createEpidata
#'
#' @param LIMS_link_file The file (exported from our LIMS) associating our
#' LUA Probennummern (3263-) and SurvNet IDs (Aktenzeichen with licence plate
#' prefix), defaults to a path on our System on which automated LIMS exports are
#' stored
#'
#' @param out The output file in xlsx format, defaults to NULL, meaning no
#' output is written, but the object can be written to a file by the user
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
#'   from SurvNet (see ?luaRlp:::.combine_exposure) in add_values()}
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
    out = NULL,
    SurvNet = import_SurvNet(build_query(Thema = "IMS"), ...),
    problems = NULL,
    type_of_problem = "non-matching-LIMS", ...
){
  SN <- add_values(SurvNet) %>%
    mutate(Aktenzeichen = gsub(" ", "", paste0(.data$Kürzel, " - ",
                                               .data$Aktenzeichen)))%>%
    .cleanup_RIDOM()
  IDs <- read.delim(LIMS_link_file, sep = ";")
  Out_EpiDaten <- inner_join(IDs, SN,
                             by = c("Survnet" = "Aktenzeichen")) %>%
    ## rename variables coming from the LIMS merger!
    rename(`Sample ID` = .data$Laborprobennummer,
           `Alias ID (s)` =.data$Survnet,
           `Zip of Isolation` = .data$Postleitzahl)
  if(!is.null(out)){
    write_xlsx(Out_EpiDaten, out)
  }
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
    mutate(
      `Project` = recode(.data$IdType,
                         "121" = "Produktiv_EHEC",
                         "135" = "Produktiv_EHEC",
                         "138" = "Produktiv_Legionella_pneumophila",
                         "140" = "Produktiv_Listeria_monocytogenes",
                         "157" = "Produktiv_Salmonella_enterica",
                         "179" = "Produktiv_MRSA", .default="XX_Problem_XX"),
      `Country of Isolation` = "Germany",
      `Lat/Long of Isolation` = geo2ridom(.data$centroid)
    ) %>%
    mutate(## everywhere but Legionella human host as default
      `Host` = ifelse(IdType != "138", "Homo sapiens", ## Legionellose
                      ""),
      `Source type` = ifelse(IdType != "138", ## Legionellose kann Wasser sein
                           "clinical/host-associated",
                           ""),
      `Source subtype` = ifelse(IdType != "138", "human",
                                ""),
      `Specimen type` = ifelse(IdType %in% c("157", "121", "135"),
                               "stool",
                               "")
        ) %>%
    rename(`Collection Date` = .data$Meldedatum,
           `Host Age (years)` = .data$AlterBerechnet,
           `Host Sex` = .data$Geschlecht,
           `HospStatus` = .data$HospitalisierungStatus,
           `Outbreak` = .data$Ausbruch,
           `Gesundheitsamt` = .data$Wohnort_GA) %>%
    select(
      .data$Aktenzeichen,
      .data$IdRecord,
      .data$`Collection Date`, .data$Gesundheitsamt, .data$Meldelandkreis,
      .data$MunicipalityKey, .data$`Host Age (years)`, .data$`Host Sex`,
      .data$HospStatus, .data$VerstorbenStatus, .data$Expositionsort,
      .data$Outbreak, .data$`Country of Isolation`,
      .data$`Lat/Long of Isolation`, .data$Project, .data$Host,
      .data$`Source type`, .data$`Source subtype`, .data$`Specimen type`,
      .data$`Eigentuemer_ID`, .data$`Datensatzkategorie`
    )
}

