
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
  SN <-
    .add_Sero_Erreger(SurvNet) |>
    add_values() |>
    mutate(Aktenzeichen = gsub(" ", "", paste0(Kürzel, " - ",
                                               Aktenzeichen)))|>
    .cleanup_RIDOM()
  IDs <- read.delim(LIMS_link_file, sep = ";")
  Out_EpiDaten <- .merge_lims_survnet(lims = IDs, survnet = SN)
  ## write output if out is given
  if(!is.null(out)){
    write_xlsx(x = Out_EpiDaten, path = out)
  }
  ### optional reporting of a problems-file
  if(!is.null(problems)){
    .write_problem_report(problems = problems,
                          type_of_problem = type_of_problem,
                          lims = IDs,
                          survnet = SN)
  }
  return(Out_EpiDaten)
}


#' geo2ridom
#'
#' @param x a POINT geometry list from sf
#'
#' @returns a vector of RIDOM compatible geo-coordinate point
geo2ridom <- function(x, digits = 4) {
  coords <- sf::st_coordinates(x)
  fmt <- paste0("%+.", digits, "f")
  sprintf(paste(fmt, fmt, sep = ", "),
          coords[, 2], coords[, 1])
}

#' write_problem_report
#'
#' @param problems Ein Pfad zu einer (csv)-Datei in der problematische "Aktenzeichen" gegeben werden
#'
#' @returns schreibt ein csv mit "Aktenzeichen", aus wenn ein gültiger Pfad gegeben wird
#'
.write_problem_report <- function(problems, type_of_problem, lims, survnet){
  if(type_of_problem %in%(c("non-matching-LIMS"))){
    p <- lims |>
      filter(!Survnet %in% survnet$Aktenzeichen)
    write_csv2(p, problems)
  }else{
    stop("Choose a type of problem to report when requesting problems output")
  }
}

.merge_lims_survnet <- function(lims, survnet){
  inner_join(lims, survnet,
             by = c("Survnet" = "Aktenzeichen")) |>
    ## rename variables coming from the LIMS merger!
    rename(`Sample ID` = Laborprobennummer,
           `Alias ID (s)` =Survnet,
           `Zip of Isolation` = Postleitzahl)
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
#'
.cleanup_RIDOM <- function(data){
# Final dataset with cleaned and formatted variables
  data |>
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
    ) |>
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
        ) |>
    rename(`Collection Date` = Meldedatum,
           `Host Age (years)` = AlterBerechnet,
           `Host Sex` = Geschlecht,
           `HospStatus` = HospitalisierungStatus,
           `Outbreak` = Ausbruch,
           `Gesundheitsamt` = Wohnort_GA) |>
    select(
      Aktenzeichen,
      IdRecord,
      `Collection Date`, Gesundheitsamt, Meldelandkreis,
      MunicipalityKey, `Host Age (years)`, `Host Sex`,
      HospStatus, VerstorbenStatus, Expositionsort,
      Outbreak, `Country of Isolation`,
      `Lat/Long of Isolation`, Project, Host,
      `Source type`, `Source subtype`, `Specimen type`,
      `Eigentuemer_ID`, `Datensatzkategorie`, `Erreger`
    )
}

#' .add_Sero
#'
#' Füge Serovar für Salmonellen einer SurvNet-Abfrage hinzu
#'
#' @param SN die ursprünliche SurvNet Abfrage (Rohform, ohne add_values)
#'
#' @returns Ein kombinierte SuvNet Abfrage, die z.B. mit add_values() weiter be
#' arbeitet weren kann
#'
.add_Sero <- function(SN){
  ## Daten für serovar importieren
  SAL <- import_SurvNet(build_query(Thema="SerovarSAL")) |>
    rename(Pathogen_ID_SAL = Pathogen_ID)
  left_join(SN, SAL)
}

#' .add_Erreger
#'
#' @param SN die ursprünliche SurvNet Abfrage (Rohform, ohne add_values)
#'
#' Füge Erreger-Art für Enterobacterales einer SurvNet-Abfrage hinzu
#'
#' @returns Ein kombinierte SuvNet Abfrage, die z.B. mit add_values() weiter be
#' arbeitet weren kann
#'
.add_Erreger <- function(SN){
  #Daten für Erreger bei Enterobacterales importiere
  EBC <- import_SurvNet(build_query(Thema="EBC")) |>
    rename(Pathogen_ID_EBC = Pathogen_ID)
  left_join(SN, EBC)
}

#' .add_Sero_Erreger
#'
#' Füge Serovar (für Salmonellen) und Erreger-Art (für Enterobacterales) einer
#' SurvNet-Abfrage hinzu. Die Informationen werden in einer einzigen Spalte
#' "Pathogen_ID" gesammelt.
#'
#' @param SN die ursprünliche SurvNet Abfrage (Rohform, ohne add_values)
#'
#' @returns Ein kombinierte SuvNet Abfrage, die z.B. mit add_values() weiter be
#' arbeitet weren kann
#'
.add_Sero_Erreger <-function(SN){
  SN |>
    .add_Sero() |>
    .add_Erreger() |>
    mutate(
      Pathogen_ID = coalesce(Pathogen_ID_SAL, Pathogen_ID_EBC)
    ) |>
    select(-Pathogen_ID_SAL, -Pathogen_ID_EBC)
}


