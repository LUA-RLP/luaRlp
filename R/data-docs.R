#' Landkreise in Rheinland-Pfalz: Centroids of Latitude and Longitude
#'
#' Centroids (thier latitude and longitude) of the Landkreise in Rheinland-Pfalz
#' We might remove this dataset at some point and include code to generate the
#' centroids from geographic shapes. We should do so when we use the shapes for
#' other purposes.
#'
#' @docType data
#' @usage data(LKLatLong)
#'
#' @format A tibble with 36 rows and 2 variables:
#' \describe{
#'   \item{Meldekreis}{Name of the Landkreis (starting LK for Landkreis)}
#'   \item{Lat/Long of Isolation}{The latitude and Longitude of the Landkreis'
#'   centroid separated by a comma}
#'   }
#' @source Compiled by Thomas Stelzer and Anja Schoeps
#' @examples
#' data(LKLatLong)
#' head(LKLatLong)
"LKLatLong"




#' Landkreise und Gemeinden in Rheinland-Pfalz
#'
#' The shapes and names of Kreise and Gemeinden in Rheinland Pfalz
#'
#' @docType data
#' @usage data(RLP_geo)
#'
#' @format A list with two geo-objects (Sf - Simple feature collections):
#' \describe{
#'   \item{Kreise}{Landkreise in Rheinland-Pfalz}
#'   \item{Gemeinden}{Gemeinden in Rheinland-Pfalz}
#'   }
#' @source Compiled by Thomas Stelzer and Anja Schoeps
#' @examples
#' data(RLP_geo)
#' head(RLP_geo[["Kreise"]])
#' head(RLP_geo[["Gemeinden"]])
"RLP_geo"


#' Praxisliste
#'
#' Enhält Daten zu den teilnehmenden Arztpraxen in SURE
#'
#' @docType data
#' @usage data(Praxisliste)
#'
#' @format Eine Tabelle mit 5 Spalten, in denen zusätzliche Informationen zu den Arztpraxen liegen:
#' \describe{
#'   \item{einsender}{LIMS-Kürzel für die Praxis}
#'   \item{praxisname}{Name der Arztpraxis}
#'   \item{Kreis}{Landkreis, in dem die Arztpraxis liegt}
#'   \item{Zuordnung}{LA oder KO: Labor, in dem Proben analysiert werden}
#'   \item{Arzttyp}{Kinder oder Allgemein}
#'   }
#' @source Compiled by Anja Schoeps
#' @examples
#' data(Praxisliste)
"Praxisliste"


#' geo_standards
#'
#' Enthält eine Tabelle zur standardisierten Benennung und Verarbeitung von
#' Gebietsdaten mit Bezug zum öffentlichen Gesundheitsdienst in Rheinland-Pfalz
#'
#' @docType data
#' @usage data(geo_standards)
#'
#' @format Eine Tabelle mit 4 Spalten, in denen Informationen zu :
#' \describe{
#'   \item{Meldelandkreis}{LUA RLP interne Standard-Schreibweise der Landkreise}
#'   \item{GID_2}{Geo ID wie in der gadm Datenbank}
#'   \item{LD_ID}{Landkreis Identifyer, kompatibel mit SurvNet}
#'   \item{Gesundheitsamt}{Für den Landkreis zuständiges Gesundheitsamt}
#'   \item{Kürzel}{LUA RLP interes Standard-Kürzel für das Gesundheitsamt}
#'   \item{geom}{Polygone der Landkreis-Geometrie aus gadm}
#'   }
#' @source Compiled by Anja Schoeps with additions by Emanuel Heitlinger
#' @examples
#' data(geo_standards)
"geo_standards"


#' themes_list
#'
#' Liste der Themen, die standardmäßig aus SurvNet abgefragt werden können
#'
#' Jedem Themenbereich, der mittels \code{\link{import_SurvNet}} standardmäßig
#' abgefragt werden kann, sind die passenden SQL-Blöcke zugeordnet. Diese SQL-Blöcke
#' werden in \code{\link{build_query}} verknüpft, um die finale Abfrage zu generieren.
#'
#' Dieser Datensatz enthält alle Themen, die in \code{build_query} angegeben werden können.
#' Jeder Eintrag besteht aus einem Themenbereich und den zugehörigen SQL-Blöcken,
#' die für die Abfrage verwendet werden.
#'
#' @docType data
#' @usage data(themes_list)
#'
#' @format Eine Tabelle mit 4 Spalten, mit folgenden SQL-Blöcken :
#' \describe{
#'   \item{Thema}{Name des Themas}
#'   \item{Select}{Passender Select-Block für das jeweilige Thema}
#'   \item{From}{Passender From-Block für das jeweilige Thema}
#'   \item{Where}{Passender Where-Block für das jeweilige Thema}
#'   }
#' @source Compiled by Anja Schoeps
#' @examples
#' data(themes_list)
"themes_list"


#' IdType_Datensatzkategorie
#'
#' Eine Tabelle mit 2 Spalten, die dem Code für IdType in SurvNet die
#' korrekte Bezeichnung der Meldekategorie zuordnet
#'
#' @docType data
#' @usage data(IdType_Datensatzkategorie)
#'
#' @format Eine Tabelle mit 2 Spalten:
#' \describe{
#'   \item{IdType}{Vom RKI benutzte Kodierung (3 Ziffern)}
#'   \item{Datensatzkategorie}{ausformulierte Meldekategorie}
#'   }
#' @source Compiled by Anja Schoeps (from SurvNet)
#' @examples
#' data(IdType_Datensatzkategorie)
"IdType_Datensatzkategorie"
