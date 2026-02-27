################################################################################
########## Daten aus der SurvNet-Datenbank #####################################
################################################################################



##### Anbindung an die SurvNet-Datenbank #####
library(pacman)

pacman::p_load(odbc)

### musste ich anpassen, da mein dsn anders heißt
myconn <- dbConnect(odbc(), dsn = 'SurvNetdb')



##### Abfrage der SurvNet-Daten über SQL-Code (aus SurvNet!) #####
data <- dbGetQuery(myconn, "SELECT
[Data].[Version].[IdType] AS 'IdType__Group'
,DT1001.Week AS 'ReportingDate_Week_Group'
,DT1001.WeekYear AS 'ReportingDate_WeekYear_Group'
,[Data].[Disease71].[ReportingCounty] AS 'ReportingCounty__Group'
,[Data].[Disease71].[ReferenceDefComputed] AS 'ReferenceDefComputed__Group'
,COUNT(DISTINCT [Data].[Version].[IdRecord]) AS 'COUNT__COUNT'

FROM
[Data].[Version] INNER JOIN [Data].[Disease71] ON [Data].[Version].[IdVersion] = [Data].[Disease71].[IdVersion]
LEFT OUTER JOIN [Meta].[DayTable] DT1001 ON DT1001.IdDaySQL = CAST(CAST([Data].[Disease71].[ReportingDate] AS FLOAT) AS INT)

WHERE
(GETDATE() BETWEEN [Data].[Version].[ValidFrom] AND [Data].[Version].[ValidUntil])
AND ([Data].[Version].[IsActive] = 1)
AND ([Data].[Version].[IdRecordType] = 1)
AND (((DT1001.WeekYear IN (2020, 2021, 2022, 2023, 2024, 2025, 2026))))
AND (([Data].[Disease71].[ReportingCounty] IN (11007131, 11007132, 11007331, 11007332, 11007133, 11007231,  11007134, 11007232, 11007135, 11007333, 11007334, 11007335, 11007336, 11007339, 11007137, 11007138, 11007140, 11007141, 11007338, 11007337, 11007340, 11007235, 11007233, 11007143, 11007312, 11007111, 11007313, 11007314, 11007315, 11007316, 11007317, 11007318, 11007211, 11007319, 11007320, 11007311)))

GROUP BY
[Data].[Version].[IdType]
,DT1001.Week
,DT1001.WeekYear
,[Data].[Disease71].[ReportingCounty]
,[Data].[Disease71].[ReferenceDefComputed]
")



##### Alle möglichen Datensatzkategorien für Nullwerte hinzufügen #####
library(dplyr)

new_ids <- c(101, 102, 103, 104, 105, 106, 107, 108,	109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213)

neue_zeilen <- data.frame(
  IdType__Group = new_ids,
  ReportingDate_Week_Group = 0,
  ReportingDate_WeekYear_Group = 2026,
  ReportingCounty__Group = 0,
  ReferenceDefComputed__Group = 1,
  COUNT__COUNT = 0
)

data <- bind_rows(data, neue_zeilen)



##### Umkodierungen (dabei neue Spalten erstellen) #####
library(readxl)
#library(dplyr)

Datensatzkategorien <- read_excel("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/Hilfsdateien/Datensatzkategorien.xlsx")
data <- data %>%
  left_join(Datensatzkategorien, by = "IdType__Group")

Kreisnamen <- read_excel("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/Hilfsdateien/Kreisnamen.xlsx")
data <- data %>%
  left_join(Kreisnamen, by = "ReportingCounty__Group")

data <- data %>%
  mutate(
    Referenzdefinition = case_when(ReferenceDefComputed__Group == "1" ~ "Ja", ReferenceDefComputed__Group == "0" ~ "Nein"))



##### Löschen alter Spalten #####
data$IdType__Group <- NULL
data$ReportingCounty__Group <- NULL
data$ReferenceDefComputed__Group <- NULL



##### Umbenennungen Spaltennamen #####
colnames(data)[colnames(data) == "ReportingDate_Week_Group"] <- "Meldewoche"
colnames(data)[colnames(data) == "ReportingDate_WeekYear_Group"] <- "Meldejahr"
colnames(data)[colnames(data) == "COUNT__COUNT"] <- "Fallanzahl"



##### Umsortierung der Spalten #####
data <- data[, c("Fallanzahl", "Datensatzkategorie", "Meldewoche", "Meldejahr", "Meldelandkreis", "Referenzdefinition")]



##### Speichern in Excel-Datei #####
#library(openxlsx)

#wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
#writeData(wb, sheet = "Anleitung_Abfrage-Ergebnis", x = data, startCol = 1, startRow = 126)
#saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)






################################################################################
########## Pivots erstellen ####################################################
################################################################################



########## Pivot 2026, RefDef "Ja" ##########

#library(dplyr)
library(tidyr)

current_week <- as.integer(format(Sys.Date(), "%V"))
week_cols <- as.character(current_week:1)

#Alle Kombinationen von Datensatzkategorie und Wochen erzeugen:
all_weeks <- 1:current_week
all_categories <- unique(data$Datensatzkategorie)

#Datensatz filtern:
filtered_data <- data %>%
  filter(Referenzdefinition == "Ja", Meldejahr == 2026, Meldewoche %in% all_weeks)

#Kreuztabelle erstellen:
cross_table_2026 <- filtered_data %>%
  group_by(Datensatzkategorie, Meldewoche) %>%
  summarise(Summe = sum(Fallanzahl, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Datensatzkategorie = all_categories,
           Meldewoche = all_weeks,
           fill = list(Summe = 0)) %>%    #Fehlende Kombinationen mit 0 füllen
  mutate(Meldewoche = as.character(Meldewoche)) %>%
  pivot_wider(names_from = Meldewoche,
              values_from = Summe,
              values_fill = 0) %>%
  select(Datensatzkategorie, all_of(week_cols))

#Kumulation 1- bis letzte Meldewoche:
sum_weeks <- as.character(1:(current_week - 5))
cross_table_2026 <- cross_table_2026 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzte4MW = rowSums(across(all_of(sum_weeks))))

#Umsortierung
cross_table_2026 <- cross_table_2026 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzte4MW = rowSums(across(all_of(sum_weeks)))) %>%
  select(Insgesamt_seit_Jahresbeginn_ohneletzte4MW, everything())

#Speichern:
library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Pivots", x = cross_table_2026, startCol = 2, startRow = 6)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)



########## Pivot 2025, RefDef "Ja" ##########

#library(dplyr)
#library(tidyr)

current_week <- as.integer(format(Sys.Date(), "%V"))
week_cols <- as.character(current_week:1)

#Alle Kombinationen von Datensatzkategorie und Wochen erzeugen:
all_weeks <- 1:current_week
all_categories <- unique(data$Datensatzkategorie)

#Datensatz filtern:
filtered_data <- data %>%
  filter(Referenzdefinition == "Ja", Meldejahr == 2025, Meldewoche %in% all_weeks)

#Kreuztabelle erstellen:
cross_table_2025 <- filtered_data %>%
  group_by(Datensatzkategorie, Meldewoche) %>%
  summarise(Summe = sum(Fallanzahl, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Datensatzkategorie = all_categories,
           Meldewoche = all_weeks,
           fill = list(Summe = 0)) %>%
  mutate(Meldewoche = as.character(Meldewoche)) %>%
  pivot_wider(names_from = Meldewoche,
              values_from = Summe,
              values_fill = 0) %>%
  select(Datensatzkategorie, all_of(week_cols))

#Kumulation 1- bis letzte Meldewoche:
sum_weeks <- as.character(1:(current_week - 1))
cross_table_2025 <- cross_table_2025 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneaktuelleMW = rowSums(across(all_of(sum_weeks))))

#Umsortierung
cross_table_2025 <- cross_table_2025 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzteMW = rowSums(across(all_of(sum_weeks)))) %>%
  select(Insgesamt_seit_Jahresbeginn_ohneletzteMW, everything())

#Speichern:
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Pivots", x = cross_table_2025, startCol = 2, startRow = 236)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)



########## Pivot 2024, RefDef "Ja" ##########

#library(dplyr)
#library(tidyr)

current_week <- as.integer(format(Sys.Date(), "%V"))
week_cols <- as.character(current_week:1)

#Alle Kombinationen von Datensatzkategorie und Wochen erzeugen:
all_weeks <- 1:current_week
all_categories <- unique(data$Datensatzkategorie)

#Datensatz filtern:
filtered_data <- data %>%
  filter(Referenzdefinition == "Ja", Meldejahr == 2024, Meldewoche %in% all_weeks)

#Kreuztabelle erstellen:
cross_table_2024 <- filtered_data %>%
  group_by(Datensatzkategorie, Meldewoche) %>%
  summarise(Summe = sum(Fallanzahl, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Datensatzkategorie = all_categories,
           Meldewoche = all_weeks,
           fill = list(Summe = 0)) %>%
  mutate(Meldewoche = as.character(Meldewoche)) %>%
  pivot_wider(names_from = Meldewoche,
              values_from = Summe,
              values_fill = 0) %>%
  select(Datensatzkategorie, all_of(week_cols))

#Kumulation 1- bis letzte Meldewoche:
sum_weeks <- as.character(1:(current_week - 1))
cross_table_2024 <- cross_table_2024 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneaktuelleMW = rowSums(across(all_of(sum_weeks))))

#Umsortierung
cross_table_2024 <- cross_table_2024 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzteMW = rowSums(across(all_of(sum_weeks)))) %>%
  select(Insgesamt_seit_Jahresbeginn_ohneletzteMW, everything())

#Speichern:
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Pivots", x = cross_table_2024, startCol = 2, startRow = 352)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)



########## Pivot 2023, RefDef "Ja" ##########

#library(dplyr)
#library(tidyr)

current_week <- as.integer(format(Sys.Date(), "%V"))
week_cols <- as.character(current_week:1)

#Alle Kombinationen von Datensatzkategorie und Wochen erzeugen:
all_weeks <- 1:current_week
all_categories <- unique(data$Datensatzkategorie)

#Datensatz filtern:
filtered_data <- data %>%
  filter(Referenzdefinition == "Ja", Meldejahr == 2023, Meldewoche %in% all_weeks)

#Kreuztabelle erstellen:
cross_table_2023 <- filtered_data %>%
  group_by(Datensatzkategorie, Meldewoche) %>%
  summarise(Summe = sum(Fallanzahl, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Datensatzkategorie = all_categories,
           Meldewoche = all_weeks,
           fill = list(Summe = 0)) %>%
  mutate(Meldewoche = as.character(Meldewoche)) %>%
  pivot_wider(names_from = Meldewoche,
              values_from = Summe,
              values_fill = 0) %>%
  select(Datensatzkategorie, all_of(week_cols))

#Kumulation 1- bis letzte Meldewoche:
sum_weeks <- as.character(1:(current_week - 1))
cross_table_2023 <- cross_table_2023 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneaktuelleMW = rowSums(across(all_of(sum_weeks))))

#Umsortierung
cross_table_2023 <- cross_table_2023 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzteMW = rowSums(across(all_of(sum_weeks)))) %>%
  select(Insgesamt_seit_Jahresbeginn_ohneletzteMW, everything())

#Speichern:
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Pivots", x = cross_table_2023, startCol = 2, startRow = 468)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)



########## Pivot 2022, RefDef "Ja" ##########

#library(dplyr)
#library(tidyr)

current_week <- as.integer(format(Sys.Date(), "%V"))
week_cols <- as.character(current_week:1)

#Alle Kombinationen von Datensatzkategorie und Wochen erzeugen:
all_weeks <- 1:current_week
all_categories <- unique(data$Datensatzkategorie)

#Datensatz filtern:
filtered_data <- data %>%
  filter(Referenzdefinition == "Ja", Meldejahr == 2022, Meldewoche %in% all_weeks)

#Kreuztabelle erstellen:
cross_table_2022 <- filtered_data %>%
  group_by(Datensatzkategorie, Meldewoche) %>%
  summarise(Summe = sum(Fallanzahl, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Datensatzkategorie = all_categories,
           Meldewoche = all_weeks,
           fill = list(Summe = 0)) %>%
  mutate(Meldewoche = as.character(Meldewoche)) %>%
  pivot_wider(names_from = Meldewoche,
              values_from = Summe,
              values_fill = 0) %>%
  select(Datensatzkategorie, all_of(week_cols))

#Kumulation 1- bis letzte Meldewoche:
sum_weeks <- as.character(1:(current_week - 1))
cross_table_2022 <- cross_table_2022 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneaktuelleMW = rowSums(across(all_of(sum_weeks))))

#Umsortierung
cross_table_2022 <- cross_table_2022 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzteMW = rowSums(across(all_of(sum_weeks)))) %>%
  select(Insgesamt_seit_Jahresbeginn_ohneletzteMW, everything())

#Speichern:
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Pivots", x = cross_table_2022, startCol = 2, startRow = 584)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)



########## Pivot 2021, RefDef "Ja" ##########

#library(dplyr)
#library(tidyr)

current_week <- as.integer(format(Sys.Date(), "%V"))
week_cols <- as.character(current_week:1)

#Alle Kombinationen von Datensatzkategorie und Wochen erzeugen:
all_weeks <- 1:current_week
all_categories <- unique(data$Datensatzkategorie)

#Datensatz filtern:
filtered_data <- data %>%
  filter(Referenzdefinition == "Ja", Meldejahr == 2021, Meldewoche %in% all_weeks)

#Kreuztabelle erstellen:
cross_table_2021 <- filtered_data %>%
  group_by(Datensatzkategorie, Meldewoche) %>%
  summarise(Summe = sum(Fallanzahl, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Datensatzkategorie = all_categories,
           Meldewoche = all_weeks,
           fill = list(Summe = 0)) %>%
  mutate(Meldewoche = as.character(Meldewoche)) %>%
  pivot_wider(names_from = Meldewoche,
              values_from = Summe,
              values_fill = 0) %>%
  select(Datensatzkategorie, all_of(week_cols))

#Kumulation 1- bis letzte Meldewoche:
sum_weeks <- as.character(1:(current_week - 1))
cross_table_2021 <- cross_table_2021 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneaktuelleMW = rowSums(across(all_of(sum_weeks))))

#Umsortierung
cross_table_2021 <- cross_table_2021 %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzteMW = rowSums(across(all_of(sum_weeks)))) %>%
  select(Insgesamt_seit_Jahresbeginn_ohneletzteMW, everything())

#Speichern:
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Pivots", x = cross_table_2021, startCol = 2, startRow = 700)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)



########## Pivot 2026, RefDef "Nein" ##########

#library(dplyr)
#library(tidyr)

current_week <- as.integer(format(Sys.Date(), "%V"))
week_cols <- as.character(current_week:1)

#Alle Kombinationen von Datensatzkategorie und Wochen erzeugen:
all_weeks <- 1:current_week
all_categories <- unique(data$Datensatzkategorie)

#Datensatz filtern:
filtered_data <- data %>%
  filter(Referenzdefinition == "Nein", Meldejahr == 2026, Meldewoche %in% all_weeks)

#Kreuztabelle erstellen:
cross_table_2026_RefDefNein <- filtered_data %>%
  group_by(Datensatzkategorie, Meldewoche) %>%
  summarise(Summe = sum(Fallanzahl, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(Datensatzkategorie = all_categories,
           Meldewoche = all_weeks,
           fill = list(Summe = 0)) %>%
  mutate(Meldewoche = as.character(Meldewoche)) %>%
  pivot_wider(names_from = Meldewoche,
              values_from = Summe,
              values_fill = 0) %>%
  select(Datensatzkategorie, all_of(week_cols))

#Kumulation 1- bis letzte Meldewoche:
sum_weeks <- as.character(1:(current_week - 5))
cross_table_2026_RefDefNein <- cross_table_2026_RefDefNein %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzte4MW = rowSums(across(all_of(sum_weeks))))

#Umsortierung
cross_table_2026_RefDefNein <- cross_table_2026_RefDefNein %>%
  mutate(Insgesamt_seit_Jahresbeginn_ohneletzteMW = rowSums(across(all_of(sum_weeks)))) %>%
  select(Insgesamt_seit_Jahresbeginn_ohneletzteMW, everything())

#Speichern:
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Pivots", x = cross_table_2026_RefDefNein, startCol = 2, startRow = 120)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)






################################################################################
#################### Tabellen nach Kreisen erstellen ###########################
################################################################################



############### Anzahl Fälle seit Jahresbeginn ###############

### Filtern ###
subset(data, Referenzdefinition == "Ja" & Meldejahr == 2026)
#(Zum Anzeigen als separates Dataframe:)
data_filtered <- data[data$Referenzdefinition == "Ja" & data$Meldejahr == 2026, ]
#View(data_filtered)


### Löschen unnötiger Spalten ###
data_filtered$Referenzdefinition <- NULL
data_filtered$Meldejahr <- NULL
data_filtered$Meldewoche <- NULL


### Kreuztabelle (mit "Nuller"-Spalten) erstellen ###
#library(dplyr)
#library(tidyr)

data_filtered %>%
  group_by(Meldelandkreis, Datensatzkategorie) %>%
  summarise(Summe_Fallanzahl = sum(Fallanzahl, na.rm = TRUE)) %>%
  pivot_wider(names_from = Datensatzkategorie, values_from = Summe_Fallanzahl, values_fill = 0)

kreuztabelle <- data_filtered %>%
  group_by(Meldelandkreis, Datensatzkategorie) %>%
  summarise(Summe_Fallanzahl = sum(Fallanzahl, na.rm = TRUE), .groups = "drop") %>%
  complete(Meldelandkreis, Datensatzkategorie, fill = list(Summe_Fallanzahl = 0)) %>%
  pivot_wider(
    names_from = Datensatzkategorie,
    values_from = Summe_Fallanzahl,
    values_fill = 0
  )

### Spaltensumme in neuer Zeile "Insgesamt hinzufügen ###
#library(dplyr)

summe_zeile <- kreuztabelle %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(Meldelandkreis = "RLP") %>%
  select(Meldelandkreis, everything())

kreuztabelle <- bind_rows(kreuztabelle, summe_zeile)


### Spalte "NA" entfernen ###
kreuztabelle <- kreuztabelle %>%
  filter(
    !(is.na(Meldelandkreis)
    )
  )

### Kreuztabelle speichern in Excel-Datei ###
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Fälle u Inzidenzen nach Kreisen", x = kreuztabelle, startCol = 11, startRow = 110)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)




############### Inzidenz seit Jahresbeginn nach Kreisen ###############

### Bevölkerungszahlen einlesen und Inzidenzen berechnen ###
#library(readxl)
#library(dplyr)

Bevölkerungszahlen <- read_excel("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/Hilfsdateien/Bevölkerung2024.xlsx", sheet = "Bev. Kreisebene")

kreuztabelle1 <- kreuztabelle %>%
  left_join(Bevölkerungszahlen %>% select(Meldelandkreis, Insgesamt), by = "Meldelandkreis")

kreuztabelle_neu <- kreuztabelle1

spalten_zum_umrechnen <- setdiff(colnames(kreuztabelle1), c("Meldelandkreis", "Insgesamt"))

kreuztabelle_neu[ , spalten_zum_umrechnen] <- sweep(
  kreuztabelle1[ , spalten_zum_umrechnen],
  1,
  kreuztabelle1$Insgesamt,
  FUN = "/"
) * 100000

kreuztabelle_neu[ , spalten_zum_umrechnen] <- round(kreuztabelle_neu[ , spalten_zum_umrechnen], 1)


### Kreuztabelle speichern in Excel-Datei ###
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Fälle u Inzidenzen nach Kreisen", x = kreuztabelle_neu, startCol = 11, startRow = 152)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)




############### Anzahl Fälle der aktuellen und letzten 4 MW ###############

# Heutiges Datum (für aktuelle Kalenderwoche)
heute <- Sys.Date()

# Kalenderwoche und Jahr aus dem heutigen Datum berechnen
aktuelle_woche <- as.integer(strftime(heute, format = "%V"))
aktuelles_jahr <- as.integer(format(heute, "%Y"))

# Zum sicherstellen, dass es auf das Jahr 2025 begrenzt wird:
# aktuelles_jahr <- 2025

# Kalenderwochenbereich: aktuelle Woche und die 4 davor
wochenbereich <- (aktuelle_woche - 4):aktuelle_woche

# Falls der Bereich < 1 geht (z. B. in Woche 2), dann bei 1 begrenzen
wochenbereich <- wochenbereich[wochenbereich >= 1]

# Dataframe filtern
gefiltert <- subset(
  data,
  Referenzdefinition == "Ja" &
    Meldejahr == aktuelles_jahr &
    Meldewoche %in% wochenbereich | Meldewoche == 0
)


### Kreuztabelle (mit "Nuller"-Spalten) erstellen ###
#library(dplyr)
#library(tidyr)

gefiltert %>%
  group_by(Meldelandkreis, Datensatzkategorie) %>%
  summarise(Summe_Fallanzahl = sum(Fallanzahl, na.rm = TRUE)) %>%
  pivot_wider(names_from = Datensatzkategorie, values_from = Summe_Fallanzahl, values_fill = 0)

kreuztabelle_4MW <- gefiltert %>%
  group_by(Meldelandkreis, Datensatzkategorie) %>%
  summarise(Summe_Fallanzahl = sum(Fallanzahl, na.rm = TRUE), .groups = "drop") %>%
  complete(Meldelandkreis, Datensatzkategorie, fill = list(Summe_Fallanzahl = 0)) %>%
  pivot_wider(
    names_from = Datensatzkategorie,
    values_from = Summe_Fallanzahl,
    values_fill = 0
  )


### Spaltensumme in neuer Zeile "Insgesamt hinzufügen ###
#library(dplyr)

summe_zeile <- kreuztabelle_4MW %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(Meldelandkreis = "RLP") %>%
  select(Meldelandkreis, everything())

kreuztabelle_4MW <- bind_rows(kreuztabelle_4MW, summe_zeile)


### Spalte "NA" entfernen ###
kreuztabelle_4MW <- kreuztabelle_4MW %>%
  filter(
    !(is.na(Meldelandkreis)
    )
  )


## Kreuztabelle speichern in Excel-Datei ##
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Fälle u Inzidenzen nach Kreisen", x = kreuztabelle_4MW, startCol = 11, startRow = 10)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)




############### Inzidenz aktuelle und letzte 4MW nach Kreisen ###############

### Bevölkerungszahlen einlesen und Inzidenzen berechnen ###
#library(readxl)
#library(dplyr)

Bevölkerungszahlen <- read_excel("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/Hilfsdateien/Bevölkerung2024.xlsx", sheet = "Bev. Kreisebene")

kreuztabelle2 <- kreuztabelle_4MW %>%
  left_join(Bevölkerungszahlen %>% select(Meldelandkreis, Insgesamt), by = "Meldelandkreis")

kreuztabelle_neu2 <- kreuztabelle2

spalten_zum_umrechnen <- setdiff(colnames(kreuztabelle2), c("Meldelandkreis", "Insgesamt"))

kreuztabelle_neu2[ , spalten_zum_umrechnen] <- sweep(
  kreuztabelle2[ , spalten_zum_umrechnen],
  1,
  kreuztabelle2$Insgesamt,
  FUN = "/"
) * 100000

kreuztabelle_neu2[ , spalten_zum_umrechnen] <- round(kreuztabelle_neu2[ , spalten_zum_umrechnen], 1)


### Kreuztabelle speichern in Excel-Datei ###
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Fälle u Inzidenzen nach Kreisen", x = kreuztabelle_neu2, startCol = 11, startRow = 52)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)





################################################################################
############################# Ausbrüche ########################################
################################################################################



########## Generelle Anbindung an die SurvNet-Datenbank ########################
#library(pacman)

pacman::p_load(odbc)


## musste ich ändern, da meine dsn anders heißt
myconn <- dbConnect(odbc(), dsn = 'SurvNetdb')
#DECLARE @QueryParam_0 INT
#SET @QueryParam_0 = 2


########## Abfrage der SurvNet-Daten über SQL-Code (aus SurvNet) ###############
data <- dbGetQuery(myconn, "SELECT DISTINCT
[Data].[Version].[IdVersion] AS 'IdVersion_'
,[Data].[Version].[GuidRecord] AS 'GuidRecord_'
,[Data].[Version].[IdRecord] AS 'IdRecord_'
,[Data].[Version].[VersionNo] AS 'VersionNo_'
,[Data].[Version].[ValidFrom] AS 'ValidFrom_'
,[Data].[Version].[ValidUntil] AS 'ValidUntil_'
,[Data].[Version].[IsCurrent] AS 'IsCurrent_'
,[Data].[Version].[IdRecordType] AS 'IdRecordType_'
,[Data].[Version].[IdType] AS 'IdType_'
,[Data].[Version].[IdSchema] AS 'IdSchema_'
,[Data].[Version].[VersionNoSender] AS 'VersionNoSender_'
,[Data].[Version].[CodeRecordOwner] AS 'CodeRecordOwner_'
,[Data].[Version].[CodeVersionEditor] AS 'CodeVersionEditor_'
,[Data].[Version].[Token] AS 'Token_'
,[Data].[Version].[GuidAccountEditor] AS 'GuidAccountEditor_'
,[Data].[Version].[IsActive] AS 'IsActive_'
,[Data].[Version].[StatusTransportImplicit] AS 'StatusTransportImplicit_'
,[Data].[Version].[StatusTransportExplicit] AS 'StatusTransportExplicit_'
,[Data].[Version].[StatusProcessing] AS 'StatusProcessing_'
,[Data].[Version].[StatusEvaluator] AS 'StatusEvaluator_'
,[Data].[Version].[ToTransport] AS 'ToTransport_'
,[Data].[Version].[ToTransportManual] AS 'ToTransportManual_'
,[Data].[Version].[ChangedAt] AS 'ChangedAt_'
,[Data].[Version].[GuidAccountAppropriate] AS 'GuidAccountAppropriate_'
,[Data].[Version].[StatusActive] AS 'StatusActive_'
,(SELECT COUNT(*) FROM [Data].[ContactLog] WITH (FORCESEEK, INDEX(IX_IdVersion)) WHERE [Data].[ContactLog].IdVersion = [Data].[Version].[IdVersion]) AS 'ContactLogs_COUNT'
,(SELECT COUNT(*) FROM [Data].[AdditionalAttribute] WITH (FORCESEEK, INDEX(IX_IdVersion)) WHERE [Data].[AdditionalAttribute].IdVersion = [Data].[Version].[IdVersion]) AS 'AdditionalAttributes_COUNT'
,[Data].[Version].[StatusExposition] AS 'StatusExposition_'
,[Data].[Outbreak].[OutbreakCategoryComputed] AS 'OutbreakCategoryComputed_'
,[Data].[Outbreak].[NameGA] AS 'NameGA_'
,[Data].[Outbreak].[NameLS] AS 'NameLS_'
,[Data].[Outbreak].[NameRKI] AS 'NameRKI_'
,[Data].[Outbreak].[SettingOutbreak] AS 'SettingOutbreak_'
,[Data].[Outbreak].[IsNonLocal] AS 'IsNonLocal_'
,[Data].[Outbreak].[GuidSetting] AS 'GuidSetting_'
,(SELECT COUNT(*) FROM [Data].[OutbreakDisease] WITH (FORCESEEK, INDEX(IX_IdVersion)) WHERE [Data].[OutbreakDisease].IdVersion = [Data].[Version].[IdVersion]) AS 'OutbreakDiseases_COUNT'
,(SELECT COUNT(*) FROM [Data].[OutbreakSuboutbreak] WITH (FORCESEEK, INDEX(IX_IdVersion)) WHERE [Data].[OutbreakSuboutbreak].IdVersion = [Data].[Version].[IdVersion]) AS 'OutbreakSuboutbreaks_COUNT'
,[Data].[Outbreak].[P112Relevant] AS 'P112Relevant_'
,[Data].[Outbreak].[P112Source] AS 'P112Source_'
,[Data].[Outbreak].[InternalName] AS 'InternalName_'

FROM
[Data].[Version] INNER JOIN [Data].[Outbreak] ON [Data].[Version].[IdVersion] = [Data].[Outbreak].[IdVersion]
LEFT OUTER JOIN [Data].[OutbreakDisease] ON [Data].[Version].[IdVersion] = [Data].[OutbreakDisease].[IdVersion]

WHERE
(GETDATE() BETWEEN [Data].[Version].[ValidFrom] AND [Data].[Version].[ValidUntil])
AND ([Data].[Version].[IsActive] = 1)
AND ([Data].[Version].[IdType] = 20)
AND ([Data].[Version].[IdVersion] IN (SELECT [IdVersionOutbreak] FROM [Data].[GetOutbreakDiseases] ([Data].[Version].[IdVersion], GETDATE()) WHERE [GuidDisease] IN (SELECT DISTINCT [Data].[Version].[GuidRecord] AS 'GuidRecord_' FROM [Data].[Version] INNER JOIN [Data].[Disease71] ON [Data].[Version].[IdVersion] = [Data].[Disease71].[IdVersion]
                                                                                                                                                                     LEFT OUTER JOIN [Meta].[DayTable] DT1001 ON DT1001.IdDaySQL = CAST(CAST([Data].[Disease71].[ReportingDate] AS FLOAT) AS INT)
                                                                                                                                                                     LEFT OUTER JOIN [Meta].[DayTable] DT1110 ON DT1110.IdDaySQL = CAST(CAST([Data].[Disease71].[OnsetOfDisease] AS FLOAT) AS INT)
                                                                                                                                                                     LEFT OUTER JOIN [Meta].[DayTable] DT1122 ON DT1122.IdDaySQL = CAST(CAST([Data].[Disease71].[BirthdayComputed] AS FLOAT) AS INT)
                                                                                                                                                                     LEFT OUTER JOIN [Query].[vAgeGroupingData] AG1123 ON ( AG1123.[IdAgeGrouping] = 2 ) AND ( [Data].[Disease71].[AgeComputed] BETWEEN AG1123.[AgeFrom] AND AG1123.[AgeUntil] )
                                                                                                                                                                     WHERE ((GETDATE() BETWEEN [Data].[Version].[ValidFrom] AND [Data].[Version].[ValidUntil])
                                                                                                                                                                            AND ([Data].[Version].[IsActive] = 1)
                                                                                                                                                                            AND ([Data].[Version].[IdRecordType] = 1)
                                                                                                                                                                            AND (
    (DT1001.WeekYear = DATEPART(YEAR, GETDATE()) AND DT1001.Week = DATEPART(ISO_WEEK, GETDATE()))
    OR
    (
        -- vorherige Woche: Wenn aktuelle Woche > 1, dann gleiche Jahr - sonst Jahreswechsel
        (
            DATEPART(ISO_WEEK, GETDATE()) > 1
            AND DT1001.WeekYear = DATEPART(YEAR, GETDATE())
            AND DT1001.Week = DATEPART(ISO_WEEK, GETDATE()) - 1
        )
        OR
        (
            DATEPART(ISO_WEEK, GETDATE()) = 1
            AND DT1001.WeekYear = DATEPART(YEAR, GETDATE()) - 1
            AND DT1001.Week = (SELECT MAX(Week) FROM [Meta].[DayTable] WHERE WeekYear = DATEPART(YEAR, GETDATE()) - 1)
        )
    )
)
                                                                                                                                                                     ))))
                                                                                                                                                                     ")



##### Umkodierungen (dabei z.T. neue Spalten erstellen) #####
#library(readxl)
#library(dplyr)

data$GueltigAb <- format(as.Date(data$ValidFrom_, format="%Y-%m-%d %H:%M:%S"), "%d.%m.%Y")


Datensatzkategorien <- read_excel("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/Hilfsdateien/Eigentuemer.xlsx")
data <- data %>%
  left_join(Datensatzkategorien, by = "CodeRecordOwner_")


Datensatzkategorien <- read_excel("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/Hilfsdateien/Ausbruchskategorie.xlsx")
data <- data %>%
  left_join(Datensatzkategorien, by = "OutbreakCategoryComputed_")


Datensatzkategorien <- read_excel("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/Hilfsdateien/Infektionsumfeld.xlsx")
data <- data %>%
  left_join(Datensatzkategorien, by = "SettingOutbreak_")


data <- data %>%
  mutate(
    P112Relevant_ = case_when(P112Relevant_ == "1" ~ "Ja", P112Relevant_ == "0" ~ "Nein"))



##### Umbenennungen Spaltennamen #####
colnames(data)[colnames(data) == "OutbreakDiseases_COUNT"] <- "AusbruchFall_Anzahl"
colnames(data)[colnames(data) == "Token_"] <- "Aktenzeichen"
colnames(data)[colnames(data) == "NameGA_"] <- "Name im GA"



##### Irrelevante Spalten löschen #####
#library(dplyr)
data <- data %>%
  select(GueltigAb, Eigentuemer, Ausbruchskategorie, Infektionsumfeld,
         P112Relevant_, AusbruchFall_Anzahl, Aktenzeichen, "Name im GA")


##### Sortieren #####
#library(dplyr)

data <- data %>%
  arrange(Ausbruchskategorie, Infektionsumfeld, desc(GueltigAb))


##### Neue Spalte für "1" oder "0" bei jeder Ausbruchskategorie (zur Einfäbrbung in Excel)
kategorien <- unique(data$Ausbruchskategorie)
zuordnung <- data.frame(
  Ausbruchskategorie = kategorien,
  Wechselwert = rep(c(0, 1), length.out = length(kategorien))
)
#library(dplyr)
data <- left_join(data, zuordnung, by = "Ausbruchskategorie")



##### Leerzeilen zum Überschreiben hinzufügen #####
n_leer <- 100

leere_zeilen <- data.frame(matrix(NA, nrow = n_leer, ncol = ncol(data)))
colnames(leere_zeilen) <- colnames(data)

data <- rbind(data, leere_zeilen)



##### Speichern #####
#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Ausbrüche", x = data, startCol = 1, startRow = 5)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)


############## Stand (Datum/Uhrzeit) angeben####################################

stand_text <- paste0("(Stand: ", format(Sys.time(), "%d.%m.%y, %H:%M Uhr"), ")")

#library(openxlsx)
wb <- loadWorkbook("Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx")
writeData(wb, sheet = "Fälle nach Meldewoche", x = stand_text, startCol = 1, startRow = 2)
saveWorkbook(wb, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Erstellen/KWXX.xlsx", overwrite = TRUE)










################################################################################
##### Interaktive Karte erstellen: Inzidenzen - Aktuelle und letzte 4 MW #######
################################################################################




library(sf)
library(readxl)
library(dplyr)



################################################################################
### Einlesen und verknüpfen der Geometriedaten mit den EpiMeeting-Statistiken ##
################################################################################

### Ich kommentiere alles was du nicht (mehr; ich habe ein windig im Paket
### angepasst) brauchst.


### Einlesen "gadm41_DEU.gpkg"
####  gpkg_path <- "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/Geodata/gadm41_DEU.gpkg"
### Kreislayer auswählen:
### geodata_Kreise <- st_read(gpkg_path, layer="ADM_ADM_2")

#Umkodierung:
### RLP_Kreise <- geodata_Kreise[geodata_Kreise$NAME_1 == "Rheinland-Pfalz", ]


### Dies kann man ersetzen durch Daten, die wir zentral im Paket halten
library(luaRlp)
library(sf)

data("geo_standards")
data("RLP_geo")

## hier wären die kompletten gadm daten (für Rheinland-PfalZ):
RLP_geo

### die Geometrien (polygone) aus gadm sind jetzt aber auch für die Kreise in
geo_standards


## Das spart man sich dadurch:
#
# colnames(RLP_Kreise)[colnames(RLP_Kreise) == "NAME_2"] <- "KREISE"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Ahrweiler"] <- "LK Ahrweiler"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Altenkirchen (Westerwald)"] <- "LK Altenkirchen"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Alzey-Worms"] <- "LK Alzey-Worms"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Bad Dürkheim"] <- "LK Bad Dürkheim"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Bad Kreuznach"] <- "LK Bad Kreuznach"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Bernkastel-Wittlich"] <- "LK Bernkastel-Wittlich"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Birkenfeld"] <- "LK Birkenfeld"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Cochem-Zell"] <- "LK Cochem-Zell"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Donnersbergkreis"] <- "LK Donnersbergkreis"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Eifelkreis Bitburg-Prüm"] <- "LK Bitburg-Prüm"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Frankenthal (Pfalz)"] <- "SK Frankenthal"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Germersheim"] <- "LK Germersheim"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Kaiserslautern"] <- "LK Kaiserslautern"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Kaiserslautern (Kreisfreie Stadt"] <- "SK Kaiserslautern"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Koblenz"] <- "SK Koblenz"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Mayen-Koblenz"] <- "LK Mayen-Koblenz"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Kusel"] <- "LK Kusel"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Landau in der Pfalz"] <- "SK Landau i.d.Pfalz"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Ludwigshafen am Rhein"] <- "SK Ludwigshafen"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Bernkastel-Wittlich"] <- "LK Bernkastel-Wittlich"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Mainz"] <- "SK Mainz"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Mainz-Bingen"] <- "LK Mainz-Bingen"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Neustadt an der Weinstraße"] <- "SK Neustadt a.d.Weinstraße"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Neuwied"] <- "LK Neuwied"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Pirmasens"] <- "SK Pirmasens"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Rhein-Hunsrück-Kreis"] <- "LK Rhein-Hunsrück-Kreis"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Rhein-Lahn-Kreis"] <- "LK Rhein-Lahn-Kreis"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Rhein-Pfalz-Kreis"] <- "LK Rhein-Pfalz-Kreis"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Speyer"] <- "SK Speyer"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Südliche Weinstraße"] <- "LK Südliche Weinstraße"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Südwestpfalz"] <- "LK Südwestpfalz"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Trier"] <- "SK Trier"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Trier-Saarburg"] <- "LK Trier-Saarburg"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Vulkaneifel"] <- "LK Vulkaneifel"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Westerwaldkreis"] <- "LK Westerwaldkreis"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Worms"] <- "SK Worms"
# RLP_Kreise$KREISE[RLP_Kreise$KREISE == "Zweibrücken"] <- "SK Zweibrücken"
#
#
# ### und statt dem hier
#
# #Verknüpfung der Tabelle mit der InzMW4-Tabelle über den Schüssel "Kreisnamen"
#  rlp_landkreise_data <- RLP_Kreise %>%
#    left_join(kreuztabelle_neu2, by = c("KREISE" = "Meldelandkreis"))

## Einfach

rlp_landkreise_data <- geo_standards %>%
 left_join(kreuztabelle_neu2, by = c("Meldelandkreis")) ## Meldlandkreis könnte man sogar weglassen da Namen gleich


### Dann weiter wie bisher... um einen Vereinfachung der SQL Abfrage und um eine
## Standardisierung unserer Krankheiten im Paket kümmern wir uns dann später

library(leaflet)
library(htmlwidgets)

krankheiten <- c(
  "Acinetobacter","Adenovirus","Adenovirus, Länderverordnung",
  "Amoebiasis","Arbovirus","Astrovirus","aviäre Influenza",
  "Bornavirus","Botulismus","Brucellose","Campylobacter",
  "Candida auris","Chikungunya","Chlamydia-trachomatis",
  "Cholera","CJK","Clostridioides difficile","COVID-19",
  "Cytomegalie","Denguefieber","Diphtherie","E.-coli-Enteritis",
  "Ebolafieber","Echinokokkose","EHEC/STEC","Enterobacterales",
  "Enterovirus","Fleckfieber","FSME","Gasbrand","Gelbfieber",
  "Giardiasis","Gonorrhoe","Gruppe-B-Streptokokken",
  "Haemophilus influenzae","Hand-Fuß-Mund-Krankheit",
  "Hantavirus","Hepatitis (allgemein)","Hepatitis A","Hepatitis B",
  "Hepatitis C","Hepatitis D","Hepatitis E","Hepatitis Non A-E",
  "HIV-Infektion","HUS","Influenza","Keuchhusten","Kopfläuse",
  "Krätzmilben","Kryptosporidiose","Lassafieber",
  "Läuserückfallfieber","Legionellose","Lepra","Leptospirose",
  "Listeriose","Lyme-Borreliose","Malaria",
  "Malaria, Länderverordnung","Marburgfieber","Masern",
  "Meningoenzephalitis, andere","Meningokokken","Milzbrand",
  "Mpox","MRSA","Mumps","Mycoplasma","Norovirus","Ornithose",
  "Orthopocken (andere)","Parainfluenza","Paratyphus","Pest",
  "Pneumokokken","Pocken","Poliomyelitis","Q-Fieber",
  "Respiratorisches-Synzytial-Virus","Ringelröteln","Rotavirus",
  "Röteln, konnatal","Röteln, konnatale Infektion",
  "Röteln, postnatal","Salmonellose","SARS","Scharlach",
  "Shigellose","SSPE","Syphilis","Tetanus","Tollwut",
  "Tollwutexpositionsverdacht","Toxoplasmose, konnatal",
  "Toxoplasmose, postnatal","Trichinellose","Tuberkulose",
  "Tularämie","Typhus","Typhus/Paratyphus","vCJK",
  "VHF, andere Erreger","Vibrionen","Weitere bedrohliche",
  "Weitere bedrohliche (gastro)","West-Nil","Windpocken",
  "Yersiniose","Zika"
)


karte <- leaflet(
  rlp_landkreise_data,
  options = leafletOptions(
    minZoom = 7,
    maxZoom = 9
  )
) %>%
  addTiles() %>%
  fitBounds(
    lng1 = min(st_coordinates(st_geometry(rlp_landkreise_data))[,1]),
    lat1 = min(st_coordinates(st_geometry(rlp_landkreise_data))[,2]),
    lng2 = max(st_coordinates(st_geometry(rlp_landkreise_data))[,1]),
    lat2 = max(st_coordinates(st_geometry(rlp_landkreise_data))[,2])
  )

paletten <- list()

for (k in krankheiten) {

  # Prüfen, ob der Layer nur Nullwerte hat
  if (all(rlp_landkreise_data[[k]] == 0 | is.na(rlp_landkreise_data[[k]]))) {
    fill_color_fun <- function(x) "#f0f8ff"
    paletten[[k]] <- NULL
  } else {
    # Normale Palette für Layer mit Werten >0
    paletten[[k]] <- colorBin(
      palette = "Blues",
      domain  = rlp_landkreise_data[[k]],
      bins    = 6,
      pretty  = TRUE,
      na.color = "white"
    )
    fill_color_fun <- function(x) paletten[[k]](x)
  }

  # Layer hinzufügen
  karte <- karte %>%
    addPolygons(
      fillColor   = ~fill_color_fun(get(k)),
      fillOpacity = 0.8,
      color       = "black",
      weight      = 1,
      group       = k,
      popup       = ~paste(
        "Meldelandkreis:", Meldelandkreis,
        "<br>", "Inzidenz:", get(k)
      )
    )

  # Legende nur für Layer mit Werten >0
  if (!is.null(paletten[[k]])) {
    karte <- karte %>%
      addLegend(
        position = "bottomleft",
        pal      = paletten[[k]],
        values   = rlp_landkreise_data[[k]],
        title    = paste0(k, "-Inzidenz_aktuelle und letzte 4 MW"),
        group    = k
      )
  }
}

# Layer-Control
karte <- karte %>%
  addLayersControl(
    overlayGroups = krankheiten,
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(krankheiten)

# Speichern
saveWidget(karte, "Z:/DFS-LUA-LD-Zusammenarbeit/LD-AB32.5_IfSG_Meldewesen/EPI_Meetings/Statistiken/Karte/interaktive_Karte.html", selfcontained = FALSE)









