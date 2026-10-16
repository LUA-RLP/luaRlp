## input
file_PnNK <- "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/06_WGS/Proben_nach_Nummernkreis.xlsx"
file_LIMS <- "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/09_Kryokonservierung/Seq_Legio_ERMITTLUNG.xlsx"

file_out <- "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/14_EpiDaten/Legio_text.xlsx"

LIMS_3222 <- read_xlsx(file_LIMS, sheet = "LIMS_Export_3222")

library(excel.link)

# Read the password-protected sheet directly
PnNK <- xl.read.file(
  filename = file_PnNK,
  password = "IMS",
  xl.sheet = "Autom."
)


table(PnNK$`Interne-Probennr.`%in%LIMS_3222$Labornummer)

wanted.cols <- c("Seq-Nr", "Interne-Probennr.", "Survnet-Aktenzeichen",
                 "Art der Einsendung",
                 "Name-\n(Art/Spezies)", "Top-Species-Match",
                 "Source-Subtype", "Source-Type"
)


foo <- merge(PnNK[, wanted.cols], LIMS_3222,
             by.x = "Interne-Probennr.",
             by.y = "Labornummer",
             all.x =TRUE)

colnames(foo) <- make.names(colnames(foo))

table()

foo <- subset(foo, foo$Name...Art.Spezies.%in%"Legionella"&
                grepl("\\d{4}-\\d{4}-.*", foo$Seq.Nr))


write_xlsx(foo, file_out)
