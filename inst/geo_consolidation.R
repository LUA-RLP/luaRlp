library(tidyr)
library(luaRlp)

data("geo_standards")
data("RLP_geo")

names(RLP_geo)

## enlich mal korrigieren
names(RLP_geo) <- c("Land", "Kreise", "Gemeinden1", "Gemeinden2")

save(RLP_geo, file="RLP_geo.rda")


n2n <- c("Ahrweiler", "LK Ahrweiler",
         "Altenkirchen (Westerwald)", "LK Altenkirchen",
         "Alzey-Worms", "LK Alzey-Worms",
         "Bad Dürkheim", "LK Bad Dürkheim",
         "Bad Kreuznach", "LK Bad Kreuznach",
         "Bernkastel-Wittlich", "LK Bernkastel-Wittlich",
         "Birkenfeld", "LK Birkenfeld",
         "Cochem-Zell", "LK Cochem-Zell",
         "Donnersbergkreis", "LK Donnersbergkreis",
         "Eifelkreis Bitburg-Prüm", "LK Bitburg-Prüm",
         "Frankenthal (Pfalz)", "SK Frankenthal",
         "Germersheim", "LK Germersheim",
         "Kaiserslautern", "LK Kaiserslautern",
         "Kaiserslautern (Kreisfreie Stadt", "SK Kaiserslautern",
         "Koblenz", "SK Koblenz",
         "Mayen-Koblenz", "LK Mayen-Koblenz",
         "Kusel", "LK Kusel",
         "Landau in der Pfalz", "SK Landau i.d.Pfalz",
         "Ludwigshafen am Rhein", "SK Ludwigshafen",
         "Bernkastel-Wittlich", "LK Bernkastel-Wittlich",
         "Mainz", "SK Mainz",
         "Mainz-Bingen", "LK Mainz-Bingen",
         "Neustadt an der Weinstraße", "SK Neustadt a.d.Weinstraße",
         "Neuwied", "LK Neuwied",
         "Pirmasens", "SK Pirmasens",
         "Rhein-Hunsrück-Kreis", "LK Rhein-Hunsrück",
         "Rhein-Lahn-Kreis", "LK Rhein-Lahn-Kreis",
         "Rhein-Pfalz-Kreis", "LK Rhein-Pfalz-Kreis",
         "Speyer", "SK Speyer",
         "Südliche Weinstraße", "LK Südliche Weinstraße",
         "Südwestpfalz", "LK Südwestpfalz",
         "Trier", "SK Trier",
         "Trier-Saarburg", "LK Trier-Saarburg",
         "Vulkaneifel", "LK Vulkaneifel",
         "Westerwaldkreis", "LK Westerwaldkreis",
         "Worms", "SK Worms",
         "Zweibrücken", "SK Zweibrücken")


babel <- data.frame(Meldelandkreis = n2n[1:(length(n2n)/2)*2],
                    gadm = n2n[1:(length(n2n)/2)*2-1]) %>%
  as_tibble()


babel$gadm%in%RLP_geo$Kreise$NAME_2


gadm_lua_names <-
  RLP_geo$Kreise %>%
  inner_join(babel, by = c("NAME_2" = "gadm"))


geo_standards <-
  gadm_lua_names %>% select(Meldelandkreis, GID_2) %>%
  inner_join(
    geo_standards,
    by = c("Meldelandkreis")
  )


## nach dem testen
save(geo_standards, file = "data/geo_standards.rda")




