# R Nutzung am LUA RLP

Unsere Web-Apps sind (nur) innerhalb des Netzwerkes des LUA RLP [unter dieser Adresse](http://srvldap0008.lua.rlp.de:3838/) zu finden.

# R package luaRlp

Weiter Funktionen stehen für R-Nutzer innerhalb unsers Pakets zu Verfügung. 
Nutzer, welche R-Code auch in der Kommandozeile ausführen möchsten, können zum 
Installieren des Pakets folgenden Code ausführen (dies ist nur bei der ersten 
Installation oder bei Updates notwendig):


```
install.packages("remotes")

remotes::install_github("derele/luaRlp")
```

Das Paket Laden mit
```
library(luaRlp)
```

Das Paket kann z.B. epidemiologische Daten aus SurvNet zu unseren Labornummern 
hinzufügen. Dafür wird ein Export aus unserem LIMS benötigt. Dann lässt sich 
der folgende Code ausführen: 


```
create_Epidata(LIMS_link_file = "O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/14_EpiDaten/LIMS Export Merger/Import1.csv")
```


