
# R Nutzung am LUA RLP

Unsere Web-Apps sind (nur) innerhalb des Netzwerkes des LUA RLP [unter
dieser Adresse](http://srvldap0008.lua.rlp.de:3838/) zu finden.

# R package luaRlp

Weiter Funktionen stehen für R-Nutzer innerhalb unsers Pakets zu
Verfügung. Nutzer, welche R-Code auch in der Kommandozeile ausführen
möchsten, können zum Installieren des Pakets folgenden Code ausführen
(dies ist nur bei der ersten Installation oder bei Updates notwendig):

    install.packages("remotes")

    remotes::install_github("derele/luaRlp", build_vignettes = TRUE)

Falls Probleme bei der Installation auftauchen sind diese meist durch
eine fehlende Proxy-Konfiguration verursacht. Diese lassen sich
(innerhalb des LUA) durch Ausführen des folgenden Codes lösen:

    source("O:/Abteilung Humanmedizin (AHM)/Referat 32/R_setup/lua_proxy_networking.R")

Danach sollte die Installation durchführbar sein.

Das Paket Laden mit

``` r
library(luaRlp)
```

    ## survnet.dsn ist gesetzt auf: SurvNet_datenbank

Das Paket hat mehrere wichtige Funktionen für unsere Amtsaufgaben:

- es kann epidemiologische Daten aus SurvNet zu unseren Labornummern
  hinzufügen. Mehr dazu via

<!-- -->

    vignette("EpiData_RIDOM")

- es kann eine Re-Evaluierung der bioionformatischen Pipelines am
  LUA-RLP durchführen. Mehr dazu via

<!-- -->

    vignette("QM_pipeline_evaluation")

Diese Art der Dokumentation stellt sicher, dass die Funktionen wie
Dokumentiert auf dem Rechner des Nutzers ausführbar sind (die Funktionen
werden lokal auf dem Nutzer-Rechner ausgeführt um die Dokumentation zu
erstellen).
