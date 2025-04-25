
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

Das Paket Laden mit

``` r
library(luaRlp)
```

Das Paket hat mehrere wichtige Funktionen für unsere Amtsaufgaben:

- es kann epidemiologische Daten aus SurvNet zu unseren Labornummern
  hinzufügen. Mehr dazu via

<!-- -->

    vignette("EpiData_RIDOM")

- es kann eine Re-Evaluierung der bioionformatischen Pipelines am
  LUA-RLP durchführen. Mehr dazu via

<!-- -->

    vignette("EpiData_RIDOM")
