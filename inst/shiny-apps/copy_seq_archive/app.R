library(shiny)
library(shinyFiles)
library(zip)

# Zugangsdaten aus Datei einlesen
ALLOWED_USERS <- read.csv("/home/HeitlingerE/allowed_users.csv",
                          stringsAsFactors = FALSE)
# In Liste umwandeln für Kompatibilität mit bestehendem Code
ALLOWED_USERS <- setNames(as.list(ALLOWED_USERS$password),
                          ALLOWED_USERS$user)

# — Erlaubte Wurzelverzeichnisse (Name = Label, Wert = Pfad) —
ALLOWED_DIRS <- c(
  "ARCHIVE" = "/data/ARCHIVED_RUNS/"
)

ui <- fluidPage(
  titlePanel("📁 Sequenzdaten-Archiv-Download"),
  uiOutput("login_ui"),
  conditionalPanel(
    condition = "output.loggedIn == true",
    sidebarLayout(
      sidebarPanel(
        # EIN einziger Dialog für Dateien & Ordner (multi = TRUE)
        shinyFilesButton(
          id = "files",
          label = "📁 Dateien/Ordner wählen",
          title = "Wähle Dateien und/oder Ordner",
          multiple = TRUE
        ),
        verbatimTextOutput("sel_paths"),
        downloadButton("download", "ZIP herunterladen")
      ),
      mainPanel(
        p("Beachten Sie, dass nur ARCHIVIERTE Daten hier zu finden sind. Daten
        aus aktuellen Sequenzierläufen finden sie unter H:Analysenetz"),
        p("→ Nach dem Login wählen Sie mit Klick auf „Dateien/Ordner wählen!“"),
        p("→ Klappen Sie Ordner auf, markiere Dateien (auch mehrere) oder ganze Ordner!"),
        p("→ Klicke Sie auf ZIP herunterladen!")
      )
    )
  )
)

server <- function(input, output, session) {
  creds <- reactiveValues(authorized = FALSE)

  # Login-UI & -Logik
  output$login_ui <- renderUI({
    if (!creds$authorized) {
      tagList(
        textInput("user", "Benutzername:"),
        passwordInput("pwd", "Passwort:"),
        actionButton("go", "Login"),
        verbatimTextOutput("msg")
      )
    }
  })
  output$loggedIn <- reactive(creds$authorized)
  outputOptions(output, "loggedIn", suspendWhenHidden = FALSE)

  observeEvent(input$go, {
    u <- isolate(input$user); p <- isolate(input$pwd)
    if (!is.null(u) && u %in% names(ALLOWED_USERS) &&
        identical(ALLOWED_USERS[[u]], p)) {
      creds$authorized <- TRUE
    } else {
      output$msg <- renderText("❌ Login fehlgeschlagen")
    }
  })

  # EINMALIG: shinyFileChoose initialisieren mit Deinen Roots
  shinyFileChoose(
    input, "files",
    roots = ALLOWED_DIRS,
    session = session,
    restrictions = system.file(package = "base")#,
##    filetypes = c("fastq.gz", "fastq")
  )

  # Selektierte Pfade parsen
  sel_paths <- reactive({
    req(input$files)
    # parseFilePaths liefert ein DataFrame mit name & datapath
    fp <- parseFilePaths(ALLOWED_DIRS, input$files)
    fp$datapath
  })

  # Anzeige der Pfade
  output$sel_paths <- renderText({
    sp <- sel_paths()
    paste(sp, collapse = "\n")
  })

  # ZIP-Download
  output$download <- downloadHandler(
    filename = function() paste0("auswahl_", Sys.Date(), ".zip"),
    content = function(dest) {
      paths <- sel_paths()
      req(length(paths) > 0)
      zip(zipfile = dest, files = paths, mode = "cherry-pick")
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)
