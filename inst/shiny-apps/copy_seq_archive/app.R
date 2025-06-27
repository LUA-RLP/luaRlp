library(shiny)
library(shinyFiles)
library(zip)
library(readr)
library(shinycssloaders)
library(callr)

# --- Zugangsdaten einlesen ---
ALLOWED_USERS <- read.csv("/home/HeitlingerE/allowed_users.csv", stringsAsFactors = FALSE)
ALLOWED_USERS <- setNames(as.list(ALLOWED_USERS$password), ALLOWED_USERS$user)

# --- Erlaubte Wurzelverzeichnisse (NUR ARCHIV) ---
ALLOWED_DIRS <- c("ARCHIVE" = "/data/ARCHIVED_RUNS/")

ui <- fluidPage(
  titlePanel("📦 Sequenzdaten-Archiv-Download"),
  uiOutput("login_ui"),
  conditionalPanel(
    condition = "output.loggedIn == true",
    sidebarLayout(
      sidebarPanel(
        tags$hr(),
        h4("📁 Dateien manuell auswählen"),
        shinyFilesButton("files", "📁 Dateien/Ordner wählen", "Wähle Dateien und/oder Ordner", multiple = TRUE),
        tags$hr(),

        h4("🔎 Für einzelne Proben fastq-Dateien suchen"),
        p("Gib eine Probe-ID oder ein Muster (Regex) ein, z.\u202fB. '3263-2025-000162' oder '3263-2025-00.*'"),
        textInput("text_input_id", "Probe-ID oder Suchmuster:"),
        actionButton("manual_search_btn", "Dateien suchen (Einzeleingabe)"),
        tags$hr(),

        h4("🔎📋 Auswahl von fastq-Dateien via CSV"),
        p("Lade eine CSV-Datei mit einer Spalte 'Sample ID' zur Suche hoch."),
        fileInput("csv_file", "CSV-Datei auswählen"),
        actionButton("search_btn", "Dateien suchen (Sample ID aus Liste)"),
        tags$hr(),

        h4("📦 Gefundene/ausgewählte Dateien herunterladen"),
        withSpinner(textOutput("search_info")),
        withSpinner(uiOutput("found_files_ui")),
        uiOutput("cancel_ui"),
        downloadButton("download_zip", "ZIP herunterladen")
      ),

      mainPanel(
        p("Beachten Sie, dass nur ARCHIVIERTE Daten hier zu finden sind."),
        p("\u2192 Nach dem Login wählen Sie mit Klick auf „Dateien/Ordner wählen!“"),
        p("\u2192 Klappen Sie Ordner auf, markieren Sie Dateien (auch mehrere) oder ganze Ordner!"),
        p("\u2192 Oder (1): Geben Sie eine 'Sample ID' ein und suchen Sie nach passenden fastq Dateien."),
        p("\u2192 Oder (2): Laden Sie eine CSV mit Spalte 'Sample ID' hoch und suchen Sie nach passenden fastq Dateien."),
        p("\u2192 Klicken Sie auf ZIP herunterladen!"),
        tags$hr(),
        h4("📄 Liste aktuell ausgewählter/gefunder Dateien:"),
        withSpinner(verbatimTextOutput("found_files_list"))
      )
    )
  )
)

server <- function(input, output, session) {
  search_mode <- reactiveVal("none")
  creds <- reactiveValues(authorized = FALSE)
  found_files <- reactiveVal(NULL)
  zip_process <- reactiveVal(NULL)
  copying <- reactiveVal(FALSE)

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
    u <- isolate(input$user)
    p <- isolate(input$pwd)
    if (!is.null(u) && u %in% names(ALLOWED_USERS) && identical(ALLOWED_USERS[[u]], p)) {
      creds$authorized <- TRUE
    } else {
      output$msg <- renderText("❌ Login fehlgeschlagen")
    }
  })

  shinyFileChoose(input, "files", roots = ALLOWED_DIRS, session = session, restrictions = system.file(package = "base"))

  observeEvent(input$files, {
    parsed <- parseFilePaths(ALLOWED_DIRS, input$files)
    if (nrow(parsed) > 0) {
      found_files(as.character(parsed$datapath))
    }
  })

  observeEvent(input$manual_search_btn, {
    found_files(NULL)
    search_mode("text")
    pattern <- trimws(input$text_input_id)
    req(nzchar(pattern))
    root_path <- ALLOWED_DIRS[["ARCHIVE"]]
    all_files <- list.files(root_path, recursive = TRUE, full.names = TRUE)
    regex_pattern <- paste0("^", pattern, ".*\\.fastq(\\.gz)?$")
    matches <- all_files[grepl(regex_pattern, basename(all_files))]
    found_files(matches)
  })

  target_sample_ids <- reactive({
    req(input$csv_file)
    df <- read_csv(input$csv_file$datapath, show_col_types = FALSE)
    validate(need("Sample ID" %in% names(df), "⚠️ CSV muss eine Spalte 'Sample ID' enthalten."))
    ids <- trimws(as.character(df[["Sample ID"]]))
    ids[nchar(ids) == 16]
  })

  observeEvent(input$search_btn, {
    found_files(NULL)
    search_mode("csv")
    req(target_sample_ids())
    root_path <- ALLOWED_DIRS[["ARCHIVE"]]
    all_files <- list.files(root_path, recursive = TRUE, full.names = TRUE)
    pattern <- paste0("^(", paste0(target_sample_ids(), collapse = "|"), ").*\\.fastq(\\.gz)?$")
    matching <- all_files[grepl(pattern, basename(all_files))]
    found_files(matching)
  })

  output$search_info <- renderText({
    files <- found_files()
    if (is.null(files)) return("⏳ Suche läuft oder noch keine Auswahl/Suche durchgeführt.")
    search_label <- switch(
      search_mode(),
      "csv" = paste0("🔍📋 Gesuchte IDs (CSV): ", paste0(target_sample_ids(), collapse = ", "), "\n"),
      "text" = if (nzchar(input$text_input_id)) paste0("🔍 Gesuchtes Muster: ", input$text_input_id, "\n") else "",
      ""
    )
    paste0(search_label, "📂 Dateien bereit: ", length(files),
           if (length(files) == 0) "\n⚠️ Keine Dateien gefunden oder ausgewählt." else "")
  })

  output$found_files_ui <- renderUI({
    req(found_files())
    if (length(found_files()) == 0) {
      "❌ Keine passenden Dateien gefunden."
    } else {
      HTML("<b>✅ Dateien bereit zum Download.</b>")
    }
  })

  output$found_files_list <- renderText({
    f <- found_files()
    if (is.null(f)) return("Noch keine Dateien ausgewählt oder gefunden.")
    paste(f, collapse = "\n")
  })

  output$cancel_ui <- renderUI({
    if (copying()) {
      actionButton("cancel_zip", "⛔ Kopiervorgang abbrechen", class = "btn-danger")
    }
  })

  observeEvent(input$cancel_zip, {
    proc <- zip_process()
    if (!is.null(proc) && proc$is_alive()) {
      proc$kill()
      copying(FALSE)
      showNotification("❌ ZIP-Vorgang abgebrochen!", type = "warning")
    }
  })

  session$onSessionEnded(function() {
    proc <- zip_process()
    if (!is.null(proc) && proc$is_alive()) {
      proc$kill()
    }
  })

  output$download_zip <- downloadHandler(
    filename = function() {
      paste0("download_", Sys.Date(), ".zip")
    },
    content = function(file) {
      files <- found_files()
      if (is.null(files) || length(files) == 0 || any(!file.exists(files))) {
        showNotification("Keine gültigen Dateien zum Herunterladen.", type = "error")
        return(NULL)
      }

      copying(TRUE)
      output$found_files_ui <- renderUI({ HTML("<b>⏳ ZIP-Vorgang läuft...</b>") })

      p <- callr::r_bg(function(files, file) {
        zip::zipr(zipfile = file, files = files, root = "/")
      }, args = list(files = files, file = file))
      zip_process(p)

      while (p$is_alive()) {
        Sys.sleep(0.5)
      }

      copying(FALSE)

      if (p$get_exit_status() != 0) {
        showNotification("❌ ZIP-Vorgang fehlgeschlagen oder abgebrochen", type = "error")
        return(NULL)
      }
    },
    contentType = "application/zip"
  )
}

shinyApp(ui, server)
