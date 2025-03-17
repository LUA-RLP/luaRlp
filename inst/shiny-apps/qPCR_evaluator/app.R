library(shiny)
library(DT)
library(tidyr)
library(dplyr)
library(magrittr)

source(file.path("R", "preprocess_data.R"))

# UI
ui <- fluidPage(
  titlePanel("qPCR Evaluator"),

  sidebarLayout(
    sidebarPanel(
      tags$p("Einlesen der .csv Datei(en) aus:"),
      tags$p("📂 O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/qPCR_CSVs/"),

      radioButtons("file_choice", "Wie viele Dateien möchten Sie hochladen?",
                   choices = c("Eine Datei" = "one", "Zwei Dateien" = "two"),
                   selected = "two"),

      radioButtons("column_choice", "Spaltenauswahl:",
                   choices = c("Generelle" = "generic", "COV-FLU" = "cov-flu"),
                   selected = "generic"),

      fileInput("first_file", "Datei 1 hochladen",
                accept = c(".csv"),
                placeholder = "Wähle die erste Datei"),

      conditionalPanel(
        condition = "input.file_choice == 'two'",
        fileInput("second_file", "Datei 2 hochladen",
                  accept = c(".csv"),
                  placeholder = "Wähle die zweite Datei")
      ),

      actionButton("process", "Daten verarbeiten"),
      hr(),
      tags$a("Erstellung der CSV-Dateien für den Import", href = "info.html",
             target = "_blank")
    ),

    mainPanel(
      uiOutput("warning_msg"),
      DTOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {

  warning_msg <- reactiveVal(NULL)

  PCR <- eventReactive(input$process, {
    req(input$first_file)

    warning_msg(NULL)

    tab1 <- read.csv(input$first_file$datapath, skip = 19, header = TRUE)

    if (!is.null(input$second_file) && input$file_choice == "two") {
      tab2 <- read.csv(input$second_file$datapath, skip = 19, header = TRUE)

      # Count non-matching sample names
      incomp <- sum(!(tab1$Sample %in% tab2$Sample)) +
        sum(!(tab2$Sample %in% tab1$Sample))

      warning_msg(paste0("⚠️ Warnung: ", incomp,
                         " Sample-Namen aus den beiden Dateien sind nicht kompatibel! ",
                         "Dies führt zu leeren Tabellenfeldern."))

      result <- preprocess_data(tab1, tab2, nicecols = input$column_choice)
    } else {
      warning_msg("✅ Alle Samples stammen aus einer einzigen Datei.")
      result <- preprocess_data(tab1, NULL, nicecols = input$column_choice)
    }

    return(result)
  })

  output$warning_msg <- renderUI({
    tags$div(style = "color: red; font-weight: bold; margin-bottom: 10px;", warning_msg())
  })

  output$table <- renderDT({
    req(PCR())

    datatable(PCR(), escape = FALSE, rownames = FALSE, options = list(
      pageLength = 15,
      autoWidth = TRUE,
      columnDefs = list(list(width = '70px', targets = "_all"))
    )) %>%
      formatStyle(
        columns = colnames(PCR())[-1],
        backgroundColor = styleInterval(c(0), c("white", "red"))
      ) %>%
      formatStyle(
        columns = "ICR",
        backgroundColor = styleInterval(c(NA), c("white", "lightgreen"))
      )
  })
}

shinyApp(ui, server)
