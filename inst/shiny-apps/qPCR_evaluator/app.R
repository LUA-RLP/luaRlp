library(shiny)
library(DT)  # For interactive tables
library(tidyr)
library(dplyr)
library(magrittr)

source(file.path("R", "preprocess_data.R"))

# UI
ui <- fluidPage(
  titlePanel("qPCR Evaluator"),

  # File Input
  sidebarLayout(
    sidebarPanel(
      tags$p("Einlesen der .csv Datei(en) aus:"),
      tags$p("📂 O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/qPCR_CSVs/"),

      # User selects whether to upload one or two files
      radioButtons("file_choice", "Wie viele Dateien möchten Sie hochladen?",
                   choices = c("Eine Datei" = "one", "Zwei Dateien" = "two"),
                   selected = "two"),

      radioButtons("column_choice", "Spaltenauswahl:",
                   choices = c("Generelle" = "generic", "COV-FLU" = "cov-flu"),
                   selected = "generic"),

      # File input for CoV-2 (always shown)
      fileInput("first_file", "Datei 1 hochladen",
                accept = c(".csv"),
                placeholder = "Wähle die erste Datei"),

      # Conditionally show Flu-RSV input if the user selects "two files"
      conditionalPanel(
        condition = "input.file_choice == 'two'",
        fileInput("second_file", "Datei 2 hochladen",
                  accept = c(".csv"),
                  placeholder = "Wähle die zweite Datei")
      ),

      hr(),
      tags$a("Erstellung der CSV-Dateien für den Import", href = "info.html",
             target = "_blank")  # Opens in new tab
    ),

    mainPanel(
      DTOutput("table")
    )
  )
)

# Server
server <- function(input, output, session) {

  # Reactive Data Loading
  PCR <- reactive({
    # Ensure at least one file is uploaded
    req(input$first_file)

    # Read first file (COV-2)
    tab1 <- read.csv(input$first_file$datapath, skip = 19, header = TRUE)

    # Check if second file is uploaded and process it if available
    if (!is.null(input$second_file)) {
      tab2 <- read.csv(input$second_file$datapath, skip = 19, header = TRUE)

      # Combine both datasets and preprocess
      return(preprocess_data(tab1, tab2))
    } else {
      # Only preprocess the first dataset
      return(preprocess_data(tab1, NULL))
    }
  })
  output$table <- renderDT({
    datatable(PCR(), escape = FALSE, rownames = FALSE, options = list(
      pageLength = 15,
      autoWidth = TRUE,
      columnDefs = list(list(width = '70px', targets = "_all"))  # Smaller column width
    )) %>%
      formatStyle(
        columns = colnames(PCR())[-1],
        backgroundColor = styleInterval(c(0), c("white", "red"))  # Red for numbers > 0
      ) %>%
      formatStyle(
        columns = "ICR",
        backgroundColor = styleInterval(c(NA), c("white", "lightgreen"))  # Green if ICR has a value
      ) #%>%
      # formatStyle(
      #   columns = "Sample",
      #   fontWeight = styleEqual(
      #     unique(PCR()$Sample),
      #     ifelse(rowSums(!is.na(PCR()[, c("CoV-2", "InfA", "InfB", "RSV A/B")])) > 0, "bold", "normal")
      #   )
      # )
    })
}

shinyApp(ui, server)
