library(shiny)
library(DT)  # For interactive tables
library(tidyr)
library(dplyr)
library(magrittr)

source(file.path("R", "preprocess_data.R"))

# UI
ui <- fluidPage(
  titlePanel("PCR Data Table with Background Colors"),

  # File Input
  sidebarLayout(
    sidebarPanel(
      tags$p("Default files can be found at:"),
      tags$p("O:/Abteilung Humanmedizin (AHM)/Referat 32/32_6/qPCR_CSVs/"),

      fileInput("cov_file", "Upload CoV-2 File",
                accept = c(".csv"),
                placeholder = "Select a CoV-2 CSV file"),

      fileInput("flu_file", "Upload Flu-RSV File",
                accept = c(".csv"),
                placeholder = "Select a Flu-RSV CSV file")
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
    # Check if files are uploaded
    req(input$cov_file, input$flu_file)

    # Read CSVs
    COV <- read.csv(input$cov_file$datapath, skip = 19, header = TRUE) %>% cbind(pcr = "COV")
    FLU <- read.csv(input$flu_file$datapath, skip = 19, header = TRUE) %>% cbind(pcr = "FLU")

    preprocess_data_COV_FLU(COV, FLU)
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
      ) %>%
      formatStyle(
        columns = "Sample",
        fontWeight = styleEqual(
          unique(PCR()$Sample),
          ifelse(rowSums(!is.na(PCR()[, c("CoV-2", "InfA", "InfB", "RSV A/B")])) > 0, "bold", "normal")
        )
      )
    })
}

shinyApp(ui, server)
