library(shiny)
library(DT)  # For interactive tables

options(shiny.maxRequestSize = 500 * 1024^2)  # Allow up to 500 MB uploads

wanted_columns_generic <- c("Perc..Good.Targets", "Avg..Coverage..Assembled.",
                           "Approximated.Genome.Size..Mbases.", "Top.Species.Match.Identity",
                           "Top.Species.Match", "Sample.ID", "Complex.Type",
                           "Sequencing.Run.ID", "N50..Assembled.")

wanted_columns_EHEC <- c("ST.Warwick", "O.Type", "H.Type", "CC.Warwick")

wanted_columns_Salmonella <- c()

wanted_columns_Listeria <- c()

wanted_columns_MRSA <- c()

wanted_columns_Legionella <- c()

wanted_columns <- c(wanted_columns_generic,
                    wanted_columns_EHEC,
                    wanted_columns_Salmonella,
                    wanted_columns_Listeria,
                    wanted_columns_MRSA)


ui <- fluidPage(
  titlePanel("CSV Column Selector"),

  fileInput("file", "Upload a CSV file", accept = ".csv"),

  DTOutput("preview"),  # Display first rows of file

  uiOutput("column_selector"),  # Dynamic column selection UI

  downloadButton("download", "Download Processed CSV")
)

server <- function(input, output, session) {

  # Reactive: Read uploaded CSV
  csv_data <- reactive({
    req(input$file)
    read.delim(input$file$datapath, header = TRUE, stringsAsFactors = FALSE,
               sep=";")
  })

  # Display first few rows of CSV
  output$preview <- renderDT({
    req(csv_data())
    datatable(csv_data(), options = list(
      pageLength = 10,      # Default number of rows
      lengthMenu = c(10, 25, 50, 100),  # Allow user to expand rows
      scrollX = TRUE        # Enable horizontal scrolling if needed
    ))
  })

  # Generate column selection UI dynamically
  output$column_selector <- renderUI({
    req(csv_data())
    colnames_list <- colnames(csv_data())

    checkboxGroupInput("selected_columns", "Select columns to keep:",
                       choices = colnames_list,
                       ## default: wanted columns defined top of scritp
                       selected = colnames_list[colnames_list%in%
                                                  wanted_columns])
  })

  # Reactive: Filter dataset based on selected columns
  filtered_data <- reactive({
    req(csv_data(), input$selected_columns)
    csv_data()[, input$selected_columns, drop = FALSE]
  })

  # Download processed file
  output$download <- downloadHandler(
    filename = function() {
      paste0("filtered_", input$file$name)
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )

}

shinyApp(ui, server)
