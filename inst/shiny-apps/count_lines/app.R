library(shiny)

options(shiny.maxRequestSize = 1000 * 1024^2)  # 1000 MB limit

ui <- fluidPage(
  titlePanel("Line Counter"),
  fileInput("file", "Choose a text file", accept = c(".txt", ".csv", ".fastq", ".fastq.gz")),
  actionButton("check", "Check File Format"),
  verbatimTextOutput("line_count"),
  verbatimTextOutput("file_format")
)

server <- function(input, output, session) {
  # Reactive value to store the file format result
  format_result <- reactiveVal(NULL)

  # Count lines (always updates when a file is uploaded)
  output$line_count <- renderText({
    req(input$file)
    num_lines <- length(readLines(input$file$datapath))
    paste("Number of lines:", num_lines, "\n",
          "Number of fastq sequences:", num_lines / 4, "if fastq-format!!!")
  })

  # Run format check only when button is clicked
  observeEvent(input$check, {
    req(input$file)  # Ensure a file is uploaded

    # Read the first few lines to infer file format
    file_path <- input$file$datapath
    first_lines <- readLines(file_path, n = 10)  # Read first 10 lines

    if (grepl("^@", first_lines[1]) && grepl("^\\+", first_lines[3])) {
      format_result("Detected format: FASTQ")
    } else {
      format_result("Detected format: UNKNOWN or not FASTQ")
    }
  })

  # Display file format result
  output$file_format <- renderText({
    req(format_result())  # Only show if a result is available
    format_result()
  })
}

shinyApp(ui, server)
