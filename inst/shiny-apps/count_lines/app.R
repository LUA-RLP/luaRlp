library(shiny)

options(shiny.maxRequestSize = 1000 * 1024^2)  # 1000 MB limit

ui <- fluidPage(
  titlePanel("Line Counter"),
  fileInput("file", "Choose a text file", accept = c(".txt", ".csv",
                                                     ".fastq", ".fastq.gz")),
  verbatimTextOutput("line_count")
)

server <- function(input, output) {
  output$line_count <- renderText({
    req(input$file)
    num_lines <- length(readLines(input$file$datapath))
    paste("Number of lines:", num_lines, "\n",
          "Number of fastq sequences:", num_lines/4, "if fastq-format!!!")
  })
}

shinyApp(ui, server)
