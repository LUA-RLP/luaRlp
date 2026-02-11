# app.R
# App author: Emanuel Heitlinger

library(shiny)
library(dplyr)
library(DT)

source("R/config.R", local = TRUE)
source("R/utils.R", local = TRUE)
source("R/sure_link.R", local = TRUE) 
source("R/io_discovery.R", local = TRUE)
source("R/parsers.R", local = TRUE)
source("R/models.R", local = TRUE)

ui <- navbarPage(
  "Influenza Dashboard",
  id = "main_tab",
  tabPanel(
    "Runs",
    fluidPage(
      fluidRow(
        column(
          12,
          h3("Runs"),
          fluidRow(
            column(6, checkboxInput("include_empty_runs",
            "Zeige Läufe without ohne Influenza-Proben (keine 3263-Proben)", FALSE)),
            column(6, checkboxInput("include_no_pass_runs",
            "Zeige Läufe ohne Proben über den Qualitätsschwellenwerten", FALSE))
          ),
          DTOutput("runs_tbl"),
          hr(),
          uiOutput("run_header"),
          DTOutput("samples_tbl"),
          hr(),
          tags$div(style = "color:#666; font-size:12px;", "App author: Emanuel Heitlinger")
        )
      )
    )
  ),
  
  tabPanel(
    "Epidemiology",
    fluidPage(
      fluidRow(
        column(
          12,
          h3("Epidemiology"),
          tags$p(style="color:#666;",
          "Positive Proben (über ausgewählte Sequenzieräufe und Zeitraum)"),
          
          fluidRow(
            column(
              6,
              uiOutput("epi_run_picker")
            ),
            column(
              6,
              dateRangeInput(
                "epi_daterange",
                "Auswahl des Laufdatums",
                start = Sys.Date() - 30,
                end = Sys.Date(),
                format = "yyyy-mm-dd"
              )
            )
          ),
          
          # >>> ADD THIS BLOCK (Epidemiology-only filter) <<<
          fluidRow(
            column(
              12,
              checkboxInput(
                "epi_only_sure",
                "Zeige nur SURE Proben",
                value = TRUE
              )
            )
          ),
          
          
          br(),
          DTOutput("epi_tbl"),
          hr(),
          tags$div(style = "color:#666; font-size:12px;", "App author: Emanuel Heitlinger")
        )
      )
    )
  )
)
  
  server <- function(input, output, session) {
    
    runs_r <- reactiveVal(tibble::tibble())
    
    refresh_runs <- function() {
      out <- tryCatch(
        list_runs_fast(DATA_ROOT, BASE_URL),
        error = function(e) {
          dbg("list_runs_fast error: ", conditionMessage(e))
          tibble::tibble()
        }
      )
      runs_r(out)
    }
    
    observeEvent(TRUE, { refresh_runs() }, once = TRUE)
    
    observe({
      invalidateLater(REFRESH_MS, session)
      refresh_runs()
    })
    
    filtered_runs <- reactive({
      runs <- runs_r()
      if (nrow(runs) == 0) return(runs)
      if (!isTRUE(input$include_empty_runs)) runs <- runs %>% filter(has_samplesheet)
      runs
    })
    
    output$epi_run_picker <- renderUI({
      runs <- filtered_runs()
      choices <- runs$run
      # default: all runs
      selectInput(
        "epi_runs",
        "Zeige folgende Sequenzierläufe",
        choices = choices,
        selected = choices,
        multiple = TRUE
      )
    })
    
epi_data <- reactive({
  runs <- epi_runs_filtered()
  if (nrow(runs) == 0) return(tibble::tibble())

  all_samples <- purrr::pmap_dfr(
    list(runs$pipeline_dir, runs$results_dir, runs$run),
    function(pipeline_dir, results_dir, run_name) {
      tryCatch({
        df <- build_sample_table(pipeline_dir, results_dir)
        if (is.null(df) || nrow(df) == 0) return(tibble::tibble())
        df %>% dplyr::mutate(run = run_name)
      }, error = function(e) {
        dbg("epi build_sample_table error for run ", run_name, ": ", conditionMessage(e))
        tibble::tibble()
      })
    }
  )

  if (nrow(all_samples) == 0) return(tibble::tibble())

  # Optional: SURE-only filter (epi tab only)
  if (isTRUE(input$epi_only_sure)) {
    sure_ids <- get_sure_ids()  # must return a df with column ID
    all_samples <- all_samples %>%
      dplyr::mutate(sample_md5 = md5_id(sample_id)) %>%
      dplyr::semi_join(sure_ids, by = c("sample_md5" = "ID"))
  }

  all_samples %>%
    dplyr::mutate(
      subtype  = dplyr::na_if(as.character(subtype), ""),
      clade    = dplyr::na_if(as.character(clade), ""),
      subclade = dplyr::na_if(as.character(subclade), "")
    ) %>%
    dplyr::filter(!is.na(subtype) | !is.na(clade) | !is.na(subclade)) %>%
    dplyr::group_by(subtype, clade, subclade) %>%
    dplyr::summarise(
      n_samples = dplyr::n_distinct(sample_id),
      n_runs = dplyr::n_distinct(run),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_samples))
})


    epi_runs_filtered <- reactive({
      runs <- filtered_runs()
      if (nrow(runs) == 0) return(runs)
      
      # filter by run selection
      sel <- input$epi_runs
      if (!is.null(sel) && length(sel) > 0) {
        runs <- runs %>% dplyr::filter(.data$run %in% sel)
      } else {
        # if user deselects everything: show nothing
        return(runs[0, , drop = FALSE])
      }
      
      # filter by date range (based on runs$updated)
      dr <- input$epi_daterange
      if (!is.null(dr) && length(dr) == 2 && !any(is.na(dr))) {
        start <- as.POSIXct(dr[1], tz = "Europe/Berlin")
        end   <- as.POSIXct(dr[2] + 1, tz = "Europe/Berlin") # inclusive end date
        runs <- runs %>% dplyr::filter(.data$updated >= start, .data$updated < end)
      }
      
      runs
    })
    
    
    
    
    output$runs_tbl <- renderDT({
      runs <- filtered_runs()
      validate(need(nrow(runs) > 0, "Keine Sequenzierläufe gefunden (für Auswahlbedingungen)."))
      
      # ---- NEW: cached counts per run (fast after first computation) ----
      metrics <- purrr::pmap_dfr(
        list(runs$pipeline_dir, runs$results_dir),
        function(pipeline_dir, results_dir) {
          tryCatch(
            run_sample_counts_cached(pipeline_dir, results_dir),
            error = function(e) tibble::tibble(
              n_samples = NA_integer_, n_reads = NA_integer_,
              n_hn = NA_integer_, n_subtype = NA_integer_, n_nextclade = NA_integer_
            )
          )
        }
      )
      
      runs2 <- dplyr::bind_cols(runs, metrics)
      
      disp <- runs2 %>%
      transmute(
        ` ` = case_when(
          !has_samplesheet ~ "\U0001F6D1",        # 🛑
          status == "done" ~ "\U0001F7E2",        # 🟢
          status == "partial" ~ "\U0001F7E1",     # 🟡
          status == "no_results" ~ "\U000026AA",  # ⚪
          TRUE ~ "\U0001F534"
        ),
        Run = run,
        Status = status,
        Updated = format(updated, "%Y-%m-%d %H:%M"),
        
        # ---- NEW: run overview metrics ----
        Reads     = paste0(n_reads, "/", n_samples),
        `H/N`     = paste0(n_hn, "/", n_samples),
        Subtype   = paste0(n_subtype, "/", n_samples),
        Nextclade = paste0(n_nextclade, "/", n_samples),
        
        MultiQC = ifelse(
          !is.na(multiqc_url),
          paste0('<a href="', multiqc_url, '" target="_blank">MultiQC</a>'),
          ifelse(!is.na(multiqc_path), "Local", "—")
        )
      )
      
      datatable(disp, escape = FALSE, selection = "single",
      options = list(pageLength = 20, autoWidth = TRUE))
    })
    
    selected_run <- reactive({
      runs <- filtered_runs()
      idx <- input$runs_tbl_rows_selected
      if (length(idx) != 1) return(NULL)
      runs[idx, , drop = FALSE]
    })
    
    output$run_header <- renderUI({
      sr <- selected_run()
      if (is.null(sr)) return(NULL)
      
      run_name <- scalar_chr(sr$run)
      results_dir <- scalar_chr(sr$results_dir)
      pipeline_dir <- scalar_chr(sr$pipeline_dir)
      has_ss <- scalar_lgl(sr$has_samplesheet)
      
      dbg("Selected run: ", run_name)
      dbg("  results_dir=", results_dir)
      dbg("  pipeline_dir=", pipeline_dir)
      dbg("  has_samplesheet=", has_ss)
      
      ss_path <- find_samplesheet_path(pipeline_dir, results_dir)
      
      mq_url <- scalar_chr(sr$multiqc_url)
      mq_path <- scalar_chr(sr$multiqc_path)
      
      mq <- if (!is.na(mq_url) && mq_url != "") {
        tags$a(href = mq_url, target = "_blank", "Open MultiQC report")
      } else if (!is.na(mq_path) && mq_path != "") {
        tags$span("MultiQC (local path): ", tags$code(mq_path))
      } else {
        tags$span("MultiQC: —")
      }
      
      tags$div(
        h3(paste0("Run: ", run_name)),
        tags$p(tags$b("Status: "), scalar_chr(sr$status)),
        tags$p(mq),
        tags$p("Samplesheet: ", tags$code(ss_path %||str% "—")),
        if (!has_ss) tags$p(tags$span(style="color:#a94442; font-weight:700;",
        "ERROR: Fehlendes oder leeres samplesheet (kann keine Probentabelle ersellen).")) else NULL,
        tags$p(tags$small(tags$code(results_dir)))
      )
    })
    
    output$samples_tbl <- renderDT({
      sr <- selected_run()
      validate(need(!is.null(sr), "Wähle einen Sequenzierlauf."))
      
      tryCatch({
        run_name <- scalar_chr(sr$run)
        results_dir <- scalar_chr(sr$results_dir)
        pipeline_dir <- scalar_chr(sr$pipeline_dir)
        
        validate(need(!is.na(results_dir) && nzchar(results_dir) && fs::dir_exists(results_dir),
        "Sequenzierlauf hat kein Resultate-Ordner."))
        
        dbg("samples_tbl start for run ", run_name)
        
        df <- build_sample_table(pipeline_dir, results_dir)
        validate(need(!is.null(df) && nrow(df) > 0, "No readable samplesheet for this run."))
        
        if (!isTRUE(input$include_no_pass_runs)) {
          validate(need(any(df$read_count > 0, na.rm = TRUE),
          "Dieser Lauf hat keine Proben (mit reads). Setze den Haken um auszuwählen."))
        }
        
        show_cols <- c("sample_id","read_count",
        "influenza_type","H","N","subtype",
        "clade","subclade","qc_status","qc_score")
        
        disp <- df %>% select(any_of(show_cols))
        
        dt <- datatable(disp, options = list(pageLength = 25, autoWidth = TRUE), rownames = FALSE)
        
        dt
        
      }, error = function(e) {
        dbg("samples_tbl ERROR: ", conditionMessage(e))
        datatable(
          data.frame(
            Message = c(
              "Ein Fehler ist aufgetreten. Kontrolliere die logs oder kontaktiere den Author der app.",
              paste0("Details: ", conditionMessage(e)),
              "App Author: Emanuel Heitlinger"
            )
          ),
          options = list(dom = "t"),
          rownames = FALSE
        )
      })
    })
    
    output$epi_tbl <- renderDT({
      df <- epi_data()
      validate(need(nrow(df) > 0, "Keine subtype/clade/subclade daten für die aktuelle Auswahl gefunden."))
      
      disp <- df %>%
      transmute(
        Subtype = dplyr::coalesce(subtype, "—"),
        Clade = dplyr::coalesce(clade, "—"),
        Subclade = dplyr::coalesce(subclade, "—"),
        `#Samples` = n_samples,
        `#Runs` = n_runs
      )
      
      datatable(
        disp,
        rownames = FALSE,
        options = list(pageLength = 25, autoWidth = TRUE, order = list(list(3, "desc")))
      )
    })
    
    
  }
  
  shinyApp(ui, server)
  