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
    "Läufe",
    fluidPage(
      fluidRow(
        column(
          12,
          h3("Läufe"),
          fluidRow(
            column(6, checkboxInput("include_empty_runs",
            "Zeige Läufe ohne Influenza-Proben (keine 3263-Proben)", FALSE)),
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
    "Epidemiologie",
    fluidPage(
      fluidRow(
        column(
          12,
          h3("Epidemiologie"),
          tags$p(style="color:#666;",
          "Positive Proben (über ausgewählte Sequenzieräufe und Zeitraum)"),
          
          fluidRow(
            column(6, uiOutput("epi_run_picker")),
            column(6, dateRangeInput(
              "epi_daterange",
              "Sequenzierlauf innerhalb Zeitraum",
              start = Sys.Date() - 30,
              end = Sys.Date(),
              format = "yyyy-mm-dd"
            ))
          ),
          
          fluidRow(
            column(6, uiOutput("epi_probenahme_ui"))
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
          
          br(),
          h4("Line list"),
          fluidRow(
            column(12, downloadButton("dl_epi_line", "Download Line list (CSV)"))
          ),
          DTOutput("epi_line_tbl"),
          
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
  
  output$epi_probenahme_ui <- renderUI({
    ctl <- dateRangeInput(
      "epi_probenahme_range",
      "Probenahmedatum innerhalb Zeitraum (nur SURE)",
      start = Sys.Date() - 30,
      end   = Sys.Date(),
      format = "yyyy-mm-dd"
    )
    
    if (isTRUE(input$epi_only_sure)) {
      ctl
    } else {
      tags$div(style = "opacity:0.5; pointer-events:none;", ctl)
    }
  })
  
  
  
  
  epi_runs_filtered <- reactive({
    runs <- filtered_runs()
    if (nrow(runs) == 0) return(runs)
    
    # filter by run selection
    sel <- input$epi_runs
    if (!is.null(sel) && length(sel) > 0) {
      runs <- runs %>% dplyr::filter(.data$run %in% sel)
    } else {
      return(runs[0, , drop = FALSE])
    }
    
    # filter by run updated date range
    dr <- input$epi_daterange
    if (!is.null(dr) && length(dr) == 2 && !any(is.na(dr))) {
      start <- as.POSIXct(dr[1], tz = "Europe/Berlin")
      end   <- as.POSIXct(dr[2] + 1, tz = "Europe/Berlin") # inclusive end date
      runs <- runs %>% dplyr::filter(.data$updated >= start, .data$updated < end)
    }
    
    runs
  })
  
  epi_samples <- reactive({
    runs <- epi_runs_filtered()
    if (nrow(runs) == 0) return(tibble::tibble())
    
    # 1) collect samples across runs
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
    
    # 2) attach SURE metadata (left join; keeps non-SURE samples!)
    sure_df <- get_sure_data()
    all_samples <- join_sure(all_samples, sure_df)
    if (nrow(all_samples) == 0) return(tibble::tibble())
    
    # 3) ONLY when epi_only_sure: restrict to SURE + apply Probenahmedatum filter
    if (isTRUE(input$epi_only_sure)) {
      
      all_samples <- samples_only_sure(all_samples)
      if (nrow(all_samples) == 0) return(tibble::tibble())
      
      drp <- input$epi_probenahme_range
      if (!is.null(drp) && length(drp) == 2 && !any(is.na(drp)) &&
      "Probenahmedatum" %in% names(all_samples)) {
        
        all_samples <- all_samples %>%
        dplyr::mutate(
          Probenahmedatum = suppressWarnings(as.Date(
            Probenahmedatum,
            tryFormats = c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y")
          ))
        )
        
        start_p <- as.Date(drp[1])
        end_p   <- as.Date(drp[2])
        
        all_samples <- all_samples %>%
        dplyr::filter(
          !is.na(Probenahmedatum),
          Probenahmedatum >= start_p,
          Probenahmedatum <= end_p
        )
      }
    }
    
    # 4) ALWAYS normalize typing columns (independent of SURE)
    all_samples <- all_samples %>%
    dplyr::mutate(
      subtype  = dplyr::na_if(as.character(subtype), ""),
      clade    = dplyr::na_if(as.character(clade), ""),
      subclade = dplyr::na_if(as.character(subclade), "")
    )
    
    all_samples
  })
  
  
  epi_data <- reactive({
    x <- epi_samples()
    if (nrow(x) == 0) return(tibble::tibble())
    
    x %>%
    dplyr::filter(!is.na(subtype) | !is.na(clade) | !is.na(subclade)) %>%
    dplyr::group_by(subtype, clade, subclade) %>%
    dplyr::summarise(
      n_samples = dplyr::n_distinct(sample_id),
      n_runs    = dplyr::n_distinct(run),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_samples))
  })
  
  epi_line_list <- reactive({
    x <- epi_samples()
    if (nrow(x) == 0) return(tibble::tibble())
    
    # first non-NA helper that works for ANY type
    first_non_na_any <- function(v) {
      v <- v[!is.na(v)]
      if (length(v) == 0) return(NA)
      v[[1]]
    }
    
    # for character fields: ignore empty strings too
    first_non_empty_chr <- function(v) {
      v <- as.character(v)
      v <- v[!is.na(v) & nzchar(v)]
      if (length(v) == 0) return(NA_character_)
      v[[1]]
    }
    
    # robustly get first Date (keeps Date class)
    first_date <- function(v) {
      v <- v[!is.na(v)]
      if (length(v) == 0) return(as.Date(NA))
      as.Date(v[[1]])
    }
    
    # minimal mojibake repair (turns "mÃ¤nnlich" -> "männlich")
    fix_mojibake <- function(v) {
      v <- as.character(v)
      out <- iconv(v, from = "latin1", to = "UTF-8")
      # keep original if iconv failed
      ifelse(is.na(out), v, out)
    }
    
    output$dl_epi_line <- downloadHandler(
      filename = function() {
        paste0("influenza_epi_linelijst_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file) {
        df <- epi_line_list()
        
        # optional: make the exported columns exactly match the DT display
        out <- df %>%
        dplyr::transmute(
          MD5_ID = sample_md5,
          Probenahmedatum = Probenahmedatum,
          Geburtsmonat = Geburtsmonat,
          Geburtsjahr = Geburtsjahr,
          Geschlecht = Geschlecht,
          Einsender = Einsender,
          Subtype = dplyr::coalesce(subtype, "—"),
          Clade = dplyr::coalesce(clade, "—"),
          Subclade = dplyr::coalesce(subclade, "—"),
          Runs = runs,
          `#Runs` = n_runs
        )
        
        # write UTF-8 CSV (works for umlauts, Excel generally OK; if needed add BOM)
        readr::write_excel_csv(out, file)   # requires readr (you already use it)
      }
    )
    
    
    x %>%
    dplyr::filter(!is.na(sample_md5) & nzchar(sample_md5)) %>%
    dplyr::group_by(sample_md5) %>%
    dplyr::summarise(
      runs   = paste(sort(unique(as.character(run))), collapse = ", "),
      n_runs = dplyr::n_distinct(run),
      
      Probenahmedatum = first_date(Probenahmedatum),
      Geburtsmonat    = first_non_empty_chr(Geburtsmonat),
      Geburtsjahr     = first_non_empty_chr(Geburtsjahr),
      Geschlecht      = fix_mojibake(first_non_empty_chr(Geschlecht)),
      Einsender       = fix_mojibake(first_non_empty_chr(Einsender)),
      
      subtype  = first_non_empty_chr(subtype),
      clade    = first_non_empty_chr(clade),
      subclade = first_non_empty_chr(subclade),
      
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(n_runs), dplyr::desc(Probenahmedatum))
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
    
    # ensure columns exist even if discovery returned a reduced schema
    need_chr <- c("multiqc_url", "multiqc_path", "status", "run")
    need_lgl <- c("has_samplesheet")
    need_posix <- c("updated")
    
    for (nm in need_chr)  if (!nm %in% names(runs2)) runs2[[nm]] <- NA_character_
    for (nm in need_lgl)  if (!nm %in% names(runs2)) runs2[[nm]] <- NA
    for (nm in need_posix) if (!nm %in% names(runs2)) runs2[[nm]] <- as.POSIXct(NA)
    
    
    
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
      
      MultiQC = dplyr::if_else(
        !is.na(.data$multiqc_url) & nzchar(.data$multiqc_url),
        paste0('<a href="', .data$multiqc_url, '" target="_blank">MultiQC</a>'),
        dplyr::if_else(
          !is.na(.data$multiqc_path) & nzchar(.data$multiqc_path),
          "Local",
          "—"
        )
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
      h3(paste0("Lauf: ", run_name)),
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
  
  output$epi_line_tbl <- renderDT({
    df <- epi_line_list()
    validate(need(nrow(df) > 0, "Keine Proben für die aktuelle Auswahl."))
    
    disp <- df %>%
    dplyr::transmute(
      MD5_ID = sample_md5,
      Probenahmedatum = Probenahmedatum,
      Geburtsmonat = Geburtsmonat,
      Geburtsjahr = Geburtsjahr,
      Geschlecht = Geschlecht,
      Einsender = Einsender,
      Subtype = dplyr::coalesce(subtype, "—"),
      Clade = dplyr::coalesce(clade, "—"),
      Subclade = dplyr::coalesce(subclade, "—"),
      Runs = runs,
      `#Runs` = n_runs
    )
    
    datatable(
      disp,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        autoWidth = FALSE,
        scrollX = TRUE,
        order = list(list(10, "desc"))  # #Runs descending
      )
    )
  })
}

shinyApp(ui, server)
