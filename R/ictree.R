#' @export

#' @importFrom shiny column fluidRow isolate navbarPage observeEvent reactiveValues shinyApp


ictree <- function(treedata = NULL) {
  
  treedata_name <- deparse(substitute(treedata))
  
  shinyApp(
    
    ui =
      navbarPage(
        title = "Conditional inference trees",
        id = "navbar",
        
        tabPanel(
          title = "Filter", 
          fluidRow(
            column(
              width = 3,
              datamods::filter_data_ui("filtering", max_height = "500px")
            ),
            column(
              width = 9,
              DT::DTOutput(outputId = "table"),
              tags$b("R code:"),
              verbatimTextOutput(outputId = "code_dplyr")
            )
          )
        ),
        
        tabPanel(
          title = "Trees",
          ctreeUI(id = "interactive_tree")
        )
        
      ),
    
    server = 
      
      function(input, output, session) {
        
        ns <- session$ns
        
        rv <- reactiveValues(
          data = treedata,
          name = treedata_name
        )
        
        # Launch import modal if no data at start
        if (is.null(isolate(rv$data))) {
          datamods::import_modal(
            id = ns("import-data"),
            from = c("env", "file", "copypaste", "url"),
            title = "Import data"
          )
        }
        
        data_imported <- datamods::import_server("import-data", return_class = "tbl_df")
        
        observeEvent(data_imported$data(), {
          rv$data <- data_imported$data()
          rv$name <- data_imported$name()
        })
        
        res_filter <- datamods::filter_data_server(
          id = "filtering",
          data = reactive(rv$data),
          name = reactive(rv$name),
          # vars = reactive(NULL), # toutes les variables
          widget_num = "slider",
          widget_date = "slider",
          label_na = "missing"
        )
        
        dt_tree <- reactiveValues(
          data = NULL,
          name = NULL
        )
        
        observeEvent(rv$data, {
          dt_tree$data = rv$data
          dt_tree$name = rv$name
        })
        
        observeEvent(res_filter$filtered(), {
          dt_tree$data = res_filter$filtered()
        })
        
        output$table <- DT::renderDT({
          res_filter$filtered()
        }, options = list(pageLength = 10))
        
        output$code_dplyr <- renderPrint({
          res_filter$code()
        })
        
        ctreeServer(id = "interactive_tree", reactive(dt_tree$data))
        
      }
  )}
