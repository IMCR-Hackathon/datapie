#' A data package interface for evaluation (datapie)
#' 
#' To run use \code{datapie_shiny()}
#'
#' @return A GUI for visual exploration of a dataset
#' 
#' @import ggplot2
#' @import shiny
#' @import shinyjs
#' @import readxl
#' @import haven
#' @import RColorBrewer
#' @import magrittr
#' @importFrom stringr str_replace_all
#' @importFrom readr read_delim
#' @export
#' 
datapie_shiny <- function() {
  
  # UI ------------------------------------------------------------------------
  
  ui <- fluidPage(
    
    # Allows app to print messages to the console for user understanding of
    # ongoing processes
    shinyjs::useShinyjs(),
    
    # UI title
    headerPanel("datapie"),

    # Define tabs
    mainPanel(
      width = 12,
      
      tabsetPanel(
        type = "tabs",
        
        # Tab - Data ----------------------------------------------------------
        tabPanel(
          "Data",
          
          # Options
          radioButtons(
            "data_input", 
            NULL,
            choices = list(
              "Use sample data" = 1,
              "Fetch data from DOI" = 2,
              "Upload data" = 3
            ),
            selected = 1
          ),
          
          # Load example data
          conditionalPanel(
            condition = "input.data_input=='1'",
            helpText("Sample data is loaded.")
          ),
          
          # Load DOI data
          conditionalPanel(
            condition = "input.data_input=='2'",
            textInput(
              inputId = "doi", 
              label = "Enter data package DOI:", 
              placeholder = "doi:10.18739/A2DP3X"
            ),
            actionButton("fetch_button", "Fetch Data"),
            shinyFiles::shinyDirButton("dir", "Save Data", "Upload"),
            p(),
            p(),
            textOutput("text"),
            selectInput(
              "repo_file", 
              "Select data object:", 
              choices = "",
              selected = "No object selected"
            )
          ),
          
          # Load data (local file)
          conditionalPanel(
            condition = "input.data_input=='3'",
            fileInput(
              "upload", 
              "Upload file from your computer:", 
              multiple = FALSE
            ),
            selectInput(
              "file_type", 
              "Type of file:",
              list("text (csv)" = "text"),
              selected = "text"
            ),
            conditionalPanel(
              condition = "input.file_type=='text'",
              selectInput(
                "upload_delim", 
                "Delimiter:",
                list(
                  "Comma" = ",",
                  "Tab" = "\t",
                  "Semicolon" = ";",
                  "Space" = " "
                ),
                selected = "Semicolon"
              )
            ),
            actionButton(
              "submit_datafile_button",
              "Submit File"
            )
          ),
          
          # Messaging
          hr(),
          dataTableOutput("out_table"),
          h3(textOutput("download_done_message")),
          textOutput("message_text")
          
        ),
        
        # Tab - Report --------------------------------------------------------
        
        tabPanel(
          "Report", 
          p(),
          
          # Create report
          conditionalPanel(
            condition = "input.tabs == 'Report'",
            conditionalPanel(
              htmlOutput("current_obj_text"),
              p(),
              condition = "input.report_to_display == 'None selected'",
              actionButton("generate_example_report", "Create Report"),
              p()
            ),
            
            # Select report to view
            selectInput(
              "report_to_display", 
              "Reports created in this session:",
              choices = "",
              selected = "None selected"
            ),
            p(),
            p(),
            
            # Download report
            downloadButton("download_report", "Download Report"),
            hr()
            
          ),
          
          # Print report to UI
          htmlOutput("report_html")
          
        ),
        
        # Tab - Plot ----------------------------------------------------------
        
        tabPanel(
          "Plot",

          # Place controls in a row of columns above the plot
          fluidRow(
            
            # Tab - Plot - Column 1 -------------------------------------------
            column(
              4,
              p(),
              
              # Plot type
              selectInput(
                inputId = "Type",
                label = "Plot type:",
                choices = c("None selected", "Boxplot", "Histogram", "Scatter"),
                selected = "None selected"
              ),

              # Histogram or Boxplot
              conditionalPanel(
                condition = "input.Type == 'Histogram' || input.Type == 'Boxplot' || input.Type == 'Scatter'",
                # X-variable
                selectInput(
                  inputId = "x_var",
                  label = "X-variable:",
                  choices = ""
                ),
                # X-variable coercion
                selectInput(
                  inputId = "x_cast",
                  label = "X-variable coercion:",
                  choices = c('None selected','character', 'numeric', 'date', 'factor'),
                  selected = "None selected"
                )
              ),
              
              # Scatter
              conditionalPanel(
                condition = "input.Type == 'Scatter'",
                # Y-variable
                selectInput(
                  inputId = "y_var",
                  label = "Y-variable:",
                  choices = ""
                ),
                # Y-variable coercion
                selectInput(
                  inputId = "y_cast",
                  label = "Y-variable coercion:",
                  choices = c('None selected','character', 'numeric', 'date', 'factor'),
                  selected = "None selected"
                )
              )
              
            ),
            
            # Tab - Plot - Column 2 -------------------------------------------
            column(
              3,
              p(),
              
              # Grouping
              selectInput(
                "group",
                "Group:",
                choices = ""
              ),
              
              # Group type
              conditionalPanel(
                condition = "input.Type == 'Scatter'",
                selectInput(
                  inputId = "group_type",
                  label = "Group type:",
                  choices = c("None selected", "factor", "continuous"),
                  selected = "None selected"
                )
              ),
              
              # Opacity
              conditionalPanel(
                condition = "input.Type == 'Scatter' || input.Type == 'Histogram' || input.Type == 'Boxplot'",
                sliderInput(
                  "input_alpha", 
                  "Opacity:", 
                  min = 0, 
                  max = 1, 
                  value = 0.8
                )
              )
              
            ),
            
            # Tab - Plot - Column 3 -------------------------------------------
            column(
              4,
              offset = 1,
              p(),
              
              # Title
              textInput(
                "title_value", 
                "Title:", 
                value = "Title"
              ),
              
              # Download
              downloadButton(
                "download_plot_PNG",
                "Download plot"
              )
              
            )
            
          ),
          
          # Tab - Plot - Plot area --------------------------------------------
          
          fluidRow(
            plotly:: plotlyOutput("out_plotly", height = "700px")
          )
          
        ),

        # Tab - Code ----------------------------------------------------------
        
        tabPanel(
          "Code", 
          p(),
          verbatimTextOutput("out_r_code")
        ),
        
        # Tab - Help ----------------------------------------------------------
        
        tabPanel(
          "Help",
          p(),
          shiny::includeMarkdown(
            system.file("/vignettes/help_tab.Rmd", package = "datapie")
          )
        ),
        
        id = "tabs"
        
      )#,
      
      # conditionalPanel(
      #   condition="$('html').hasClass('shiny-busy')",
      #   tags$div(
      #     "Loading...",
      #     id="loadmessage"
      #   )
      # )
      
    )
    
  )
  
  # SERVER --------------------------------------------------------------------
  
  server <- shinyServer(function(input, output, session) {
    
    # Set file size upload limit for locally uploaded files
    options(shiny.maxRequestSize = 5e9)
  
  # Get data from DOI ---------------------------------------------------------
    
    #Initialize the output that will be displayed using the "Fetch data from DOI" option
    # values$shiny_data is the object that most downstream functions will want to
    # use
    values <- reactiveValues(shiny_data = NULL)
  
    #Make repo download available to downstream app tools
    list_shiny <- eventReactive(input$fetch_button, {
      #Allow messages to be printed to the console
      withCallingHandlers({
        
        # empty out the "done" message box
        output$download_done_message <- renderText({return("")})
        
        #Initialize the package used to print messages
        shinyjs::html("message_text", "")
      #Read in data
      #In the future, a lot of this logic could be placed in the function
      # data_package_shiny_handler, thereby unifying the concepts expressed
      # below with said function.
        if(is.logical(all.equal(isolate(values$shiny_data), data_example))) {
          #This is required to make sure the app doesn't crash when the "Fetch
          # Data" button is pressed twice without an good doi.
          # cat("App condition 1\n") #Debugging
          data_list <- data_example
        } else if(!is.null(values$shiny_data)) {
          #The downloaded data is initially set to NULL, so it is easier to check
          # if 
          if(attr(isolate(values$shiny_data), "doi") == input$doi) {
            #Return the existing dataset when the same doi is input
            # cat("App condition 2A\n") #Debugging
           data_list <- isolate(values$shiny_data)
          } else if(is.null(input$doi) ||
              is.na(input$doi) ||
              nchar(input$doi) < 1){
            #Return the existing dataset when an invalid doi is entered
            # cat("App condition 2B\n") #Debugging
            data_list <- isolate(values$shiny_data)
          } else {
            #Otherwise download the data
            data_list <- data_package_shiny_handler(input$doi,
              isolate(values$shiny_data))
            # cat("App condition 2C\n") #Debugging
          }
        } else {
          # cat("App condition 3\n") #Debugging
          #If there is not data (NULL) then try to download the data package
          data_list <- data_package_shiny_handler(input$doi,
            isolate(values$shiny_data))
        }
      },
        #Generate the loading message
        message = function(m) {
          shinyjs::html(id = "message_text",
            html = paste(m$message, "<br>"),
            add = TRUE)
        })
      
      output$download_done_message <- renderText({return("Download and parsing is completed. Select a data object from the left menu to start. \n")})
      
      data_list
    }
      )
    
    #Update the values after the "Fetch data from DOI" button is pressed and the list_shiny
    # code is run.
    observeEvent(input$fetch_button, {
      #populate the object
      values$shiny_data <- list_shiny()
    })
    
    #Update the data package columns to be selected
    observe({
      #Extract the file names in the data package
      file_names <- names(list_shiny())
      
      #Use the file names to populate the dropdown list
      updateSelectInput(
        session,
        "repo_file",
        choices = c("No object selected", file_names),
        selected = 'No object selected')
    })
    
    # Download data package ---------------------------------------------------
    
    # Choose a local path to which the data package will be "downloaded" (the
    # data package is actually being copied form the tempdir() to a user 
    # specified location.
    shinyFiles::shinyDirChoose(input, 'dir', roots = c(home = '~'),
                               filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw"))
    global <- reactiveValues(datapath = getwd())
    dir <- reactive(input$dir)
    output$dir <- renderText({global$datapath})
    observeEvent(ignoreNULL = TRUE, eventExpr = {input$dir},
                 handlerExpr = {
                   if (!"path" %in% names(dir())) return()
                   home <- normalizePath("~")
                   global$datapath <-
                     file.path(home, paste(unlist(dir()$path[-1]), 
                                           collapse = .Platform$file.sep))})
    text_reactive <- eventReactive(input$dir, {
      suppressMessages(data_package_copy(global$datapath))})
    output$text <- renderText({
      if (global$datapath != getwd()){
        msg <- text_reactive()
        if (isTRUE(msg)){
          'Download complete'
        } else {
          'Data package already exists'
        }
      }
    })
    
    # Report generation and download ------------------------------------------
    
    # get name of current data object for
    
    current_obj <- reactive({
      if (input$data_input == 1) return("Sample data")
      if (input$data_input == 2) return(input$repo_file)
      if (input$data_input == 3) return("Input file")
    })
    
    output$current_obj_text <- renderText({paste0("Create report for the selected data object: <b>", current_obj(), "</b>. The report might take some time to generate.")})
    
    get_report <- 
      
      eventReactive(input$generate_example_report, {
        
        # create report output folder within tempdir
        temp_output <- file.path(tempdir(), "reports_output")
        
        # if using sample data 
        
        if (input$data_input == 1) {
          report_filename <- paste0("report_", data_example[[1]][["summary_metadata"]][1, 2], ".html")
          
          # check for existing report, otherwise call static_report_complete
          
          if (!file.exists(file.path(temp_output, report_filename))) {
            report_filename <-
              static_report_complete(
                entity_list = data_example[[1]],
                output_path = temp_output,
                shiny = T
              )
          }
          
          # handle download
          output$download_report <- downloadHandler(filename = report_filename,
                                                    content <- function(file) {
                                                      file.copy(file.path(temp_output, report_filename), file)
                                                    },
                                                    contentType = "text/HTML")
          
          # return HTML output report
          return(includeHTML(file.path(temp_output, report_filename)))
          
          # if using data from DOI
          
        } else if (input$data_input == 2) {
          
          if (is.null(list_shiny()[[input$repo_file]])) {
            return("In order to generate a report, please select a data table in this data package DOI from the drop-down menu in the Data tab.")
          } else {
            
          report_filename <- paste0("report_", list_shiny()[[input$repo_file]][["summary_metadata"]][1, 2], ".html")
          
          # check for existing report, otherwise call static_report_complete
          
          if (!file.exists(file.path(temp_output, report_filename))) {
            
            # get user-selected data entity from list of entities within package
            
          entity_list <- list_shiny()[[input$repo_file]]
          
            tryCatch(static_report_complete(entity_list = entity_list,
                                       output_path = temp_output,
                                       DOI = input$doi,
                                       shiny = T),
                     error = function(e) {
                       report_error <- e
                     })
          }
          
          # handle download 
          
          if (exists("report_error") && !is.null(report_error)) return(textOutput(report_error))
          else {
          output$download_report <- downloadHandler(filename = report_filename,
                                                    content <- function(file) {
                                                      file.copy(file.path(temp_output, report_filename), file)
                                                    },
                                                    contentType = "text/HTML")
          
          return(includeHTML(file.path(temp_output, report_filename)))
          }
          }
          
          # if using uploaded data
          
        } else if (input$data_input == 3) {
          file_in <- input$upload
          report_filename <-
            paste0("report_", "uploaded_data_", file_in$name, ".html")
          # mimic the list structure of metajam output
          entity_list <- list(data = df_shiny())
          
          # check for existing report, otherwise call static_report_complete
          
          if (!file.exists(file.path(temp_output, report_filename))) {
            tryCatch(
              static_report_complete(
                entity_list = entity_list,
                output_path = temp_output,
                shiny = T,
                report_filename = report_filename
              ),
              error = function(e) {
                report_error <- e
              }
            )
          }
          
          # handle download
          if (exists("report_error") &&
              !is.null(report_error))
            return(textOutput(report_error))
          else {
            output$download_report <-
              downloadHandler(filename = report_filename,
                              content <-
                                function(file) {
                                  file.copy(file.path(temp_output, report_filename), file)
                                },
                              contentType = "text/HTML")
            
            return(includeHTML(file.path(temp_output, report_filename)))
          }
        }
      })

    # render HTMl static report
    
    output$report_html <- renderUI({
      
      if ("None selected" == input$report_to_display){
        get_report()
      } else {
        output$download_report <- downloadHandler(filename = input$report_to_display,
                                                  content <- function(file) {
                                                    file.copy(file.path(tempdir(), "reports_output", input$report_to_display), file)
                                                  },
                                                  contentType = "text/HTML")
        includeHTML(file.path(tempdir(), "reports_output", input$report_to_display))

      }
    })
    
    # observe report files present in tempdir
    
    has.new.files <- function() {
      unique(list.files(file.path(tempdir(), "reports_output"), pattern = "*.html"))
    }
    get.files <- function() {
      list.files(file.path(tempdir(), "reports_output"), pattern = "*.html")
    }

    # store as a reactive instead of output
    my_files <- reactivePoll(10000, session, checkFunc=has.new.files, valueFunc=get.files)

    # any time the reactive changes, update the selectInput
    observeEvent(my_files(),ignoreInit = T,ignoreNULL = T, {
      reports <- my_files()
      updateSelectInput(
        session,
        "report_to_display",
        choices = c("None selected", reports),
        selected = 'None selected')
    })
    
  
    # observe report files present in tempdir
    # An's initial solution, which didn't update when new reports are made

    observe({
      #Extract the file names
      reports <- list.files(file.path(tempdir(), "reports_output"), pattern = "*.html")

      #Use the file names to populate the dropdown list
      updateSelectInput(
        session,
        "report_to_display",
        choices = c("None selected", reports),
        selected = 'None selected')
    })
    
  # Get variable names for input ----------------------------------------------
  
      observe({
        nms <- names(df_shiny())
        # Make list of variables that are not factors
        nms_cont <- names(Filter(function(x) is.integer(x) ||
                                   is.numeric(x) ||
                                   is.double(x),
                                 df_shiny()))
  
        # Make list of variables that are not factors
        nms_fact <- names(Filter(function(x) is.factor(x) ||
                                   is.logical(x) ||
                                   is.character(x),
                                 df_shiny()))
  
        # avail_all <- c("None selected" = ".", nms)
        avail_con <-
          if (identical(nms_cont, character(0)))
            c("No continuous vars available" = ".")
          else c(nms_cont)
        avail_fac <-
          if (identical(nms_fact, character(0)))
            c("No factors available" = ".")
          else c("None selected" = ".", nms_fact)
  
        # updateSelectInput(session, "y_var", choices = c("None selected" = "' '", nms))
        # updateSelectInput(session, "x_var", choices = c("None selected" = "' '", nms))
        updateSelectInput(session, "y_var", choices = c("None selected", nms))
        updateSelectInput(session, "x_var", choices = c("None selected", nms))
        updateSelectInput(session, "group", choices = c("None selected", nms))
        # updateSelectInput(session, "facet_row",  choices = avail_fac)
        # updateSelectInput(session, "facet_col",  choices = avail_fac)
      })
    
  # Read in / get data --------------------------------------------------------
  
      df_shiny <- reactive({
        if (input$data_input == 1) {
          data <- data_example[[1]]$data
        } else if (input$data_input == 2) {
          if(!exists("list_shiny")) {
            return(data.frame(x = "Enter DOI and press 'Fetch Data' button"))
          }
          else {
            data <- list_shiny()[[input$repo_file]]$data
          }
          
        } else if (input$data_input == 3) {
          file_in <- input$upload
          # Avoid error message while file is not uploaded yet
          if (is.null(input$upload)) {
            return(data.frame(x = "Select your datafile"))
          } else if (input$submit_datafile_button == 0) {
            return(data.frame(x = "Press 'submit data file' button"))
          } else {
            isolate({
              if (input$file_type == "text") {
                data <- read_delim(file_in$datapath,
                                   delim = input$upload_delim,
                                   col_names = TRUE)
              } else if (input$file_type == "Excel") {
                data <- read_excel(file_in$datapath)
              } else if (input$file_type == "SPSS") {
                data <- read_sav(file_in$datapath)
              } else if (input$file_type == "Stata") {
                data <- read_dta(file_in$datapath)
              } else if (input$file_type == "SAS") {
                data <- read_sas(file_in$datapath)
              }
            })
          }
        } 
        #   else if (input$data_input == 3) {
        #   if (input$data_paste == "") {
        #     data <- data.frame(x = "Copy your data into the textbox,
        #                        select the appropriate delimiter, and
        #                        press 'Submit data'")
        #   } else {
        #     if (input$submit_data_button == 0) {
        #       return(data.frame(x = "Press 'submit data' button"))
        #     } else {
        #       isolate({
        #         data <- read_delim(input$data_paste,
        #                            delim = input$text_delim,
        #                            col_names = TRUE)
        #       })
        #     }
        #   }
        # } 
        #   else if (input$data_input == 4){
        #   data <- dataset
        # }
        return(data)
      })
  
    # Graph code - sample or DOI ----------------------------------------------
    
    string_code <- reactive({
      
      write_plot_script <- function(x_var = "None selected", 
                                    y_var = "None selected", 
                                    Type = "None selected", 
                                    input_alpha = "None selected", 
                                    group = "None selected", 
                                    group_type = "None selected", 
                                    group2 = "None selected", 
                                    title_value = "None selected", 
                                    x_cast = "None selected", 
                                    y_cast = "None selected") {
        
        # Basic plots
        if (Type == "Scatter") {
          p <- "plotly::plot_ly(data = df, type = 'scatter', alpha = input_alpha, x = ~x_var, y = ~y_var, color = I('black'))"
        } else if (Type == "Histogram") {
          p <- "plotly::plot_ly(data = df, type = 'histogram', alpha = input_alpha, x = ~x_var, color = I('black'))"
        } else if (Type == "Boxplot") {
          p <- "plotly::plot_ly(data = df, type = 'box', alpha = input_alpha, y = ~x_var, color = I('black'))"
        }
        
        # Grouped plots
        if ((Type == "Scatter") & (group != "None selected")) {
          if (group_type == "factor") {
            p <- "df %>% dplyr::mutate(group = factor(group)) %>% plotly::plot_ly(type = 'scatter', alpha = input_alpha, x = ~x_var, y = ~y_var, color = ~group, colors = 'Dark2') %>% plotly::layout(annotations = list(yref = 'paper', xref = 'paper', y = 1.05, x = 1.1, text = 'group', showarrow = F))"
          } else if (group_type == "continuous") {
            p <- "df %>% dplyr::mutate(group = as.numeric(group)) %>% plotly::plot_ly(type = 'scatter', alpha = input_alpha, x = ~x_var, y = ~y_var, color = ~group)"
          }
        } else if ((Type == "Histogram") & (group != "None selected")) {
          p <- "df %>% dplyr::mutate(group = factor(group)) %>% plotly::plot_ly(type = 'histogram', alpha = input_alpha, x = ~x_var, color = ~group, colors = 'Dark2') %>% plotly::layout(barmode = 'overlay') %>% plotly::layout(annotations = list(yref = 'paper', xref = 'paper', y = 1.05, x = 1.1, text = 'group', showarrow = F))"
        } else if ((Type == "Boxplot") & (group != "None selected")) {
          if ((group != "None selected") & (group2 != "None selected")) {
            p <- "df %>% dplyr::mutate(group = factor(group)) %>% dplyr::mutate(group2 = factor(group2)) %>% plotly::plot_ly(type = 'box', alpha = input_alpha, x = ~group, y = ~x_var, color = ~group2, colors = 'Dark2') %>% plotly::layout(boxmode = 'group', annotations = list(yref = 'paper', xref = 'paper', y = 1.05, x = 1.1, text = 'group2', showarrow = F))"
          } else {
            p <- "df %>% dplyr::mutate(group = factor(group)) %>% plotly::plot_ly(type = 'box', alpha = input_alpha, y = ~x_var, color = ~group, colors = 'Dark2') %>% plotly::layout(showlegend = TRUE) %>% plotly::layout(annotations = list(yref = 'paper', xref = 'paper', y = 1.05, x = 1.1, text = 'group', showarrow = F))"
          }
          
        }
        
        # Coerce X and Y variables
        if (x_cast != "None selected") {
          if (x_cast == 'character') {
            p <- stringr::str_replace(p, "~x_var", "~as.character(x_var)")
          } else if (x_cast == 'numeric') {
            p <- stringr::str_replace(p, "~x_var", "~as.numeric(x_var)")
          } else if (x_cast == 'date') {
            p <- stringr::str_replace(p, "~x_var", "~as.Date(x_var)")
          } else if (x_cast == 'factor') {
            p <- stringr::str_replace(p, "~x_var", "~as.factor(x_var)")
          }
        }
        
        if (y_cast != "None selected") {
          if (y_cast == 'character') {
            p <- stringr::str_replace(p, "~y_var", "~as.character(y_var)")
          } else if (y_cast == 'numeric') {
            p <- stringr::str_replace(p, "~y_var", "~as.numeric(y_var)")
          } else if (y_cast == 'date') {
            p <- stringr::str_replace(p, "~y_var", "~as.Date(y_var)")
          } else if (y_cast == 'factor') {
            p <- stringr::str_replace(p, "~y_var", "~as.factor(x_var)")
          }
        }
        
        # Title
        if (title_value != "Title") {
          p <- paste(p, " %>% plotly::layout(title = 'title_value')")
        }
        
        # Performance enhancement with WebGL()
        p <- paste0(p, " %>% plotly::toWebGL()")
        
        # Replace variables with values
        p <- str_replace_all(
          p,
          c("y_var" = y_var,
            "x_var" = x_var,
            "group_type" = group_type,
            "group2" = group2,
            "(?<!boxmode[:space:]=[:space:].|legend)group" = group,
            "input_alpha" = as.character(input_alpha),
            "title_value" = as.character(title_value)
          )
        )
        
        # Make code legible
        p <- str_replace_all(p, ",\n    \\)", "\n  \\)")
        p
        
      }
      
      # write_plot_script(
      #   x_var = input$x_var, 
      #   y_var = input$y_var, 
      #   Type = input$Type, 
      #   input_alpha = input$input_alpha, 
      #   group = input$group, 
      #   group_type = input$group_type, 
      #   group2 = input$group2, 
      #   title_value = input$title_value, 
      #   x_cast = input$x_cast, 
      #   y_cast = input$y_cast
      # )
      
      write_plot_script(
        x_var = input$x_var, 
        y_var = input$y_var, 
        Type = input$Type, 
        input_alpha = input$input_alpha, 
        group = input$group,
        group_type = input$group_type,
        title_value = input$title_value,
        x_cast = input$x_cast,
        y_cast = input$y_cast
      )
      
    })
    
    # string_code <- reactive({
    # 
    #   # Variable used for how to deal with x/y in ggplot
    #   gg_x_y <- input$Type == "Histogram"
    # 
    #   # Variable used for how to deal with color/fill
    #   gg_fil <- input$Type == "Histogram" ||
    #     input$Type == "Scatter"
    # 
    #   p <- paste0(
    # 
    #     # # if at least one facet column/row is specified, add it
    #     # facets <- gsub("`.`",".",paste0('`',input$facet_row,'`',"~", '`',input$facet_col,'`'),fixed=T)
    #     # if (facets != ".~.")  # remove the space
    #     #   p <- paste(p, "+ facet_grid(", facets, ")")
    # 
    #     "plotly::plot_ly(type = ",
    #     if (input$Type == "Scatter") {
    #       "'scatter'"
    #     } else if (input$Type == "Histogram") {
    #       "'histogram'"
    #     } else if (input$Type == "Boxplot") {
    #       "'box'"
    #     },
    #     ")",
    # 
    #     # FIXME: Move this to the server
    #     # # Coerce X variable
    #     # if (input$x_cast == 'character') {
    #     #   "x = as.character(`input$x_var`)"
    #     # } else if (input$x_cast == 'numeric') {
    #     #   "x = as.numeric(`input$x_var`)"
    #     # } else if (input$x_cast == 'Date') {
    #     #   "x = as.Date(`input$x_var`)"
    #     # } else {
    #     #   "x = `input$x_var`"
    #     # },
    # 
    #     # FIXME: Move this to the server
    #     # # Coerce Y variable
    #     # if (!gg_x_y) {
    #     #   if(input$y_cast == 'character') {
    #     #     ", y = as.character(`input$y_var`)"
    #     #   } else if (input$y_cast == 'numeric') {
    #     #     ", y = as.numeric(`input$y_var`)"
    #     #   } else if (input$y_cast == 'Date') {
    #     #     ", y = as.Date(`input$y_var`)"
    #     #   } else {
    #     #     ", y = `input$y_var`"
    #     #   }
    #     # },
    # 
    #     #     "add_markers(x = ~`input$x_var`, y = ~`input$y_var`, color = I('black'), alpha = input$alpha) %>% ",
    # 
    #     if (input$Type == "Scatter") {
    #       " %>% add_markers(x = ~`input$x_var`, y = ~`input$y_var`, color = I('black'), alpha = input$alpha)"
    #     },
    # 
    #     # # if (input$group != "." && gg_fil) {
    #     # #   ", fill = `input$group`"
    #     # # } else if (input$group != "." && !gg_fil) {
    #     # #   ", color = `input$group`"
    #     # # },
    #     #
    #     # ")) + ",
    # 
    #     if (input$Type == "Histogram") {
    #       " %>% add_histogram(x = ~`input$x_var`, color = I('black'), alpha = input$alpha)"
    #     },
    # 
    #     if (input$Type == "Boxplot") {
    #       " %>% add_trace(y = ~`input$x_var`, color = I('black'), alpha = input$alpha)"
    #     }
    # 
    #     # FIXME:
    #     # if (input$Type == "Scatter" && input$line) {
    #     #   "+ geom_smooth(se = input$se, method = 'input$smooth')"
    #     # },
    # 
    #   )
    # 
    #   # Facet rows
    #   if (input$facet_row != "None selected") {
    #     p <- paste0(
    #       "df %>% split(df$`input$facet_row`) %>% lapply(function(x) {",
    #       p,
    #       "})",
    #       " %>% subplot(nrows = length(unique(df$`input$facet_row`)), shareX = TRUE)"
    #     )
    #   } else {
    #     p <- paste0("df %>% ", p)
    #   }
    # 
    #   # FIXME:
    #   # # if labels specified
    #   # if (input$label_axes)
    #   #   p <- paste(p, "+ labs(x = 'input$lab_x', y = 'input$lab_y')")
    # 
    #   # FIXME:
    #   # # if add_title specified
    #   # if (input$add_title)
    #   #   p <- paste(p, "+ ggtitle('input$add_title')")
    # 
    #   # FIXME:
    #   # # if legend specified
    #   # if (input$adj_leg == "Change legend")
    #   #   p <- paste0(p, "+ labs(",
    #   #               if (gg_fil) "fill" else "color",
    #   #               " = 'input$leg_ttl')")
    # 
    # 
    #   # FIXME:
    #   # # If a theme specified
    #   # p <- paste(p, "+", input$theme)
    # 
    #   # FIXME:
    #   # # If theme features are specified
    #   # if (input$adj_fnt_sz ||
    #   #     input$adj_fnt ||
    #   #     input$rot_txt ||
    #   #     input$adj_leg != "Keep legend as it is" ||
    #   #     input$adj_grd) {
    #   #   p <- paste(
    #   #     p,
    #   #     paste(
    #   #       " + theme(\n    ",
    #   #       if (input$adj_fnt_sz)
    #   #         "axis.add_title = element_text(size = input$fnt_sz_ttl),\n    ",
    #   #       if (input$adj_fnt_sz)
    #   #         "axis.text = element_text(size = input$fnt_sz_ax),\n    ",
    #   #       if (input$adj_fnt)
    #   #         "text = element_text(family = 'input$font'),\n    ",
    #   #       if (input$rot_txt)
    #   #         "axis.text.x = element_text(angle = 45, hjust = 1),\n    ",
    #   #       if (input$adj_leg == "Remove legend")
    #   #         "legend.position = 'none',\n    ",
    #   #       if (input$adj_leg == "Change legend")
    #   #         "legend.position = 'input$pos_leg',\n    ",
    #   #       if (input$grd_maj)
    #   #         "panel.grid.major = element_blank(),\n    ",
    #   #       if (input$grd_min)
    #   #         "panel.grid.minor = element_blank(),\n    ",
    #   #       ")",
    #   #       sep = ""
    #   #     ),
    #   #     sep = ""
    #   #   )
    #   # }
    # 
    #   # Enhance performance with WebGL()
    #   p <- paste0(p, " %>% plotly::toWebGL()")
    # 
    # 
    #   # Replace x_var and y_var with values
    #   p <- str_replace_all(
    #     p,
    #     c("input\\$y_var" = input$y_var,
    #       "input\\$y_cast" = input$y_cast,
    #       "input\\$x_var" = input$x_var,
    #       "input\\$x_cast" = input$x_cast,
    #       "input\\$group" = input$group,
    #       # "input\\$notch" = as.character(input$notch),
    #       "input\\$binwidth" = as.character(input$binwidth),
    #       # "input\\$adj_bw" = as.character(input$adj_bw),
    #       # "input\\$dot_dir" = as.character(input$dot_dir),
    #       "input\\$alpha" = as.character(input$alpha),
    #       "input\\$se" = as.character(input$se),
    #       "input\\$smooth" = as.character(input$smooth),
    #       # "input\\$size_jitter" = as.character(input$CI),
    #       "input\\$size_jitter" = as.character(input$size_jitter),
    #       "input\\$width_jitter" = as.character(input$width_jitter),
    #       "input\\$opac_jitter" = as.character(input$opac_jitter),
    #       "input\\$col_jitter" = as.character(input$col_jitter),
    #       "input\\$lab_x" = as.character(input$lab_x),
    #       "input\\$lab_y" = as.character(input$lab_y),
    #       "input\\$add_title" = as.character(input$add_title),
    #       "input\\$palet" = as.character(input$palet),
    #       "input\\$fnt_sz_ttl" = as.character(input$fnt_sz_ttl),
    #       "input\\$fnt_sz_ax" = as.character(input$fnt_sz_ax),
    #       "input\\$font" = as.character(input$font),
    #       "input\\$leg_ttl" = as.character(input$leg_ttl),
    #       "input\\$pos_leg" = as.character(input$pos_leg))
    #   )
    #   # Creates well-formatted Code for output
    #   p <- str_replace_all(p, ",\n    \\)", "\n  \\)")
    #   p
    # 
    # })
    
    # Graph code - File -------------------------------------------------------
    
    string_upload_code <- reactive({

      file_in <- input$upload

      if (input$data_input == 3) {

        # Avoid error message while file is not uploaded yet
        if (is.null(input$upload)) {
        } else if (input$submit_datafile_button == 0) {
        } else {
          isolate({
            if (input$file_type == "text") {
              p <- paste0(
                "readr::read_delim('", file_in$name, "', delim = '", input$upload_delim, "', ", "col_names = TRUE)"
              )
            } else if (input$file_type == "Excel") {
              p <- paste0(
                "readxl::read_excel(", file_in$name, ")"
              )
            } else if (input$file_type == "SPSS") {
              p <- paste0(
                "haven::read_sav(", file_in$name, ")"
              )
            } else if (input$file_type == "Stata") {
              p <- paste0(
                "haven::read_dta(", file_in$name, ")"
              )
            } else if (input$file_type == "SAS") {
              p <- paste0(
                "haven::read_sas(", file_in$name, ")"
              )
            }
          })
        }

      }
      p

    })
    
  # Graphical/table output ----------------------------------------------------
  
      output$out_table <- renderDataTable(
        df_shiny()
      )
      
      output$summary_table <- renderDataTable(
        df_shiny() %>%
          dplyr::select_if(is.numeric) %>%
          tidyr::gather(key="column_name", value='value') %>%
          dplyr::group_by(column_name) %>%
          dplyr::summarise(min=min(value, na.rm=TRUE), 
                    quatile_25 = quantile(value, 0.25, na.rm=TRUE),
                    quatile_50 = quantile(value, 0.50, na.rm=TRUE),
                    quatile_75 = quantile(value, 0.75, na.rm=TRUE),
                    max = max(value, na.rm=TRUE), 
                    unique_count = length(unique(value)),
                    finite_count = sum(is.finite(value)),
                    count = length(value))
      )
  
      width <- reactive({ input$fig_width })
      height <- reactive({ input$fig_height })
      width_download <- reactive({ input$fig_width_download })
      height_download <- reactive({ input$fig_height_download })
  
      # output$out_ggplot <- renderPlot(width = width,
      #                                 height = height, {
      #   # evaluate the string RCode as code
      #   df <- get_subset() #note: this is a subset of data from df_shiny as we select the range of interest()
      #   p <- eval(parse(text = string_code()))
      #   p
      # })
  
      output$out_plotly <- renderPlotly({
        # evaluate the string RCode as code
        df <- get_subset()#note: this is a subset of data from df_shiny as we select the range of interest()
        p <- eval(parse(text = string_code()))
        p
      })
  
  # Generate code for output --------------------------------------------------
  
      output$out_r_code <- renderText({
        
        # data sample r-code
        if(input$data_input == 1) {
          
          paste0(
            "# You can use the code below to make the 'Plot' tab figure.\n\n",
            "library(\'datapie\')\n",
            "df <- datapie::data_example\n",
            "p <- ",
            string_code(),
            "\np\n"
          )
          
        }
        
        # doi data r-code
        else if (input$data_input == 2) {

          paste0(
            "# You can use the code below to make the 'Plot' tab figure:\n\n",
            "library(\'datapie\')\n",
            "datapie::data_package_download(data.pkg.doi = '",
            input$doi,
            "')\n",
            "df_list <- datapie::data_package_read()\n",
            "df <- df_list$",
            input$repo_file,
            "$data\n",
            "p <- ",
            string_code(),
            "\np\n"
          )
          
        } else if (input$data_input == 3) {
          
          paste0(
            "# You can use the code below to make the 'Plot' tab figure:\n\n",
            "library(\'datapie\')\n",
            "df <-", string_upload_code(), "\n",
            "p <- ",
            string_code(), "\n",
            "p"
          )
          
        }

        })
      
      # Download graph --------------------------------------------------------
  
      output$download_plot_PNG <- downloadHandler(
        filename <- function() {
          paste("Figure_ggplotGUI_", Sys.time(), ".png", sep = "")
        },
        content <- function(file) {
          df <- df_shiny()
          p <- eval(parse(text = string_code()))
          plotly::export(p, file)
        },
        contentType = "application/png" # MIME type of the image
      )

  # Scale bar -----------------------------------------------------------------
  
     # X-variable
   output$data_range <- renderUI({
     # If missing input, return to avoid error later in function
     if(is.null(input$x_var))
       return()
     
     # Get the data set with the selected column
     df<-df_shiny()
     
     if (input$x_var!="") {
       df1 <-df[[input$x_var]]
       
       if (class(df1)[1]=="numeric") {
         sliderInput("range", "X-range:", min = min(df1), max = max(df1), value = c(min(df1),max(df1)))
       } else {
         #helpText("There is no scale bar for non-numerical variables.")
       }
     } else {return()}
   })
     
     # Y-variable
     output$data_range_y <- renderUI({
       # If missing input, return to avoid error later in function
       if(is.null(input$y_var))
         return()
       
       # Get the data set with the selected column
       df<-df_shiny()
       
       if (input$y_var!="") {
         df1 <-df[[input$y_var]]
         
         if (class(df1)[1]=="numeric") {
           sliderInput("range_y", "Y-range:", min = min(df1), max = max(df1), value = c(min(df1),max(df1)))
         } else {
           #helpText("There is no scale bar for non-numerical variables.")
         }
       } else {return()}
     })
  
  # Subset data from scale bar ------------------------------------------------
     
   get_subset <- reactive({
     min_value <- input$range[1]
     max_value <- input$range[2]
     
     df <- df_shiny()
     
     if (class(df[[input$x_var]])[1]=="numeric"&!is.null(min_value)) {
       df2<-df %>% plotly::filter(df[,input$x_var]>=min_value&df[,input$x_var]<=max_value)
     } else {
       df2<-df
     }
     # df2
     
     min_value <- input$range_y[1]
     max_value <- input$range_y[2]
     
     # df <- df_shiny()
     
     if (class(df2[[input$y_var]])[1]=="numeric"&!is.null(min_value)) {
       df3<-df2 %>% plotly::filter(df2[,input$y_var]>=min_value&df2[,input$y_var]<=max_value)
     } else {
       df3<-df2
     }
     
     df3
     
   })
     
  #for debugging purpose, uncomment the
     # output$text_output <- renderText({
     #   min_value <- input$range[1]
     #   max_value <- input$range[2]
     #   if (!is.null(min_value)) {
     #      paste("range is", min_value)
     #      } else {
     #        "no range"
     #      }
     # })
  
     # End R-session when browser closed
     session$onSessionEnded(stopApp)
  })
  
  # Construct the shinyApp ----------------------------------------------------
  shinyApp(ui, server)
  
}
