#' Creating a graphical user interface for creating ggplot-graphs.
#' 
#' To run use \code{datapie_shiny()}
#'
#' @param dataset A dataset (optional).
#' @return A GUI for visualizing data from \code{dataset}.

#' @import ggplot2
#' @import shiny
#' @import shinyjs
#' @import readxl
#' @import haven
#' @import RColorBrewer
#' @import magrittr
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom stringr str_replace_all
#' @importFrom readr read_delim
#' @export
datapie_shiny <- function( dataset = NA ) {
  
  ######### UI definition #############
  ui <- fluidPage(
    #Allows app to print messages to the console
    shinyjs::useShinyjs(),
    
    ###### Application title ########
    headerPanel("datapie"),
    
    ###### Left panel: loading data and main graphing options ########
    sidebarPanel(width = 3,
                 conditionalPanel(
                   condition = "input.tabs=='Data'",
                   h4("Data loading options"),
                   radioButtons(
                     "data_input", "",
                     choices = list("Use sample data" = 1,
                                    "Fetch data from DOI" = 2,
                                    "Upload data" = 3), #, "Paste text file" = 4), 
                     selected = 1),
                   conditionalPanel(
                     condition = "input.data_input=='1'",
                     h5("Sample data is loaded.")
                     ),
                   conditionalPanel(
                     condition = "input.data_input=='2'",
                     h5("Enter data package DOI:"),
                     textInput(inputId = "doi", label = NULL, placeholder = "doi:10.18739/A2DP3X"),
                     actionButton("fetch_button", "Fetch Data"),
                     shinyFiles::shinyDirButton("dir", "Save Data", "Upload"),
                     textOutput("text"),
                     selectInput("repo_file", "Select data object:", 
                                 choices = "",
                                 selected = "No object selected")
                     ),
                   conditionalPanel(
                     condition = "input.data_input=='3'",
                     h5("Upload file from your computer: "),
                     fileInput("upload", "", multiple = FALSE),
                     selectInput("file_type", "Type of file:",
                                 list("text (csv)" = "text",
                                      "Excel" = "Excel",
                                      "SPSS" = "SPSS",
                                      "Stata" = "Stata",
                                      "SAS" = "SAS"),
                                 selected = "text"),
                     conditionalPanel(
                       condition = "input.file_type=='text'",
                       selectInput("upload_delim", "Delimiter:",
                                   list("Semicolon" = ";",
                                        "Tab" = "\t",
                                        "Comma" = ",",
                                        "Space" = " "),
                                   selected = "Semicolon")
                       ),
                     actionButton("submit_datafile_button",
                                  "Submit datafile")
                     )
                   ),
                 conditionalPanel(
                   condition = "input.tabs == 'Report'",
                   h3("Overall and variable-wise summary tables and plots"),
                
                   p("List of reports generated in this session:"),
                   selectInput("report_to_display", "Select report:", 
                               choices = "",
                               selected = "None selected"),
                   hr(),
                   actionButton("generate_example_report", "Generate report"),
                   p("Click to generate report for the currently select data object. Note that the above drop-down menu should point to \"None selected\". The report might take some time to load. Meanwhile, feel free to navigate away from this tab."),
                   hr(),
                   downloadButton("download_report", "Download current (HTML)")
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs=='Plot' || input.tabs=='Interactive Plot'",
                   h4("Create visualization"),
                   selectInput(inputId = "Type",
                               label = "Type of graph:",
                               choices = c("Boxplot", "Histogram", "Scatter"),
                               selected = "Histogram"),
                   selectInput("x_var", "X-variable", choices = ""),
                   selectInput("x_cast", "X-coerce", choices = c('default','character', 'numeric', 'date')),
                   conditionalPanel(condition = "input.Type!='Histogram'", # "input.Type!='Density' && input.Type!='Histogram'",
                                    selectInput("y_var", "Y-variable", choices = ""),
                                    selectInput("y_cast", "Y-coerce", choices = c('default', 'character', 'numeric', 'date'))
                   ),
                   conditionalPanel(condition = "input.Type =='Histogram'", # "input.Type!='Density' && input.Type!='Histogram'",
                                    p("No Y-variable relevant")
                   ),
                   selectInput("group", "Group (or color)", choices = ""),
                   selectInput("facet_row", "Facet Row", choices = ""),
                   selectInput("facet_col", "Facet Column", choices = ""),
                   
                   
                   uiOutput("data_range"),
                   #textOutput("text_output"),
                   
                   conditionalPanel(
                     condition = "input.Type == 'Boxplot'", # "input.Type == 'Boxplot' || input.Type == 'Violin' || input.Type == 'Dot + Error'",
                     checkboxInput(inputId = "jitter",
                                   label = strong("Show data points (jittered)"),
                                   value = FALSE)
                   ),
                   conditionalPanel(
                     condition = "input.Type == 'Scatter' || input.Type == 'Histogram'", # "input.Type == 'Density' || input.Type == 'Histogram'",
                     sliderInput("alpha", "Opacity:", min = 0, max = 1, value = 0.8)
                   ),
                   conditionalPanel(
                     condition = "input.Type == 'Scatter'",
                     checkboxInput(inputId = "line",
                                   label = strong("Show regression line"),
                                   value = FALSE),
                     conditionalPanel(
                       condition = "input.line == true",
                       selectInput("smooth", "Smoothening function",
                                   choices = c("lm", "loess", "gam"))
                     ),
                     conditionalPanel(
                       condition = "input.line == true",
                       checkboxInput(inputId = "se",
                                     label = strong("Show confidence interval"),
                                     value = FALSE)
                     )
                   ),
                   downloadButton("download_plot_PDF",
                                  "Download pdf of figure"),
                   
                   downloadButton("download_plot_Tiff",
                                  "Download tiff of figure")
                   
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'Code'",
                   h4("R-Code for creating the static and interactive plots")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'Help'",
                   h4("Help me!")
                 ),
                 
                 conditionalPanel(
                   condition = "input.tabs == 'About'",
                   h4("About datapie")
                 )
    ),
    
    ########Define Tabs#########
    mainPanel(width = 6,
              tabsetPanel(
                type = "tabs",
                tabPanel("Data",
                         dataTableOutput("out_table"),
                         textOutput("message_text")),
                tabPanel("Report", 
                         #dataTableOutput("summary_table"),
                         htmlOutput("report_html")
                ),
                tabPanel("Plot",
                         mainPanel(plotOutput("out_ggplot"))),
                tabPanel("Interactive Plot", plotlyOutput("out_plotly")),
                tabPanel("Code", verbatimTextOutput("out_r_code")),
                tabPanel("Help",
                         p("This is place holder text")),
                tabPanel("About",
                         h3(""),
                         p("Welcome to the Environmental Data Initiative’s web 
                           application to aid data description and exploration."),
                         h3("About the Environmental Data Initiative"),
                         p("Funded by the National Science Foundation (NSF), the 
                           Environmental Data Initiative’s (EDI) goal is to support 
                           the environmental and ecological research community by (1) 
                           accelerating the pace of information management and (2) 
                           providing a robust and secure data repository that facilitates 
                           data discovery and reuse. One barrier to information management 
                           in the ecological community is the need for information 
                           managers to regularly write new code for new information 
                           management tasks. Unfortunately, there is little reuse of 
                           this code. To improve discoverability and access of 
                           information management code, EDI initiated the Information 
                           Management Code Registry (IMCR), where users can search over 
                           80 entries in the registry for software with particular 
                           information management uses. IMCR is hosted in Ontosoft, a 
                           metadata authoring and discovery system for software that 
                           was developed by NSF’s EarthCube project. The IMCR has become 
                           a community-supported resource, with considerable input from 
                           scientists and informaticians affiliated with the Earth Science 
                           Information Partners IMCR group. More specifically, when the 
                           community identifies a need for software to manage or re-use 
                           data housed in data repositories, EDI often supports hackathons 
                           to produce this code, as is the case with", code("datapie"), "."),
                         p("For inforation about EDI, please visit: ", 
                           a("https://environmentaldatainitiative.org/", href = "https://environmentaldatainitiative.org/")),
                         p("For information about IMCR, please visit: ", 
                           a("http://wiki.esipfed.org/index.php/IM_Code_Registry", href = "http://wiki.esipfed.org/index.php/IM_Code_Registry")),
                         h3("About the 2019 Environmental Data Initiative Hackathon"),
                         p("The goal of the 2019 EDI Hackathon was to improve methods to 
                           visualize data from ", a("DataOne", href = "https://www.dataone.org/"), 
                           " and other environmental data repositories. This new code 
                           and associated software tool would be available through IMCR. 
                           Also, it would encourage more scientists to review and 
                           interpret publicly available environmental datasets, and 
                           ultimately, advance environmental and ecology research."),
                         h3("About the R datapie Package"),
                         p(code("datapie"), "which stands for Data Package Interface (for) 
                           Evaluation (“Easy as pie!”), was developed through the 2019 
                           EDI Hackathon event which occurred June 9 to 13, 2019 in 
                           Albuquerque, New Mexico. The R package was first released on 
                           August 8, 2019."),
                         p("The", code("datapie"), "R package and interactive web application 
                           help researchers and other data users who wish to reuse existing 
                           data packages that are archived on DataOne or an ", a("affiliated member node.", href = "https://www.dataone.org/current-member-nodes#uploads"),
                           "The interactive web application (1) downloads identified 
                           DOIs registered on DataOne or member nodes, (2) reads data into 
                           the web application, (3) provides summary statistics and basic graphics 
                           describing datasets associated with the DOI, and (4) 
                           generates a report describing the data. This is not intended 
                           to replace a full analysis in R or comparable statistical 
                           packages; however, it is intended to allow the user to 
                           quickly assess whether data is suitable for their research needs."),
                         p("For the web application quick start guide, please visit the 
                           “Help” tab above."),
                         p("For detailed information on the", code("datapie"), 
                           "R package, including background, use, and how to cite it, 
                           please visit: ", 
                           a("https://imcr-hackathon.github.io/datapie/", href = "https://imcr-hackathon.github.io/datapie/")),
                         p("To report a bug please submit an issue on GitHub here: ", 
                           a("https://github.com/IMCR-Hackathon/datapie/issues", href = "https://github.com/IMCR-Hackathon/datapie/issues")),
                         h3("Contributors (in alphabetical order)"),
                         p("Alesia Hallmark, Li Kui, Jason Mercer, An T. Nguyen, John Porter, 
                           Sheila Saia, Colin Smith*, Kathe Todd-Brown, Kristin Vanderbilt*, and 
                           Jocelyn Wardrup"),
                         p("*Indicates package maintainer(s). Please visit ", a("https://imcr-hackathon.github.io/datapie/", href = "https://imcr-hackathon.github.io/datapie/"),
                           " for up-to-date contact information."),
                         h3("Acknowledgements"),
                         p("This work adheres to the Findable Accessible Interoperable Reusable (FAIR) 
                           initiative. We thank Dr. Gert Stulp, the author of the", code("ggplotgui"), "R package, 
                           for sharing source code that we used (with Dr. Stulp’s permission) to meet 
                           our project goals. Dr. Stulp's application is accessible here: ", 
                           a("https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/", href = "https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/")),
                         h3("")
                         ),
                id = "tabs"
                         ),
              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                               tags$div("Loading...",id="loadmessage"))
                         ), #close mainPanel
    
    ####### Right Panel ########
    conditionalPanel(
      condition = "input.tabs=='Plot' || input.tabs=='Interactive Plot'",
      sidebarPanel(
        width = 3,
        h4("Change aesthetics"),
        tabsetPanel(
          tabPanel(
            "Text",
            checkboxInput(inputId = "label_axes",
                          label = strong("Change labels axes"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = "label x-axis")
            ),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_y", "Y-axis:", value = "label y-axis")
            ),
            checkboxInput(inputId = "add_title",
                          label = strong("Add title"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.add_title == true",
              textInput("title", "Title:", value = "Title")
            ),
            checkboxInput(inputId = "adj_fnt_sz",
                          label = strong("Change font size"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_ttl",
                           "Size axis titles:",
                           value = 12),
              numericInput("fnt_sz_ax",
                           "Size axis labels:",
                           value = 10)
            ),
            checkboxInput(inputId = "rot_txt",
                          label = strong("Rotate text x-axis"),
                          value = FALSE),
            checkboxInput(inputId = "adj_fnt",
                          label = strong("Change font"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt == true",
              selectInput("font", "Font",
                          choices = c("Courier",
                                      "Helvetica",
                                      "Times"),
                          selected = "Helvetica")
            )
          ),
          tabPanel(
            "Theme",
            conditionalPanel(
              condition = "input.group != '.'",
              checkboxInput(inputId = "adj_col",
                            label = strong("Change colors"),
                            value = FALSE),
              conditionalPanel(
                condition = "input.adj_col",
                selectInput(inputId = "palet",
                            label = strong("Select palette"),
                            choices = list(
                              "Qualitative" = c("Accent",
                                                "Dark2",
                                                "Paired",
                                                "Pastel1",
                                                "Pastel2",
                                                "Set1",
                                                "Set2",
                                                "Set3"),
                              "Diverging" = c("BrBG",
                                              "PiYG",
                                              "PRGn",
                                              "PuOr",
                                              "RdBu",
                                              "RdGy",
                                              "RdYlBu",
                                              "RdYlGn",
                                              "Spectral"),
                              "Sequential" = c("Blues",
                                               "BuGn",
                                               "BuPu",
                                               "GnBu",
                                               "Greens",
                                               "Greys",
                                               "Oranges",
                                               "OrRd",
                                               "PuBu",
                                               "PuBuGn",
                                               "PuRd",
                                               "Purples",
                                               "RdPu",
                                               "Reds",
                                               "YlGn",
                                               "YlGnBu",
                                               "YlOrBr",
                                               "YlOrRd")),
                            selected = "set1")
              )
            ),
            conditionalPanel(
              condition = "input.jitter",
              checkboxInput("adj_jitter",
                            strong("Change look jitter"), FALSE),
              conditionalPanel(
                condition = "input.adj_jitter",
                textInput("col_jitter", "Colour (name or RGB):",
                          value = "black"),
                numericInput("size_jitter", "Size:", value = 2),
                sliderInput("opac_jitter", "Opacity:",
                            min = 0, max = 1, value = 0.5, step = 0.01),
                sliderInput("width_jitter", "Width jitter:",
                            min = 0, max = 0.5, value = 0.25, step = 0.01)
              )
            ),
            checkboxInput("adj_grd",
                          strong("Remove gridlines"), FALSE),
            conditionalPanel(
              condition = "input.adj_grd",
              checkboxInput("grd_maj",
                            strong("Remove major gridlines"), FALSE),
              checkboxInput("grd_min",
                            strong("Remove minor gridlines"), FALSE)
            ),
            selectInput("theme", "Theme",
                        choices = c("bw" = "theme_bw()",
                                    "classic" = "theme_classic()",
                                    "dark" = "theme_dark()",
                                    "grey" = "theme_grey()",
                                    "light" = "theme_light()",
                                    "line_draw" = "theme_linedraw()",
                                    "minimal" = "theme_minimal()"),
                        selected = "theme_bw()")
          ),
          tabPanel(
            "Legend",
            conditionalPanel(
              condition = "input.group != '.'",
              radioButtons(inputId = "adj_leg",
                           label = NULL,
                           choices = c("Keep legend as it is",
                                       "Remove legend",
                                       "Change legend"),
                           selected = "Keep legend as it is"),
              conditionalPanel(
                condition = "input.adj_leg=='Change legend'",
                textInput("leg_ttl", "Title legend:",
                          value = "title legend"),
                selectInput("pos_leg", "Position legend",
                            choices = c("right",
                                        "left",
                                        "top",
                                        "bottom"))
              )
            )
          ),
          tabPanel(
            "Size",
            checkboxInput("fig_size",
                          strong("Adjust plot size on screen"), FALSE),
            conditionalPanel(
              condition = "input.fig_size",
              numericInput("fig_height", "Plot height (# pixels): ",
                           value = 480),
              numericInput("fig_width", "Plot width (# pixels):", value = 480)
            ),
            checkboxInput("fig_size_download",
                          strong("Adjust plot size for download"), FALSE),
            conditionalPanel(
              condition = "input.fig_size_download",
              numericInput("fig_height_download",
                           "Plot height (in cm):", value = 14),
              numericInput("fig_width_download",
                           "Plot width (in cm):", value = 14)
            )
          )
        ) # Close tabsetPanel
      ) # Close sidebarPanel
    ) # Close conditionalPanel
    
    #######
  ) #end fluidPanel
  
  ############Server function ############
  server <- shinyServer(function(input, output, session) {
  
  #####################################
  ######### GET DATA FROM DOI #########
  #####################################
    
    #Initialize the output that will be displayed using the "Fetch data from DOI" option
    # values$shiny_data is the object that most downstream functions will want to
    # use
    values <- reactiveValues(shiny_data = NULL)
  
    #Make repo download available to downstream app tools
    list_shiny <- eventReactive(input$fetch_button, {
      #Allow messages to be printed to the console
      withCallingHandlers({
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
    
    ################################################
    ########## DOWNLOAD DATA PACKAGE ###############
    ################################################
    
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
    
    ################################################
    ##### generate and show static report ##########
    ################################################
    
    
    get_report <- 
    # function() {
      # if (!"No object selected" == input$report_to_display) {
      #   return(includeHTML(
      #     file.path(tempdir(), "reports_output", input$report_to_display)
      #   ))
      # } else {
      #   
      # do all this once "generate report" is clicked
      
      #return(input$report_to_display)
      
      eventReactive(input$generate_example_report, {
        
        # ---
        # create report output folder within tempdir
        temp_output <- file.path(tempdir(), "reports_output")
        
        # -------
        # if using sample data 
        
        if (input$data_input == 1) {
          report_filename <- paste0("report_", data_example[[1]][["summary_metadata"]][1, 2], ".html")
          
          # ---
          # check for existing report, otherwise call static_report_complete
          
          if (!file.exists(file.path(temp_output, report_filename))) {
            report_filename <-
              static_report_complete(
                entity_list = data_example[[1]],
                output_path = temp_output,
                shiny = T
              )
          }
          
          # ---
          # handle download
          output$download_report <- downloadHandler(filename = report_filename,
                                                    content <- function(file) {
                                                      file.copy(file.path(temp_output, report_filename), file)
                                                    },
                                                    contentType = "text/HTML")
          
          # ---
          # return HTML output report
          return(includeHTML(file.path(temp_output, report_filename)))
          
          # ------
          # if using data from DOI
          
        } else if (input$data_input == 2) {
          
          if (is.null(list_shiny()[[input$repo_file]])) {
            return("In order to generate a report, please select a data table in this data package DOI from the drop-down menu in the Data tab.")
          } else {
            
          report_filename <- paste0("report_", list_shiny()[[input$repo_file]][["summary_metadata"]][1, 2], ".html")
          
          # ---
          # check for existing report, otherwise call static_report_complete
          
          if (!file.exists(file.path(temp_output, report_filename))) {
            
            # ---
            # get user-selected data entity from list of entities within package
            
          entity_list <- list_shiny()[[input$repo_file]]
          
          report_filename <-
            try(static_report_complete(entity_list = entity_list,
                                       output_path = temp_output,
                                       shiny = T))
          }
          
          # ---
          # handle download 
          
          output$download_report <- downloadHandler(filename = report_filename,
                                                    content <- function(file) {
                                                      file.copy(file.path(temp_output, report_filename), file)
                                                    },
                                                    contentType = "text/HTML")
          return(includeHTML(file.path(temp_output, report_filename)))
          }
          # ------
          # if using uploaded data, output message
          
        } else if (input$data_input == 3) {
          return("Sorry, we don't currently support report generation for user-uploaded data.")
        }
      })

    # render HTMl static report
    
    output$report_html <- renderUI({
      if("None selected" == input$report_to_display){
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
    
  #####################################
  ### GET VARIABLE NAMES FOR INPUT ####
  #####################################
  
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
  
        avail_all <- c("No groups" = ".", nms)
        avail_con <-
          if (identical(nms_cont, character(0)))
            c("No continuous vars available" = ".")
          else c(nms_cont)
        avail_fac <-
          if (identical(nms_fact, character(0)))
            c("No factors available" = ".")
          else c("No groups" = ".", nms_fact)
  
        updateSelectInput(session, "y_var", choices = c("No y-var" = "' '", nms))
        updateSelectInput(session, "x_var", choices = nms)
        updateSelectInput(session, "group", choices = avail_all)
        updateSelectInput(session, "facet_row",  choices = avail_fac)
        updateSelectInput(session, "facet_col",  choices = avail_fac)
      })
  
  
  #####################################
  ###### READ IN / GET DATA ###########
  #####################################
  
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
            return(data.frame(x = "Press 'submit datafile' button"))
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
  
  #####################################
  ####### CREATE GRAPH-CODE ###########
  #####################################
  
      string_code <- reactive({
  
        # Variable used for how to deal with x/y in ggplot
        gg_x_y <- input$Type == "Histogram" ||
                  input$Type == "Density"
        # Variable used for how to deal with color/fill
        gg_fil <- input$Type == "Histogram" ||
                  input$Type == "Density" ||
                  input$Type == "Dotplot"
  
        
        # Only plot jitter when graphs allow them
        if (gg_fil || input$Type == "Scatter")
          jitt <- FALSE else jitt <- input$jitter
        
        p <- paste(
          "ggplot(df, aes(",
          if(input$x_cast == 'character'){
            "x = as.character(`input$x_var`)"
          }else if(input$x_cast == 'numeric'){
            "x = as.numeric(`input$x_var`)"
          }else if(input$x_cast == 'Date'){
            "x = as.Date(`input$x_var`)"
          }else{
            "x = `input$x_var`"
          },
          if (!gg_x_y) {
            if(input$y_cast == 'character'){
              ", y = as.character(`input$y_var`)"
            }else if(input$y_cast == 'numeric'){
              ", y = as.numeric(`input$y_var`)"
            }else if(input$y_cast == 'Date'){
              ", y = as.Date(`input$y_var`)"
            }else{
              ", y = `input$y_var`"
            }
          },
          if (input$group != "." && gg_fil){
            ", fill = `input$group`"
          } else if (input$group != "." && !gg_fil){
            ", color = `input$group`"
          },
          ")) + ",
          if (input$Type == "Histogram"){
            if(is.numeric(df_shiny()[,input$x_var])){
              paste("geom_histogram(alpha = input$alpha, ",
                    "binwidth = input$binwidth)", sep = "")
            }else{
              paste("geom_histogram(alpha = input$alpha, stat='count')", sep = "")
            }
          },
          if (input$Type == "Boxplot"){
            "geom_boxplot()"
          },
          if (input$Type == "Scatter"){
            "geom_point(alpha = input$alpha, size = 2)"
          },
          if (input$Type == "Scatter" && input$line){
            "+ geom_smooth(se = input$se, method = 'input$smooth')"
          },
          if (jitt){
            paste(" + geom_jitter(size = input$size_jitter, ",
                  "alpha = input$opac_jitter, width = input$width_jitter, ",
                  "color = 'input$col_jitter')", sep = "")
          },
          sep = ""
        )
        # p <- paste(
        #   "ggplot(df, aes(",
        #     if(input$x_cast == 'character'){
        #       "x = as.character(input$x_var)"
        #     }else if(input$x_cast == 'numeric'){
        #       "x = as.numeric(input$x_var)"
        #     }else if(input$x_cast == 'Date'){
        #       "x = as.Date(input$x_var)"
        #     }else{
        #       "x = input$x_var"
        #     },
        #   if (!gg_x_y) {
        #     if(input$y_cast == 'character'){
        #       ", y = as.character(input$y_var)"
        #     }else if(input$y_cast == 'numeric'){
        #       ", y = as.numeric(input$y_var)"
        #     }else if(input$y_cast == 'Date'){
        #       ", y = as.Date(input$y_var)"
        #     }else{
        #       ", y = input$y_var"
        #     }
        #   },
        #   if (input$group != "." && gg_fil) {
        #     ", fill = input$group"
        #   } else if (input$group != "." && !gg_fil) {
        #     ", color = input$group"
        #   },
        #   ")) + ",
        #   if (input$Type == "Histogram")
        #     if(is.numeric(df_shiny()[,input$x_var])){
        #       paste("geom_histogram(alpha = input$alpha, ",
        #             "binwidth = input$binwidth)", sep = "")
        #     }else{
        #       paste("geom_histogram(alpha = input$alpha, stat='count')", sep = "")
        #       },
        #   # if (input$Type == "Density")
        #   #   paste("geom_density(position = 'identity', alpha = input$alpha, ",
        #   #         "adjust = input$adj_bw)", sep = ""),
        #   if (input$Type == "Boxplot")
        #       "geom_boxplot()",
        #   #   "geom_boxplot(notch = input$notch)",
        #   # if (input$Type == "Violin")
        #   #   "geom_violin(adjust = input$adj_bw)",
        #   # if (input$Type == "Dotplot")
        #   #   paste("geom_dotplot(binaxis = 'y', binwidth = input$binwidth, ",
        #   #         "stackdir = 'input$dot_dir')", sep = ""),
        #   # if (input$Type == "Dot + Error")
        #   #   paste("geom_point(stat = 'summary', fun.y = 'mean') +\n  ",
        #   #         "geom_errorbar(stat = 'summary', fun.data = 'mean_se', ", "
        #   #         width=0, fun.args = list(mult = input$CI))", sep = ""),
        #   if (input$Type == "Scatter")
        #     "geom_point(alpha = input$alpha, size = 2)",
        #   if (input$Type == "Scatter" && input$line)
        #     "+ geom_smooth(se = input$se, method = 'input$smooth')",
        #   if (jitt)
        #     paste(" + geom_jitter(size = input$size_jitter, ",
        #           "alpha = input$opac_jitter, width = input$width_jitter, ",
        #           "color = 'input$col_jitter')", sep = ""),
        #   sep = ""
        # )
  
        # if at least one facet column/row is specified, add it
        facets <- gsub("`.`",".",paste0('`',input$facet_row,'`',"~", '`',input$facet_col,'`'),fixed=T)
        if (facets != ".~.")  # remove the space
          p <- paste(p, "+ facet_grid(", facets, ")")
  
        # if labels specified
        if (input$label_axes)
          p <- paste(p, "+ labs(x = 'input$lab_x', y = 'input$lab_y')")
  
        # if title specified
        if (input$add_title)
          p <- paste(p, "+ ggtitle('input$title')")
  
        # if legend specified
        if (input$adj_leg == "Change legend")
          p <- paste(p, "+ labs(",
                     if (gg_fil) "fill" else "color",
                     " = 'input$leg_ttl')",
                     sep = "")
  
        # if color legend specified
        if (input$adj_col)
          p <- paste(p, "+ scale_",
                     if (gg_fil) "fill" else "color",
                     "_brewer(palette = 'input$palet')",
                     sep = "")
  
        # If a theme specified
        p <- paste(p, "+", input$theme)
  
        # If theme features are specified
        if (input$adj_fnt_sz ||
            input$adj_fnt ||
            input$rot_txt ||
            input$adj_leg != "Keep legend as it is" ||
            input$adj_grd) {
          p <- paste(
            p,
            paste(
              " + theme(\n    ",
              if (input$adj_fnt_sz)
  "axis.title = element_text(size = input$fnt_sz_ttl),\n    ",
              if (input$adj_fnt_sz)
  "axis.text = element_text(size = input$fnt_sz_ax),\n    ",
              if (input$adj_fnt)
  "text = element_text(family = 'input$font'),\n    ",
              if (input$rot_txt)
  "axis.text.x = element_text(angle = 45, hjust = 1),\n    ",
              if (input$adj_leg == "Remove legend")
  "legend.position = 'none',\n    ",
              if (input$adj_leg == "Change legend")
  "legend.position = 'input$pos_leg',\n    ",
              if (input$grd_maj)
  "panel.grid.major = element_blank(),\n    ",
              if (input$grd_min)
  "panel.grid.minor = element_blank(),\n    ",
  ")",
              sep = ""
            ),
            sep = ""
          )
        }
  
        # Replace name of variables by values
        p <- str_replace_all(
               p,
               c("input\\$y_var" = input$y_var,
                 "input\\$y_cast" = input$y_cast,
                 "input\\$x_var" = input$x_var,
                 "input\\$x_cast" = input$x_cast,
                 "input\\$group" = input$group,
                 # "input\\$notch" = as.character(input$notch),
                 "input\\$binwidth" = as.character(input$binwidth),
                 # "input\\$adj_bw" = as.character(input$adj_bw),
                 # "input\\$dot_dir" = as.character(input$dot_dir),
                 "input\\$alpha" = as.character(input$alpha),
                 "input\\$se" = as.character(input$se),
                 "input\\$smooth" = as.character(input$smooth),
                 # "input\\$CI" = as.character(input$CI),
                 "input\\$size_jitter" = as.character(input$size_jitter),
                 "input\\$width_jitter" = as.character(input$width_jitter),
                 "input\\$opac_jitter" = as.character(input$opac_jitter),
                 "input\\$col_jitter" = as.character(input$col_jitter),
                 "input\\$lab_x" = as.character(input$lab_x),
                 "input\\$lab_y" = as.character(input$lab_y),
                 "input\\$title" = as.character(input$title),
                 "input\\$palet" = as.character(input$palet),
                 "input\\$fnt_sz_ttl" = as.character(input$fnt_sz_ttl),
                 "input\\$fnt_sz_ax" = as.character(input$fnt_sz_ax),
                 "input\\$font" = as.character(input$font),
                 "input\\$leg_ttl" = as.character(input$leg_ttl),
                 "input\\$pos_leg" = as.character(input$pos_leg))
        )
        # Creates well-formatted Code for output
        p <- str_replace_all(p, ",\n    \\)", "\n  \\)")
  
        p
      })
  
  
  #####################################
  ###### GRAPHICAL/TABLE OUTPUT #######
  #####################################
  
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
  
      output$out_ggplot <- renderPlot(width = width,
                                      height = height, {
        # evaluate the string RCode as code
        df <- get_subset() #note: this is a subset of data from df_shiny as we select the range of interest()
        p <- eval(parse(text = string_code()))
        p
      })
  
      output$out_plotly <- renderPlotly({
        # evaluate the string RCode as code
        df <- get_subset()#note: this is a subset of data from df_shiny as we select the range of interest()
        p <- eval(parse(text = string_code()))
        ggplotly(p)
      })
  
  #####################################
  #### GENERATE R-CODE FOR OUTPUT #####
  #####################################
  
      output$out_r_code <- renderText({
        
        # data sample r-code
        if(input$data_input == 1) {
          gg_code <- string_code()
          gg_code <- str_replace_all(gg_code, "\\+ ", "+\n  ")
          
          paste(
            "## You can use the code below to make the 'Plot' tab figure.\n\n",
            "# You will need the following package(s):\n",
            "library(\"datapie\")\n",
            "library(\"ggplot2\")\n\n",
            "# The code below will load the sample data from\n",
            "# library(datapie) into your current R session.\n",
            "# df <- datapie::data_example\n\n",
            "# The code below will generate the 'Plot' tab figure.\n",
            "graph <- ",
            gg_code,
            "\ngraph\n\n",
            "# If you want the plot to be interactive,\n",
            "# you need the following package(s):\n",
            "library(\"plotly\")\n\n",
            "# The code below will generate the 'Interactive Plot'\n",
            "# tab figure.\n",
            "ggplotly(graph)\n\n",
            "# The code below will save your plot.\n",
            "ggsave('my_graph.pdf', graph, width = ",
            width_download(),
            ", height = ",
            height_download(),
            ", units = 'cm')",
            sep = ""
          )
        }
        
        # doi data r-code
        else if (input$data_input == 2) {
          gg_code <- string_code()
          gg_code <- str_replace_all(gg_code, "\\+ ", "+\n  ")
          
          paste(
            "## You can use the code below to make the 'Plot' tab figure.\n\n",
            "# You will need the following package(s):\n",
            "library(\"datapie\")\n",
            "library(\"ggplot2\")\n\n",
            "# The code below will download data from a DOI to a temporary\n",
            "# directory on your computer. The DOI in the code below is\n",
            "# the DOI you entered in the 'Data' tab.\n",
            "datapie::data_package_download(data.pkg.doi = '",
            input$doi,
            "')\n\n",
            "# The code below will save the data from the the DOI you\n",
            "# entered in the 'Data' tab to your current R session.\n",
            "df_list <- datapie::data_package_read()\n\n",
            "# The code below will create a variable df to hold the data\n",
            "# file you selected in the 'Data' tab.\n",
            "df <- df_list$",
            input$repo_file,
            "$data\n\n",
            "# The code below will generate the 'Plot' tab figure:\n",
            "graph <- ",
            gg_code,
            "\ngraph\n\n",
            "# If you want the plot to be interactive,\n",
            "# you need the following package(s):\n",
            "library(\"plotly\")\n\n",
            "# The code below will generate the 'Interactive Plot'\n",
            "# tab figure.\n",
            "ggplotly(graph)\n\n",
            "# The code below will save your plot.\n",
            "ggsave('my_graph.pdf', graph, width = ",
            width_download(),
            ", height = ",
            height_download(),
            ", units = 'cm')",
            sep = ""
          )
        }
        
        # uploaded data r-code
        else {
          paste("We do not support code generation from uploaded\n",
                "data at this time.")
          }
        })

  #####################################
  #### Download codes #################
  #####################################
  
    output$download_plot_PDF <- downloadHandler(
        filename <- function() {
          paste("Figure_ggplotGUI_", Sys.time(), ".pdf", sep = "")
        },
        content <- function(file) {
          df <- df_shiny()
          p <- eval(parse(text = string_code()))
          ggsave(file, p, width = width_download(),
                 height = height_download(), units = "cm")
        },
        contentType = "application/pdf" # MIME type of the image
    )
    output$download_plot_Tiff <- downloadHandler(
        filename <- function() {
          paste("Figure_ggplotGUI_", Sys.time(), ".tiff", sep = "")
        },
        content <- function(file) {
          df <- df_shiny()
          p <- eval(parse(text = string_code()))
          ggsave(file, p, width = width_download(),
                 height = height_download(), units = "cm")
        },
        contentType = "application/tiff" # MIME type of the image
      )
    # FIXME: Download interactive plots. These objects need to be conditionally
    # linked to actions occuring on the interactive plot tab. This could be 
    # done by refactoring the left pannel in two separate conditions rather
    # than the combined approach currently used.
    #
    # output$download_interactive_plot_PDF <- downloadHandler(
    #   filename <- function() {
    #     paste("Figure_ggplotlyGUI_", Sys.time(), ".pdf", sep = "")
    #   },
    #   content <- function(file) {
    #     df <- df_shiny()
    #     p <- eval(parse(text = string_code()))
    #     ggsave(file, p, width = width_download(),
    #            height = height_download(), units = "cm")
    #   },
    #   contentType = "application/pdf" # MIME type of the image
    # )
    # output$download_interactive_plot_Tiff <- downloadHandler(
    #   filename <- function() {
    #     paste("Figure_ggplotlyGUI_", Sys.time(), ".tiff", sep = "")
    #   },
    #   content <- function(file) {
    #     df <- df_shiny()
    #     p <- eval(parse(text = string_code()))
    #     ggsave(file, p, width = width_download(),
    #            height = height_download(), units = "cm")
    #   },
    #   contentType = "application/tiff" # MIME type of the image
    # )
    
     output$downloadData <- downloadHandler(
       filename = function() {
         paste('data-', Sys.Date(), '.csv', sep='')
       },
       content = function(con) {
         write.csv(data, con)
       }
     )
  ###############################
  ### scale bar #################
  ###############################
  
   output$data_range <- renderUI({
     # If missing input, return to avoid error later in function
     if(is.null(input$x_var))
       return()
     
     # Get the data set with the selected column
     df<-df_shiny()
     
     if (input$x_var!="") {
       df1 <-df[[input$x_var]]
       
       if (class(df1)[1]=="numeric") {
         sliderInput("range", "X-range of interest:", min = min(df1), max = max(df1), value = c(min(df1),max(df1)))
       } else {
         h5("No scale bar for nonnumerical variable")
       }
     } else {return()}
   })
  
  ####################################
  ####subset data from scale bar ####
  #####################################
     
   get_subset <- reactive({
     min_value <- input$range[1]
     max_value <- input$range[2]
     
     df <- df_shiny()
     
     if (class(df[[input$x_var]])[1]=="numeric"&!is.null(min_value)) {
       df2<-df %>% plotly::filter(df[,input$x_var]>=min_value&df[,input$x_var]<=max_value)
     } else {
       df2<-df
     }
     df2
     
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
  
  #####Construct the shinyApp#####
  shinyApp(ui, server)
}
