# load packages
require(shiny)
require(shinyjs)
require(tidyverse)
require(magrittr)
require(googlesheets4)

# helper functions
source("functions/filterInput.R")
source("renderers/render.R", chdir = TRUE)

# fetch data from google sheets
googlesheets4::gs4_deauth()
SHEET_ID <- "10eMzcaMgKPkKbALWkzPMKi6EXo3yt70PxqS6arhxHag"
PLOTS <- googlesheets4::read_sheet(SHEET_ID, 1, col_types = "c")
VARS <- googlesheets4::read_sheet(SHEET_ID, 2)

# prepare data
searchableVars <- VARS %>% filter( search ) %>% pull( variable )
multipleVars <- VARS %>% filter( multiple ) %>% pull( variable )
PLOTS %<>% 
    # create single text for all searchable attributes
    unite(searchIndex, any_of(searchableVars), sep = "\n", remove = FALSE) %>%
     
    # split vector attributes
    mutate(across(any_of(multipleVars), ~stringr::str_split(., ", ")))


# UI ----------------------------------------------------------------------
ui <- fluidPage(
    tags$head(
        # MathJax math rendering
        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/3.2.0/es5/tex-mml-chtml.min.js"),
        
        # highlight.js code highlighting
        tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.0.1/highlight.min.js"),
        tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.0.1/styles/default.min.css"),
        
        # custom button sorting function
        shinyjs::useShinyjs(),
        shinyjs::extendShinyjs("sortButtons.js", functions = c("sortButtons")),
    ),
    
    titlePanel("Henk's used plot emporium"),
    
    sidebarLayout(
        sidebarPanel(
            # build list of filter inputs from variables defined in the spreadsheet. 
            VARS %>%
                filter( filter ) %>%
                rowwise() %>%
                transmute(
                    id = variable,
                    label,
                    data = PLOTS[[id]] %>% list,
                    multiple
                ) %>%
                transpose() %>%
                map(~createFilterInput(.$id, .$label, .$multiple, .$data)),
        ),
        
        mainPanel(
            # buttons
            div( PLOTS %>% 
                    select( id, name ) %>%
                    arrange( id ) %>% 
                    transpose() %>%
                    map(~shiny::actionButton(.$id, .$name, 
                                             class = "btn-primary", 
                                             style = "margin: .2em .2em 0 0;")),
                style = "display: flex; flex-flow: row wrap;",
                id = "plot-button-container" ),
            
            # details
            # textOutput("details"),
            htmlOutput("details")
        )
    )
)

# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
    # selected plot
    plot <- reactiveVal(NULL)
    
    # create a list of observers, one for each button
    buttonObservers <- PLOTS %>%
        pull(id) %>%
        map(~observeEvent(input[[.]], {
            id <- .
            plot(PLOTS[PLOTS$id == id,] %>% slice(1)) # for some reason filter function doesn't work?
            cat("selected:", plot()$id, "\n")
        }))
    
    # filter observer ------------------------------------------
    filterObserver <- observe({
        filters <- VARS %>% 
            filter( filter == TRUE )
        plots <- PLOTS %>% applyFilter( filters, input )
        
        # after filtering data, update available choices on UI
        for ( filter in filters %>% transpose() ){
            
            # get data applying all other filters
            this <- filter$variable
            data <- PLOTS %>% applyFilter( filters %>% filter( variable != this ), input )
            
            # update filter input
            updateFilterInput(filter$variable, filter$multiple, data[[filter$variable]], input[[filter$variable]])
        }
        
        # enable/disable buttons, then sorts
        for (plot in PLOTS$id) {
            if(plot %in% plots$id){
                shinyjs::enable(plot) 
            } else {
                shinyjs::disable(plot)
            }
        }
        shinyjs::js$sortButtons(hide = TRUE)
    })
    
    # output blocks -----------------------------------------------------------
    output$details <- renderText({
        # pull out if no plot selected. 
        selected <- plot()
        if(is.null(selected)) return(invisible(NULL)) 
        
        cat("updating plot:", selected$id, "\n")
        
        div(
            VARS %>% 
                filter(type != "hidden") %>%
                transpose() %>%
                simplify_all() %>%
                map(~renderPlotAttribute(., selected)),
            id = "details-container"
        ) %>% as.character()
    })
}

shinyApp(ui, server)
