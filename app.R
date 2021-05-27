#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

# prepare shared data 
# note; you'll need to think of a clever solution on how to generate and filter plot types
# that are valid for multiple (combinations of) criteria.
plots <- tibble(
    numVars = c(1,1,2,"many",1),
    xLevel = c("Categorical", "Ordinal", "Ordinal", "Ratio", "Scale"),
    yLevel = c("Count", "Count", "Scale", "Ratio", "Density"),
    plotName = c("Piechart", "Piechart", "Barchart", "Multiline chart", "Density plot"),
    plotImage = c(
        "http://placekitten.com/400/300",
        "http://placekitten.com/400/301",
        "http://placekitten.com/400/302",
        "http://placekitten.com/400/303",
        "http://placekitten.com/400/304"
    ),
    plotDescription = c(
        "It's round, it's ubiquitous, it's.... kinda bad.",
        "It's round, it's ubiquitous, it's.... kinda bad.",
        "Don't get these confused with a histogram! Also comes in coloured, stacked, grouped and horizontal variants.",
        "The linear version of bar charts. Comes in many flavours, and has many uses.",
        "Good at giving a quick glance of a distribution, but histograms might be more honest." ))

plotNames <- plots %>% pull( plotName ) %>% unique() %>% sort()


ui <- fluidPage(
    titlePanel("Henk's used plot emporium"),
    
    sidebarLayout(
        sidebarPanel(
            # criteria
            selectInput("numVars", "Number of variables:", plots$numVars %>% unique() %>% sort() ),
            selectInput("xLevel", "Measurement level of x variable", plots$xLevel %>% unique() %>% sort()),
            selectInput("yLevel", "Measurement level of y variable", plots$yLevel %>% unique() %>% sort()), 
            
            # dynamically generated buttons
            uiOutput("plotChoices")
        ),

        mainPanel(
            fluidRow(column(12, textOutput("plotTitle", tags$h2))),
            fluidRow( 
                column(6, textOutput("plotDescription")),
                column(6, htmlOutput("plotImage")),
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # reactive value to hold the name (or id) of the chosen plot
    chosenPlot <- reactive({
            if(!is.null(input$choice)){
                input$choice
            } else {
                ""
            }
        }) 
    
    # reactive expression that pulls data for the plot
    chosenPlotData <- reactive({
        plots %>% 
            filter(plotName == chosenPlot()) %>%
            slice(1)
    })

    # I'd like to have a separate buttons, but the observers don't want to work.
    # # dynamically build observers for each of the plot type buttons
    # for( name in plotNames){
    #     id <- paste0("plot-", name)
    #     cat(name, "::", id, "\n")
    #     
    #     observe({
    #         input[[id]]
    #         chosenPlot(name)
    #     })
    # }
    
    output$plotTitle <- renderText({
        chosenPlotData()$plotName
    })
    
    output$plotDescription <- renderText(
        chosenPlotData()$plotDescription
    )
    
    output$plotImage <- renderText(
        # modern semantic html uses a figure container for images
        tags$figure(
            # HTML image tag with alt text
            tags$image(src = chosenPlotData()$plotImage, alt = chosenPlotData()$plotName),
            tags$figcaption(chosenPlotData()$plotName)
        ) %>% as.character()
    )
    
    # generate buttons for plot selection
    output$plotChoices <- renderUI(
        flowLayout({
            # filter plot types for the chosen criteria
            options <- plots %>% 
                filter(
                    numVars == input$numVars,
                    xLevel == input$xLevel,
                    yLevel == input$yLevel ) %>% 
                pull( plotName ) %>% 
                unique() %>% 
                sort()
            
            # populate a selectize input with the valid choices
            selectizeInput("choice", "Applicable plots", options )
            
            # I'd like to do separatae buttons, but I haven't been able to figure 
            # out how to get the event handling set up.
            # tags$div(
            #     tags$label("Applicable plots"),
            #     flowLayout(
            #         lapply( plotNames, function(name) { 
            #             id <- paste0("plot-", name)
            #             
            #             if (name %in% options ){
            #                 
            #                 actionButton(
            #                     id,
            #                     label = name, 
            #                     class = "btn btn-primary")
            #             } else {
            #                 actionButton(
            #                     id,
            #                     label = name, 
            #                     class = "btn",
            #                     disabled = "true")
            #             }   
            #         })
            #     )
            # )
        })
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
