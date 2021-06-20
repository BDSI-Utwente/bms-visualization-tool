require(magrittr)
require(shiny)
require(shinyjs)
require(stringr)
require(purrr)

applyFilter <- function( data, filters, input ){
    filters %<>% transpose()
    
    for (filter in filters) {
        # variable doesn't exist
        if(is.null(data[[filter$variable]]))
            next
        
        # no selection made
        if(is.null(input) 
           || is.null(input[[filter$variable]])
           || is.na(input[[filter$variable]])
           || stringr::str_trim(input[[filter$variable]]) == ""
        )
            next
        
        if(filter$multiple) {
            data %<>% 
                rowwise() %>%
                filter(input[[filter$variable]] %in% .data[[filter$variable]]) %>%
                ungroup()
        } else {
            data %<>% 
                filter(input[[filter$variable]] == .data[[filter$variable]])
        }
    }
    
    data
}

getChoices <- function(multiple, data){
    choices <- character()
    
    if(multiple){
        choices <- data %>% 
            purrr::flatten()
    } else {
        choices <- data
    }
    
    choices %<>% 
        stringr::str_trim() %>%
        unique() %>%
        sort()
    
    c("Filter by..." = "", "No filter" = " ", choices)
}

createFilterInput <- function( id, label, multiple, data ){
    choices <- getChoices(multiple, data)
    shiny::selectizeInput(id, label, choices)   
}

updateFilterInput <- function( id, multiple, data, selected ){
    choices <- getChoices(multiple, data)
    nChoices <- length(choices) - 2 # placeholder and no filter are not real choices
    
    print(paste0("updating ", id, ": ", nChoices, " choices [", selected, "]"))
    if(nChoices <= 1 && (selected == " " || selected == "")){ 
        # if no choices, and nothing selected, then this filter is meaningless
        noChoice = list()
        noChoice[[choices %>% last()]] = " " # last choice is the only choice, or "No filter".
        shiny::updateSelectizeInput(inputId = id, choices = noChoice, selected = " ")
        shinyjs::disable(id)
    } else {
        # we have real choices, update them and ensure filter is enabled
        shiny::updateSelectizeInput(inputId = id, choices = choices, selected = selected)
        shinyjs::enable(id)
    }

    return(TRUE)
}