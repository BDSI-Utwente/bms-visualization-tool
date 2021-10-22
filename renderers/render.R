require(shiny)
require(tidyverse)

source("renderTitle.R")
source("renderText.R")
source("renderImage.R")
source("renderLink.R")
source("renderMarkdown.R")
source("renderRaw.R")

# source("./renderRScript.R")
# source("./renderInteractivePlot.R")

#' @title Render a plot attribute as html
#' 
#' @param plot a plot object, representing a single row of the plot database.
#' @param var a variable object, representing a single row of the attribute list.
#' 
#' @return html representation of attribute `var$variable` on `plot`.
#' @details renderPlotAttribute delegates rendering to various render functions, depending on the type of the attribute.
#' @import shiny, tidyverse
renderPlotAttribute <- function(var, plot){
    id <- var$variable
    data <- plot[[id]] %>% unlist()
    comment <- HTML("<!-- ", id, "-->")
    if(is.null(data) || is.na(data) || data == "") return(comment)
    
    header <- h3(var$label)
    content <- switch (var$type,
                       "title" = renderPlotAttribute_Title(var, data),
                       "text" = renderPlotAttribute_Text(var, data),
                       "image" = renderPlotAttribute_Image(var, data),
                       "markdown" = renderPlotAttribute_Markdown(var, data, plot),
                       "template" = renderPlotAttribute_Raw(var, data),
                       "package" = renderPlotAttribute_Link_Package(var, data),
                       "link-internal" = renderPlotAttribute_Link_Internal(var, data),
                       "link-external" = renderPlotAttribute_Link_External(var, data)
    )
    
    if(var$type != "title"){
        list(comment, header, content)
    } else {
        list(comment, content)
    }
}
