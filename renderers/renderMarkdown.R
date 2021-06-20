require(markdown)
require(htmltools)

renderPlotAttribute_Markdown <- function(var, data){
    applyDynamicMarkup <- tags$script("MathJax.typeset(); hljs.highlightAll();")
    
    if(var$multiple){
        parts <- data %>%
            map(~markdown::renderMarkdown(text=.)) %>%
            htmltools::HTML()
        c(parts, applyDynamicMarkup)
    } else {
        markdown <- data %>% 
            markdown::renderMarkdown(text=.) %>%
            htmltools::HTML()
        list(markdown, applyDynamicMarkup)
    }
}
