require(markdown)
require(htmltools)
require(whisker)

renderPlotAttribute_Markdown <- function(var, data, plot){
    applyDynamicMarkup <- tags$script("MathJax.typeset(); hljs.highlightAll();")
    rendererOptions <- c("use_xhtml", "smartypants", "base64_images", "fragment_only")
    
    cat("rendering markdown")
    print(str(plot))
    print(str(data))
    
    print(whisker::whisker.render(data, plot))
    
    if(var$multiple){
        parts <- data %>%
            map(~whisker::whisker.render(template=., data = plot)) %>%
            map(~markdown::renderMarkdown(text=., renderer.options = rendererOptions)) %>%
            htmltools::HTML()
        c(parts, applyDynamicMarkup)
    } else {
        markdown <- data %>% 
            whisker::whisker.render(template=., data = plot) %>%
            markdown::renderMarkdown(text=., renderer.options = rendererOptions) %>%
            htmltools::HTML()
        
        list(markdown, applyDynamicMarkup)
    }
}
