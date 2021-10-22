require(tidyverse)
require(magrittr)
require(whisker)
require(markdown)

applyTemplate <- function(PLOTS, template) {
    PLOTS[[template$variable]] <- character(PLOTS %>% nrow())
    rendererOptions <-
        c("use_xhtml",
          "smartypants",
          "base64_images",
          "fragment_only")
    
    for (i in 1:nrow(PLOTS)) {
        PLOTS[i, template$variable] <-
            whisker::whisker.render(template$template, PLOTS[i,]) %>%
            markdown::renderMarkdown(text = ., renderer.options = rendererOptions)
        
    }
    
    PLOTS
}
