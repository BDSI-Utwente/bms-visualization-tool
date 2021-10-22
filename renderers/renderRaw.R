require(htmltools)

renderPlotAttribute_Raw <- function(var, data){
    if(var$multiple){
        data %>% map(htmltools::HTML)
    } else {
        htmltools::HTML(data)
    }
}
