require(htmltools)

renderPlotAttribute_Text <- function(var, data, el = htmltools::p ){
    if(var$multiple){
        data %>% map(p)
    } else {
        p(data)
    }
}
