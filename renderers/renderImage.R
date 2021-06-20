renderPlotAttribute_Image <- function(var, data){
    if(var$multiple){
        data %>% map(~img(src=., alt=var$name))
    } else {
        tags$figure(
            tags$image(src=data, alt=var$variable),
            tags$figcaption(var$name)
        )
    }
}
