renderPlotAttribute_Title <- function(var, data){
    if(var$multiple){
        h2(paste0(data, collapse = ", "))
    } else {
        h2(data)
    }
}