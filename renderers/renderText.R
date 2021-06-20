#' Title
#'
#' @param var 
#' @param data 
#' @param el 
#'
#' @return
#' @export
#'
#' @examples
renderPlotAttribute_Text <- function(var, data, el = htmltools::p ){
    if(var$multiple){
        data %>% map(p)
    } else {
        p(data)
    }
}
