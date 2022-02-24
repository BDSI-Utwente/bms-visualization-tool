renderPlotAttribute_Link_External <-
    function(var, data, label = NULL) {
        if (is.null(label)) {
            if (var$multiple) {
                div(class="link-list",
                    map(data, ~ a(., href = .)))
            } else {
                a(data, href = data)
            }
        } else {
            if (var$multiple) {
                div(class="link-list",
                    map2(data, label, ~ a(.y, href = .x)))
            } else {
                a(label, href = data)
            }
        }
    }

renderPlotAttribute_Link_Internal <- function(var, data) {
    if (var$multiple) {
        div(class="link-list",
            data %>% map( ~ actionLink(., .)))
    } else {
        actionLink(data, data)
    }
}

renderPlotAttribute_Link_Package <- function(var, data) {
    renderPlotAttribute_Link_External(
        var = var,
        data = paste0("https://cran.r-project.org/web/packages/", data, "/"),
        label = data
    )
}