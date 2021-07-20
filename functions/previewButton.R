require(shiny)

BASE_ICONS_PATH <- "images/icons"
UNKNOWN_ICON <- "unknown.png"
print("paths")
print(getwd())
dir(BASE_ICONS_PATH) %>% print

previewButton <- function(plot) {
    shiny::actionButton(plot$id, getPreview(plot), 
                             class = "preview-button", 
                             style = "margin: .2em .2em 0 0;")
}

getPreview <- function(plot) {
    iconPath <- getPreviewIconPath(plot)
    htmltools::div(
        htmltools::div(style=paste0("background-image: url(", iconPath, ");"), class="preview-image"),
        htmltools::span(class="preview-label", plot$name),
        htmltools::div(class="preview-overlay"),
        class="preview-content"
    )
}

getPreviewIconPath <- function(plot) {
    iconPath <- file.path(BASE_ICONS_PATH, paste0(plot$id, ".png"))
    if(file.exists(localPath(iconPath))){
        return(iconPath)
    }
    iconPath <- file.path(BASE_ICONS_PATH, paste0(stringr::str_replace_all(plot$id, "-", "_"), ".png"))
    if(file.exists(localPath(iconPath))){
        return(iconPath)
    }   
    file.path(BASE_ICONS_PATH, UNKNOWN_ICON)
}

localPath <- function(path, prefix = "www") {
    file.path(prefix, path)
}