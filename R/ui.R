#' Shiny UI
#'
#' Core UI of package.
#'
#' @param req The request object.
#'
#' @import shiny
#' @importFrom bslib bs_theme input_dark_mode
#' @importFrom waiter useWaiter waiterShowOnLoad spin_flower Waiter
#' @importFrom htmltools findDependencies
#' @importFrom datamods edit_data_ui
#'
#' @keywords internal
ui <- function(req) {
  fluidPage(
    theme = bs_theme(version = 5),
    list(assets()),
    title = "tableEditor",
    useWaiter(),
    waiterShowOnLoad(
      html = tagList(
        p("Hello!"),
        spin_flower()
      )
    ),
    # TO DO: issues on Posit Connect ...
    #input_dark_mode(id = "app_theme", mode = "light"),
    div(
      class = "bg-secondary p-5 rounded-lg m-3",
      h1(class = "display-4 bg-secondary", HTML(sprintf("Welcome %s", uiOutput("whoami", inline = TRUE)))),
      p(class = "lead", "Edit data tool")
    ),
    if (!config_get("production")) resetUI("reset"),
    uiOutput("highlight_changes"),
    edit_data_ui(id = "edit"),
    # To be able to use icons
    findDependencies(icon("check")),
    tags$footer(
      class = "footer mt-auto flex-wrap justify-content-between align-items-center py-3 my-4 border-top",
      p(
        class = "col-md-6 mb-0 text-body-secondary",
        sprintf("Production: %s", config_get("production"))
      ),
      p(
        class = "col-md-6 mb-0 text-body-secondary",
        sprintf("Version: %s", config_get("version"))
      )
    )
  )
}

#' Assets
#'
#' Includes all assets.
#' This is a convenience function that wraps
#' [serveAssets] and allows easily adding additional
#' remote dependencies (e.g.: CDN) should there be any.
#'
#' @importFrom shiny tags
#'
#' @keywords internal
assets <- function() {
  list(
    serveAssets(), # base assets (assets.R)
    tags$head(
      # Place any additional depdendencies here
      # e.g.: CDN
    )
  )
}
