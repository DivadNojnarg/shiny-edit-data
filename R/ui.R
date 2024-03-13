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
    tags$nav(
      class = "navbar navbar-expand-lg bg-body-secondary border-bottom border-body",
      `data-bs-theme` = "dark",
      tags$div(
        class = "container-fluid",
        tags$a(class = "navbar-brand", href = "#", "tableEditor"),
        div(
          class = "d-flex align-items-center",
          icon("circle-user", class = "fa-solid fa-2x mx-2", style = "color: white;"),
          uiOutput("whoami", class = "navbar-text", inline = TRUE)
        )
      )
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
