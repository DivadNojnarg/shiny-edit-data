#' Run
#'
#' Run application
#'
#' @param ... Additional parameters to pass to [shiny::shinyApp].
#' @param board Board to pin data.
#'
#' @importFrom shiny shinyApp
#'
#' @export
run <- function(board, ...) {
  app <- shinyApp(
    ui = ui,
    server = server,
    ...
  )

  app$appOptions$board <- board
  app$appOptions$first_version <- get_first_version(config_get("pin_name"), board)

  app
}

#' Run Development
#'
#' Runs the development version which includes
#' the build step.
#'
#' @keywords internal
run_dev <- function() {
  file <- system.file("run/app.R", package = "tableEditor")
  shiny::shinyAppFile(file)
}
