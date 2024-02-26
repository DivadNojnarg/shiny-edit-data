#' Run
#'
#' Run application
#'
#' @param ... Additional parameters to pass to [shiny::shinyApp].
#' @param pool Database pool.
#'
#' @importFrom shiny shinyApp
#'
#' @export
run <- function(pool, ...) {
  app <- shinyApp(
    ui = ui,
    server = server,
    ...
  )

  app$appOptions$pool <- pool

  app$appOptions$col_types <- dbReadTable(
    pool,
    sprintf("%s_types", config_get("db_data_name"))
  )

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
