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
    ...,
    onStart = function() {
      # Pool onStop callback
      onStop(function() {
        message("DISCONNECTED DB")
        poolClose(pool)
      })
    }
  )

  app$appOptions$pool <- pool

  # Happens only once when apps fire
  # This should not happen each time a user connects
  if (!inherits(pool, "error")) {
    app$appOptions$col_types <- dbReadTable(
      pool,
      sprintf("%s_types", config_get("db_data_name"))
    )

    # Necessary because the display order isn't the same as the
    # saved order. i.e some columns like validate are visible
    # for admins but not saved in the DB. This allows to correctly
    # sort the columns of the data to save in module_save_data.R
    app$appOptions$col_names <- colnames(
      dbReadTable(
        pool,
        config_get("db_data_name")
      )
    )
  }

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
