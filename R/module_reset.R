#' reset UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
resetUI <- function(id) {
  ns <- NS(id)

  div(
    class = "d-flex",
    actionButton(ns("reset"), "Reset data", class = "mx-2")
  )
}

#' reset Server
#'
#' @param id Unique id for module instance.
#' @param con Database pool.
#' @param screen_loader Waiter R6 instance.
#'
#' @keywords internal
reset_server <- function(id, con, screen_loader) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      # your code here
      observeEvent(input$reset, {
        # TO DO: add screen loader
        # This will remove the pin and reset it.
        message("RESET DATA")
        prepare_data(con, config_get("data_reset"), overwrite = TRUE)
      })
    }
  )
}

# UI
# resetUI('id')

# server
# reset_server('id')
