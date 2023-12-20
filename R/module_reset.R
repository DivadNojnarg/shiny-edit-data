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
#' @param board Where to store data.
#' @param screen_loader Waiter R6 instance.
#'
#' @keywords internal
reset_server <- function(id, board, screen_loader) {
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
        # This will remove the pin and reset it. TO DO: broken
        # board |> pin_remove(config_get("pin_name"))
        prepare_data(config_get("data_reset"), board, config_get("pin_name"))
      })
    }
  )
}

# UI
# resetUI('id')

# server
# reset_server('id')
