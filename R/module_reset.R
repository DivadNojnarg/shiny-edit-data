#' reset UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
resetUI <- function(id){
	ns <- NS(id)

	div(
	  class = "d-flex",
	  actionButton(ns("reset"), "Reset data", class = "mx-2")
	)
}

#' reset Server
#'
#' @param id Unique id for module instance.
#' @param data_reset Data to treat.
#' @param board Where to store data.
#' @param pin_name Pin name.
#'
#' @keywords internal
reset_server <- function(id, data_reset, board, pin_name){
	moduleServer(
		id,
		function(
			input,
			output,
			session
			){

				ns <- session$ns
				send_message <- make_send_message(session)

				# your code here
				observeEvent(input$reset, {
				  board |> pin_write(
				    cbind(
				      status = rep("OK", nrow(data_reset)),
				      last_updated_by = rep(NA, nrow(data_reset)),
				      feedback = rep("", nrow(data_reset)),
				      comment = rep("", nrow(data_reset)),
				      do.call(rbind, lapply(1:100, \(x) data_reset)),
				      locked = rep(FALSE, nrow(data_reset)),
				      validated = rep(NA, nrow(data_reset))
				    ),
				    pin_name
				  )
				})
		}
	)
}

# UI
# resetUI('id')

# server
# reset_server('id')
