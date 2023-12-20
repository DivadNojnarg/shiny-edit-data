#' lock_row UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
lock_rowUI <- function(id){
	ns <- NS(id)
}

#' lock_row Server
#'
#' @param id Unique id for module instance.
#' @param trigger Reactive trigger.
#' @param state App state.
#' @param board Where to save data. Pins.
#' @param screen_loader Waiter R6 instance.
#'
#' @keywords internal
lock_row_server <- function(id, trigger, state, board, screen_loader){
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
				observeEvent(trigger(), {
				  state$has_changed <- NULL
				  state$hash <- NULL
				  state$init_hash <- NULL
				  # If user accidentally closes modal without committing data
				  # we'll unlock the current row.
				  send_message("close-modal-callback", value = trigger())

				  # Pins is slow on connect so we must show a loader
				  screen_loader$show()$update(
				    html = tagList(
				      p("Preparing the editor ..."),
				      spin_flower()
				    )
				  )

				  # Only lock is not locked
				  if (!state$dat[trigger(), "locked"]) {
				    message("LOCKING PROJECT")
				    # prevents from reloading the data within the session
				    pin_data <- state$dat
				    pin_data[trigger(), "locked"] <- TRUE
				    board |> pin_write(pin_data, "user-input-poc-data")
				  }
				  screen_loader$hide()
				})
		}
	)
}

# UI
# lock_rowUI('id')

# server
# lock_row_server('id')
