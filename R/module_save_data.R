#' save_data UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
save_dataUI <- function(id){
	ns <- NS(id)
}

#' save_data Server
#'
#' @param id Unique id for module instance.
#' @param trigger Reactive trigger.
#' @param state App state.
#' @param new_data New data from the table.
#' @param row_index Row to edit.
#' @param board Where to save data.
#'
#' @keywords internal
save_data_server <- function(id, trigger, state, new_data, row_index, board){
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
				# Update data if difference.
				observeEvent(trigger(), {
				  if (!is.null(state$has_changed) && state$has_changed) {
				    pin_data <- new_data()
				    # Will be under review again whenever modified
				    pin_data[row_index(), "validated"] <- NA
				    # Don't save the button column
				    pin_data$validate <- NULL
				    pin_data[row_index(), "last_updated_by"] <- whoami()
				    board |> pin_write(pin_data, "user-input-poc-data")
				    message("UPDATING DATA")
				  } else {
				    # Unlock project for everyone in case of mistake
				    if (is.na(state$data_cache[row_index(), "last_updated_by"])) {
				      if (state$data_cache[row_index(), "locked"]) {
				        message("UNLOCK EMPTY EDIT")
				        pin_data <- state$data_cache
				        pin_data$validate <- NULL
				        pin_data[row_index(), "locked"] <- FALSE
				        board |> pin_write(pin_data, "user-input-poc-data")
				      }
				    }
				  }
				})
		}
	)
}

# UI
# save_dataUI('id')

# server
# save_data_server('id')
