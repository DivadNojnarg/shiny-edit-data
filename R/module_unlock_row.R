#' unlock_row UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
unlock_rowUI <- function(id){
	ns <- NS(id)
}

#' unlock_row Server
#'
#' @param id Unique id for module instance.
#' @param trigger Reactive trigger.
#' @param state App state.
#' @param row_index Row to edit.
#' @param con Database pool.
#'
#' @keywords internal
unlock_row_server <- function(id, trigger, state, row_index, con){
	moduleServer(
		id,
		function(
			input,
			output,
			session
			){

				ns <- session$ns
				send_message <- make_send_message(session)

				# Unlock project for everyone in case of mistake
				# TO DO: maybe check if we need to do onSessionEnded... in case
				# of disconnect.
				observe({
				  message("UNLOCK EMPTY EDIT")
				  dbExecute(
				    con,
				    sprintf(
				      "UPDATE %s SET locked = FALSE, status = '%s', last_updated_by = NULL, timestamp = '%s' WHERE id = %s;",
				      config_get("db_data_name"),
				      config_get("status_ok"),
				      Sys.time(),
				      row_index()
				    )
				  )
				}) |> bindEvent(trigger())
		}
	)
}

# UI
# unlock_rowUI('id')

# server
# unlock_row_server('id')
