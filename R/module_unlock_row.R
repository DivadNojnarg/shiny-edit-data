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
#' @param dat App data.
#' @param row_index Row to edit.
#' @param con Database pool.
#'
#' @keywords internal
unlock_row_server <- function(id, trigger, dat, row_index, con){
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
          # Unlock only if there are not revisions in the DB that is
				  # if the row_names is equal to the id. Revisions have an higher
				  # id than the row names (which is fixed). For instance
				  # if someone lock the first row, id and row_names will be 1.
				  # If changes are made, a new row is created with row_names = 1
				  # but an id equal to the highest existing id + 1. This prevents
				  # to accidentally unlock real edits.
				  if (dat()[row_index(), "row_names"] == dat()[row_index(), "id"]) {
				    message("UNLOCK EMPTY EDIT")
				    dbExecute(
				      con,
				      sprintf(
				        'UPDATE %s SET "locked" = 0, "status" = \'%s\', "last_updated_by" = NULL, "timestamp" = \'%s\' WHERE "id" = %s;',
				        config_get("db_data_name"),
				        config_get("status_ok"),
				        Sys.time(),
				        row_index()
				      )
				    )
				  }
				}) |> bindEvent(trigger())
		}
	)
}

# UI
# unlock_rowUI('id')

# server
# unlock_row_server('id')
