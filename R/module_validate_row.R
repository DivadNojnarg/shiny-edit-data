#' validate_row UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
validate_rowUI <- function(id){
	ns <- NS(id)
}

#' validate_row Server
#'
#' @param id Unique id for module instance.
#' @param trigger Reactive trigger.
#' @param dat Data to validate.
#' @param state App state.
#' @param con Database pool.
#'
#' @keywords internal
validate_row_server <- function(id, trigger, dat, state, con){
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
				  showModal(
				    modalDialog(
				      title = sprintf("You're about to %s the current changes", id),
				      "Are you sure about your choice. Please confirm.",
				      textAreaInput(ns("feedback"), "", placeholder = "optional feedback"),
				      footer = tagList(
				        modalButton("Cancel"),
				        actionButton(ns(sprintf("%s_ok", id)), "OK")
				      )
				    )
				  )
				})

				observeEvent(input[[sprintf("%s_ok", id)]], {
				  removeModal()
				  dat <- dat()

				  dat[trigger(), "last_updated_by"] <- state$user
				  dat[trigger(), "id"] <- generate_new_id(dat)
				  dat[trigger(), "timestamp"] <- create_timestamp()

				  dat[trigger(), "status"] <- config_get(sprintf("status_%sed", id))
				  dat[trigger(), "validated"] <- if (id == "accept") 1 else 0
				  dat[trigger(), "feedback"] <- input$feedback

				  # Save to DB
				  message("VALIDATING DATA")
				  dbWriteTable(
				    con,
				    config_get("db_data_name"),
				    value = dat[trigger(), state$cols],
				    append = TRUE
				  )
				})
		}
	)
}

# UI
# validate_rowUI('id')

# server
# validate_row_server('id')
