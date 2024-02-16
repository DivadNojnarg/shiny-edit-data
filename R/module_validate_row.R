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
#' @param con Database pool.
#'
#' @keywords internal
validate_row_server <- function(id, trigger, dat, con){
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

				  dat[trigger(), "last_updated_by"] <- whoami()
				  dat[trigger(), "id"] <- generate_new_id(dat)
				  dat[trigger(), "timestamp"] <- Sys.time()

				  dat[trigger(), "status"] <- config_get(sprintf("status_%sed", id))
				  dat[trigger(), "validated"] <- if (id == "accept") TRUE else FALSE
				  dat[trigger(), "feedback"] <- input$feedback
          browser()
				  # Save to DB
				  message("VALIDATING DATA")
				  dbAppendTable(
				    con,
				    config_get("db_data_name"),
				    value = dat[trigger(), !(colnames(dat) %in% c("validate"))]
				  )
				})
		}
	)
}

# UI
# validate_rowUI('id')

# server
# validate_row_server('id')
