#' refresh_data UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
refresh_dataUI <- function(id){
	ns <- NS(id)
}

#' refresh_data Server
#'
#' @param id Unique id for module instance.
#' @param state App state.
#' @param con Pool DB connection.
#'
#' @keywords internal
refresh_data_server <- function(id, state, con){
	moduleServer(
		id,
		function(
			input,
			output,
			session
			){

				ns <- session$ns
				send_message <- make_send_message(session)

				# Querying DB to check for any change
				db_data <- reactivePoll(
				  1000,
				  session,
				  # This function returns the time stamps
				  checkFunc = function() {
				    as.numeric(
				      dbReadTable(
				        con,
				        config_get("db_data_name")
				      )[["timestamp"]]
				    )
				  },
				  # This function returns the content of log_file
				  valueFunc = function() {
				    req(state$connected, state$user)
				    message("REFRESH DATA")

				    dat <- dbReadTable(
				      con,
				      config_get("db_data_name")
				    ) |> arrange(id)

				    # Needed because factors are lost when stored
				    # in SQL. We need to restore based on the
				    # preliminary metadata. See prepare_data().
				    factor_cols <- find_factor_columns(state$col_types)
				    if (length(factor_cols) > 0) {
				      dat <- dat |>
				        mutate(across(factor_cols, restore_col_type))
				    }

				    dat
				  }
				)

				processed_data <- reactive({
				  req(db_data())
				  # Take only the last observation per row_names
				  # which corresponds to the most up to date version.
				  dat <- get_last_version(db_data())

				  dat$validate <- rep(NA, nrow(dat))
				  cols <- split_data_cols(dat)
				  dat[, cols$to_show]
				})

				# Lock projects in real time
				observe({
				  req(processed_data())
				  invalidateLater(500)
				  message("UPDATE LOCKED PROJECTS")
				  # Tell JS which button to lock/unlock
				  send_message(
				    "toggle-buttons",
				    value = find_projects_to_lock(processed_data()[1:10, ], state$is_admin, state$user)
				  )
				})

				observeEvent(req(db_data()), {
				  state$first_version <- get_first_version(db_data())
				  send_message("send-init-data", value = state$first_version)
				}, once = TRUE)

				return(processed_data)
		}
	)
}

# UI
# refresh_dataUI('id')

# server
# refresh_data_server('id')
