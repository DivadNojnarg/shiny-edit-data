#' save_data UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
save_dataUI <- function(id) {
  ns <- NS(id)
}

#' save_data Server
#'
#' @param id Unique id for module instance.
#' @param trigger Reactive trigger.
#' @param new_data New data from the table.
#' @param row_index Row to edit.
#' @param state App state.
#' @param con Database pool.
#'
#' @keywords internal
save_data_server <- function(id, trigger, new_data, row_index, state, con) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      # your code here
      # Update data if difference.
      observe({
        dat <- new_data()
        # Will be under review again whenever modified
        dat[row_index(), "validated"] <- NA_real_
        dat[row_index(), "last_updated_by"] <- state$user
        dat[row_index(), "id"] <- generate_new_id(dat)
        dat[row_index(), "status"] <- config_get("status_review")
        dat[row_index(), "timestamp"] <- create_timestamp()

        # Convert row names back to character. They were set to
        # numeric so as to arrange in the display table
        dat[row_index(), "row_names"] <- as.character(dat[row_index(), "row_names"])

        # Save to DB
        message("UPDATING DATA")
        dbAppendTable(
          con,
          config_get("db_data_name"),
          value = dat[row_index(), state$cols]
        )
      }) |> bindEvent(trigger())
    }
  )
}

# UI
# save_dataUI('id')

# server
# save_data_server('id')
