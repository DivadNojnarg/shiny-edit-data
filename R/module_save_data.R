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
#' @param state App state.
#' @param new_data New data from the table.
#' @param row_index Row to edit.
#' @param con Database pool.
#'
#' @keywords internal
save_data_server <- function(id, trigger, state, new_data, row_index, con) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      # your code here
      # Update data if difference.
      observeEvent(trigger(), {
        if (!is.null(state$has_changed) && state$has_changed) {
          dat <- new_data()
          # Will be under review again whenever modified
          dat[row_index(), "validated"] <- NA
          # Don't save the button column
          dat[row_index(), "last_updated_by"] <- whoami()
          dat[row_index(), "id"] <- generate_new_id(dat)
          dat[row_index(), "timestamp"] <- Sys.time()

          # Save to DB
          message("UPDATING DATA")
          dbAppendTable(
            con,
            config_get("db_data_name"),
            value = dat[row_index(), !(colnames(dat) %in% c("validate"))]
          )
        } else {
          # Unlock project for everyone in case of mistake
          # TO DO: maybe check if we need to do onSessionEnded... in case
          # of disconnect.
          message("UNLOCK EMPTY EDIT")
          dbExecute(
            con,
            sprintf(
              "UPDATE %s SET locked = FALSE, status = '%s', last_updated_by = NULL WHERE id = %s;",
              config_get("db_data_name"),
              config_get("status_ok"),
              row_index()
            )
          )
        }
      })
    }
  )
}

# UI
# save_dataUI('id')

# server
# save_data_server('id')
