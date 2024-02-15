#' lock_row UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
lock_rowUI <- function(id) {
  ns <- NS(id)
}

#' lock_row Server
#'
#' @param id Unique id for module instance.
#' @param trigger Reactive trigger.
#' @param state App state.
#' @param con Database pool.
#' @param screen_loader Waiter R6 instance.
#'
#' @keywords internal
lock_row_server <- function(id, trigger, state, con, screen_loader) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
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
        if (!state$data_cache[trigger(), "locked"]) {
          message("LOCKING PROJECT")
          # Update cache + save to DB
          dbExecute(
            con,
            sprintf(
              "UPDATE %s SET locked = TRUE, status = '%s', last_updated_by = '%s' WHERE id = %s;",
              config_get("db_data_name"),
              config_get("status_review"),
              whoami(),
              trigger()
            )
          )
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
