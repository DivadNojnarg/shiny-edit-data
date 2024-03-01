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
#' @param dat DB data.
#' @param state App state.
#' @param con Database pool.
#' @param screen_loader Waiter R6 instance
#'
#' @keywords internal
lock_row_server <- function(id, trigger, dat, state, con, screen_loader) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      # your code here
      observe({
        # Reset reactive values
        state$has_changed <- NULL
        state$hash <- NULL
        state$init_hash <- NULL

        # Pins is slow on connect so we must show a loader
        screen_loader$show()$update(
          html = tagList(
            p("Preparing the editor ..."),
            spin_flower()
          )
        )

        # Only lock if not locked
        if (!dat()[trigger(), "locked"]) {
          message("LOCKING PROJECT")
          # Save to DB
          dbExecute(
            con,
            sprintf(
              'UPDATE %s SET "locked" = 1, "status" = \'%s\', "last_updated_by" = \'%s\', "timestamp" = %s WHERE "id" = %s;',
              config_get("db_data_name"),
              config_get("status_review"),
              state$user,
              create_timestamp(),
              trigger()
            )
          )
        }
        screen_loader$hide()
      }) |> bindEvent(trigger())
    }
  )
}

# UI
# lock_rowUI('id')

# server
# lock_row_server('id')
