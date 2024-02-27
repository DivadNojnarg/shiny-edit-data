#' init UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
initUI <- function(id) {
  ns <- NS(id)
}

#' init Server
#'
#' @param id Unique id for module instance.
#' @param con Database pool.
#' @param state App state.
#' @param screen_loader Waiter R6 instance.
#'
#' @keywords internal
init_server <- function(id, con, state, screen_loader) {
  moduleServer(
    id,
    function(
    input,
    output,
    session) {

      ns <- session$ns
      send_message <- make_send_message(session)

      # Pool onStop callback
      onStop(function() {
        message("DISCONNECTED DB")
        poolClose(getShinyOption("pool"))
      })

      # Show waiter + initialise connected state
      observeEvent(req(state$init), {
        message("TRY CONNECT TO DB")
        message <- NULL
        if (inherits(getShinyOption("pool"), "error")) {
          message <- getShinyOption("pool")$message
        } else {
          state$connected <- TRUE
        }
        screen_loader$show()$update(
          html = tagList(
            p("Initializing app ..."),
            message,
            spin_flower()
          )
        )

        if (inherits(getShinyOption("pool"), "error")) {
          Sys.sleep(2)
          screen_loader$update(
            html = tagList(
              p("Trying to reconnect to DB in 3 seconds ..."),
              spin_flower()
            )
          )
          Sys.sleep(3)
          session$reload()
        }

        # Check connected user
        check_if_user_logged(screen_loader)
      })

      # Is admin?
      observeEvent(req(state$connected), {
        message("CONNECTED TO DB")
        state$is_admin <- is_user_admin(con)
      })

      # Querying DB to check for any change (by other users)
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
      observeEvent(processed_data(), {
        req(state$connected)
        message("UPDATE LOCKED PROJECTS")
        # Tell JS which button to lock/unlock
        send_message(
          "toggle-buttons",
          value = find_projects_to_lock(processed_data()[1:10, ], state$is_admin)
        )
      })

      observeEvent(processed_data(), {
        state$first_version <- get_first_version(db_data())
        send_message("send-init-data", value = state$first_version)
      }, once = TRUE)

      return(processed_data)

    }
  )
}

# UI
# initUI('id')

# server
# init_server('id')
