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
#' @param current_page Currently active table page.
#' @param con Pool DB connection.
#' @param screen_loader Waiter R6 instance.
#'
#' @keywords internal
refresh_data_server <- function(id, state, current_page, con, screen_loader){
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
          tryCatch({
            as.numeric(
              dbReadTable(
                con,
                config_get("db_data_name")
              )[["timestamp"]]
            )
          }, error = function(e) {
            # I'd like to put the modal here but this breaks
            # the function
          })
        },
        # This function returns the content of log_file
        valueFunc = function() {
          req(state$connected, state$user)
          message("REFRESH DATA")

          # Try get data and show message if error
          dat <- tryCatch({
            dbReadTable(
              con,
              config_get("db_data_name")
            )
          }, error = function(e) {
            if (state$connected) {
              screen_loader$hide()
              state$connected <- FALSE
            }
          })

          req(dat)
          dat <- dat |> arrange(id)

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

      # If for some reason the DB is not available
      # we show a modal window.
      observeEvent(req(isFALSE(state$connected)), {
        showModal(
          modalDialog(
            title = tagList(icon("xmark"), "Error: database not available"),
            "Could not connect to the database. Please retry later."
          )
        )
      })

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
      observeEvent({
        req(processed_data())
        c(processed_data(), current_page())
      },{
        message("UPDATE LOCKED PROJECTS")
        # Tell JS which button to lock/unlock
        range <- seq(current_page() * 10 - 9, current_page() * 10)
        send_message(
          "toggle-buttons",
          value = find_projects_to_lock(
            processed_data()[range, ],
            state$is_admin,
            state$user
          )
        )
      })

      # Required to display the difference between first data version
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
