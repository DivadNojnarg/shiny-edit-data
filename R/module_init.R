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
#' @param state App state.
#' @param screen_loader Waiter R6 instance.
#' @param first_version Data to send to JS. First pin version is fine.
#' @param input_data Input data provided by a reactive pin.
#'
#' @keywords internal
init_server <- function(id, state, screen_loader, first_version, input_data) {
  moduleServer(
    id,
    function(
        input,
        output,
        session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      # Use admin mode: http://127.0.0.1:6505/?admin
      observeEvent(session$clientData$url_search, {
        query <- names(parseQueryString(session$clientData$url_search))
        if (is.null(query)) {
          state$is_admin <- FALSE
        } else {
          if ("admin" %in% query) state$is_admin <- TRUE
        }
      })

      # Show waiter + make pins data available to JS so that
      # we can use it to generate the reactable columns from JS
      # (faster than from R)...
      observeEvent(req(state$init), {
        screen_loader$show()$update(
          html = tagList(
            p("Initializing app ..."),
            spin_flower()
          )
        )
        send_message("send-init-data", value = first_version)
      })

      # Reload data
      observeEvent(input_data(), {
        message("INIT DATA AND BUTTONS")
        # Create/update a state which user can edit
        state$data_cache <- input_data()

        # Add validate button for admin
        state$data_cache$validate <- rep(NA, nrow(state$data_cache))

        # Handle status column
        state$data_cache$status <- apply_status(state$data_cache)

        # Initial locking
        locked <- find_projects_to_lock(state$data_cache[1:10, ], state$is_admin)
        # Tell JS which button to lock/unlock
        send_message("toggle-buttons", value = locked)
      })
    }
  )
}

# UI
# initUI('id')

# server
# init_server('id')
