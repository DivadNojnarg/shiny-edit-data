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
#' @param con Database pool.
#' @param screen_loader Waiter R6 instance.
#'
#' @keywords internal
init_server <- function(id, state, con, screen_loader) {
  moduleServer(
    id,
    function(
    input,
    output,
    session) {

      ns <- session$ns
      send_message <- make_send_message(session)

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
        check_if_user_logged(state, screen_loader)
      })

      # Is admin?
      observeEvent(req(state$connected, state$user), {
        message("CONNECTED TO DB")
        state$is_admin <- is_user_admin(state$user, con)
      })
    }
  )
}

# UI
# initUI('id')

# server
# init_server('id')
