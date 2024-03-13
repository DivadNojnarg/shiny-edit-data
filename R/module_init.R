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
        # Check if pool is valid
        check_db_connection(state, screen_loader, session)
        # Check if config is valid
        check_config(state, screen_loader, session)
        # Check connected user
        check_if_user_logged(state, screen_loader)
      })

      # Is admin?
      observeEvent(req(state$connected, state$user), {
        message("CONNECTED TO DB")
        state$is_admin <- is_user_admin(state$user, con)
        state$init <- FALSE
      })
    }
  )
}

# UI
# initUI('id')

# server
# init_server('id')
