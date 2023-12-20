#' allow_save UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
allow_saveUI <- function(id) {
  ns <- NS(id)
}

#' allow_save Server
#'
#' @param id Unique id for module instance.
#' @param state App state.
#' @param trigger Reactive trigger.
#'
#' @keywords internal
allow_save_server <- function(id, state, trigger) {
  moduleServer(
    id,
    function(input,
             output,
             session) {
      ns <- session$ns
      send_message <- make_send_message(session)

      observeEvent(req(length(trigger()) > 0), {
        if (is.null(state$hash)) {
          state$has_changed <- FALSE
          state$hash <- rlang::hash(trigger())
          state$init_hash <- state$hash
          return(NULL)
        }

        if (rlang::hash(trigger()) != state$init_hash) {
          if (rlang::hash(trigger()) != state$hash) {
            state$hash <- rlang::hash(trigger())
            state$has_changed <- TRUE
          }
        } else {
          state$hash <- rlang::hash(trigger())
          state$has_changed <- FALSE
        }
      })
      # Block the save result button if data have not changed
      observeEvent(state$has_changed, {
        send_message("can-save", value = state$has_changed)
      })
    }
  )
}

# UI
# allow_saveUI('id')

# server
# allow_save_server('id')
