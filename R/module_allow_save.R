#' allow_save UI
#'
#' @param id Unique id for module instance.
#'
#' @keywords internal
allow_saveUI <- function(id){
	ns <- NS(id)
}

#' allow_save Server
#'
#' @param id Unique id for module instance.
#' @param cache Central app cache.
#' @param trigger Reactive trigger.
#'
#' @keywords internal
allow_save_server <- function(id, cache, trigger){
	moduleServer(
		id,
		function(
			input,
			output,
			session
			){

				ns <- session$ns
				send_message <- make_send_message(session)

				observeEvent(req(length(trigger()) > 0), {
				  if (is.null(cache$hash)) {
				    cache$has_changed <- FALSE
				    cache$hash <- rlang::hash(trigger())
				    cache$init_hash <- cache$hash
				    return(NULL)
				  }

				  if (hash(trigger()) != cache$init_hash) {
				    if (hash(trigger()) != cache$hash) {
				      cache$hash <- rlang::hash(trigger())
				      cache$has_changed <- TRUE
				    }
				  } else {
				    cache$hash <- rlang::hash(trigger())
				    cache$has_changed <- FALSE
				  }
				})
				# Block the save result button if data have not changed
				observeEvent(cache$has_changed, {
				  send_message("can-save", value = cache$has_changed)
				})
		}
	)
}

# UI
# allow_saveUI('id')

# server
# allow_save_server('id')
