#' Shiny UI
#'
#' Core UI of package.
#'
#' @param req The request object.
#'
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom waiter useWaiter waiterShowOnLoad spin_flower Waiter
#' @importFrom htmltools findDependencies
#' @importFrom datamods edit_data_ui
#'
#' @keywords internal
ui <- function(req){
	fluidPage(
		theme = bs_theme(version = 5),
		list(assets()),
		title = "tableEditor",
		useWaiter(),
		waiterShowOnLoad(
		  html = tagList(
		    p("Hello!"),
		    spin_flower()
		  )
		),
		div(
		  class = "bg-light p-5 rounded-lg m-3",
		  h1(class = "display-4", HTML(sprintf("Welcome %s", uiOutput("whoami", inline = TRUE)))),
		  p(class = "lead", "Edit contracts dashboard ...")
		),
		resetUI("reset"),
		uiOutput("highlight_changes"),
		edit_data_ui(id = "edit"),
		# To be able to use icons
		findDependencies(icon("check"))
	)
}

#' Assets
#'
#' Includes all assets.
#' This is a convenience function that wraps
#' [serveAssets] and allows easily adding additional
#' remote dependencies (e.g.: CDN) should there be any.
#'
#' @importFrom shiny tags
#'
#' @keywords internal
assets <- function(){
	list(
		serveAssets(), # base assets (assets.R)
		tags$head(
			# Place any additional depdendencies here
			# e.g.: CDN
		)
	)
}
