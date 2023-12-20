CONFIG_FILE <- "config.yml"

#' Read Config
#' @param path Allow to pass custom pass to search for config file.
#' @importFrom yaml read_yaml
#'
#' @keywords internal
config_read <- function(path = getOption("app.config.path")) {
  if (is.null(path)) path <- system.file(CONFIG_FILE, package = "tableEditor")
  read_yaml(path)
}

#' Get Value from Configuration
#'
#' Retrieve a value from the configuration file.
#'
#' @param value The value to retrieve.
#' @param config The configuration (from [config_read()]),
#' if missing the configuration file is red.
#'
#' @keywords internal
config_get <- function(value, config = NULL) {
  if (missing(value)) {
    stop("Missing `value`", call. = FALSE)
  }

  if (is.null(config)) {
    config <- config_read()
  }

  config[[value]]
}
