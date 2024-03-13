#' @keywords internal
JS <- function (...) {
  x <- c(...)
  if (is.null(x))
    return()
  if (!is.character(x))
    stop("The arguments for JS() must be a character vector")
  x <- paste(x, collapse = "\n")
  structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}

is_local <- function() {
  Sys.getenv('SHINY_PORT') == ""
}

is_testing <- function() {
  identical(Sys.getenv("TESTTHAT"), "true")
}

`%OR%` <- function(a, b) {
  if (!length(a)) b else a
}

secure <- function(expr) {
  tryCatch(expr, error = function(e) {
    e
  })
}

#' Check database pool object
#'
#' Update loader if failure
#'
#' @param state App state.
#' @param loader R6 waiter instance.
#' @param session Shiny session object.
#'
#' @return A waiter object. Side effect: update the app
#' connected state.
#' @export
check_db_connection <- function(state, loader, session) {
  message <- NULL
  pool <- getShinyOption("pool")

  loader$show()$update(
    html = tagList(
      p("Initializing app ..."),
      message,
      spin_flower()
    )
  )

  if (inherits(pool, "error")) {
    message <- pool$message
    Sys.sleep(2)
    loader$update(
      html = tagList(
        div(icon("xmark", class = "fa-3x")),
        sprintf(
          "DB connection failed: %s. Trying to reconnect in 3 seconds ...",
          message
        )
      )
    )
    Sys.sleep(3)
    # We don't close the session as it might be a network
    # failure so we give a chance to reload the app ...
    # TO DO: maybe put a maximum amount of retry ...
    session$reload()
  } else {
    state$connected <- TRUE
  }
  Sys.sleep(1)
}

#' Check user config yaml file
#'
#' Run inside the init module.
#'
#' @param loader Waiter loader R6 instance.
#' @param session Shiny session object.
#' @return A waiter object. Side effect: disconnect the session
#' if config is invalid.
#' @export
check_config <- function(state, loader, session) {
  error <- NULL
  # DB table fail
  if (inherits(getShinyOption("col_types"), "error")) {
    error <- "Invalid db_data_name: check that the provided
         table exists in the database. Did you run prepare_data(...)?"
  } else {
    cols <- getShinyOption("col_names")
    if (length(config_get("filter_cols"))) {
      if (!all(config_get("filter_cols") %in% cols)) {
        error <- "filter_cols must be a subset of the data columns"
      }

      if (length(config_get("edit_cols"))) {
        if (!all(config_get("edit_cols") %in% config_get("filter_cols"))) {
          error <- c(error, "<br>edit_cols must be a subset of filter_cols.")
        }
      }

      if (length(config_get("hidden_cols"))) {
        if (!all(config_get("hidden_cols") %in% config_get("filter_cols"))) {
          error <- c(error, "<br>hidden_cols must be a subset of filter_cols.")
        }
      }

      if (any(config_get("hidden_cols") %in% config_get("edit_cols"))) {
        error <- c(error, "<br>hidden_cols can't be in edit_cols.")
      }

      if (length(config_get("col_defs"))) {
        if (!all(names(config_get("col_defs")) %in% config_get("filter_cols"))) {
          error <- c(error, "<br>col_def names must belong to filter_cols")
        }
      }

    } else {
      # Even if filter_cols is NULL people can still set
      # hidden_cols and edit_cols?
      if (length(config_get("hidden_cols"))) {
        if (!all(config_get("hidden_cols") %in% cols)) {
          error <- "<br>hidden_cols must be a subset of the data columns"
        }
      }
      if (length(config_get("edit_cols"))) {
        if (!all(config_get("edit_cols") %in% cols)) {
          error<- c(error, "<br>edit_cols must be a subset of the data columns")
        }
      }

      if (length(config_get("col_defs"))) {
        if (!all(names(config_get("col_defs")) %in% cols)) {
          error<- c(error, "<br>col_defs names must be a subset of the data columns")
        }
      }
    }
  }

  if (!is.null(error)) {
    loader$update(
      html = tagList(
        div(icon("xmark", class = "fa-3x")),
        lapply(error, \(e) HTML(e)),
        if (is.null(error)) spin_flower()
      )
    )
    session$close()
    state$connected <- FALSE
  } else {
    Sys.sleep(0.5)
  }
}

#' Get current user
#' Must be called from the shiny server function.
#' On Posit connect, get `session$user`. Locally will try to run `whoami`.
#' If not found hardcode the current user to `test-user`.
#'
#' @param session Shiny session object.
#'
#' @return The current user.
whoami <- function(session = shiny::getDefaultReactiveDomain()) {
  # Posit Connect
  user <- session$user
  if (is.null(user) && is_local()) {
    user <- system("whoami", intern = TRUE)
  }
  if (!is.null(user)) tolower(user)
}

#' Check if we can find the connect user
#'
#' @param state App state.
#' @param loader Screen loader to display feedback message
#' in case of error.
#' @return Loader object. Close session if invalid user.
#'
#' @keywords internal
check_if_user_logged <- function(state, loader) {
  state$user <- whoami()
  if (is.null(state$user)) {
    Sys.sleep(2)
    loader$update(
      html = tagList(
        div(icon("xmark", class = "fa-3x")),
        p(
          "Can't find current user. If the app
          runs on Posit Connect, please ensure
          to be connected before accessing it."
        ),
      )
    )
    session$close()
  }
}

#' Converts POSIXct to numeric
#'
#'
#' @return Numeric
#' @export
create_timestamp <- function() {
  as.numeric(Sys.time())
}

#' Prepare data
#'
#' Add extra columns to a given dataset needed by the editor app.
#'
#' @param con Database pool.
#' @param dat Input data. Must be a dataframe or tibble.
#' @param overwrite Whether to reset the existing table. Default
#' to FALSE.
#'
#' @return Save new data in the provided database table.
#' @export
prepare_data <- function(con, dat = lab, overwrite = FALSE) {
  dat <- if (!is.null(config_get("filter_cols"))) {
    dat[, config_get("filter_cols")]
  } else {
    dat
  }

  tmp <- cbind(
    id = seq_len(nrow(dat)),
    status = rep(config_get("status_ok"), nrow(dat)),
    last_updated_by = rep(NA_character_, nrow(dat)),
    feedback = rep("", nrow(dat)),
    comment = rep("", nrow(dat)),
    dat,
    locked = rep(0, nrow(dat)),
    validated = rep(NA_real_, nrow(dat)),
    timestamp = create_timestamp()
  )

  dbWriteTable(
    con,
    config_get("db_data_name"),
    tmp,
    overwrite = overwrite,
    row.names = TRUE
  )

  # Store columnes types metadata since factor is
  # lost when stored in the DB. This is needed by
  # datamods to properly show edit inputs based on the
  # column type.
  tmp_types <- sapply(dat, class, USE.NAMES = FALSE)

  dbWriteTable(
    con,
    sprintf("%s_types", config_get("db_data_name")),
    tibble(name = colnames(dat), type = tmp_types),
    overwrite = overwrite
  )
}

#' Find only factor type
#'
#' @param dat A dataframe containing columns metadata with name
# and type columns.
#'
#' @keywords internal
find_factor_columns <- function(dat) {
  dat[dat$type == "factor", "name"]
}

#' Check column type compared to metadata
#'
#' If type does not match, the right type is restored
#' from the metadata.
#'
#' @param x Column name.
#'
#' @keywords internal
restore_col_type <- function(x) {
  cur <- cur_column()
  cur_type <- class(x)
  state <- get("state", parent.frame(n = 1))
  saved_type <- state$col_types[state$col_types$name == cur, "type"]
  if (saved_type != cur_type) {
    as.factor(x)
  }
}

#' Generate new DB id
#'
#' @param dat Data.
#'
#' @return An integer.
#' @note ID could also be auto incremented when
#' creating the database ...
#' @export
generate_new_id <- function(dat) {
  max(dat$id) + 1
}

#' Setup database pool
#'
#' Establish a database connection with the
#' provided driver.
#'
#' @param driver DB driver.
#' @param ... Other params passed to dbConnect.
#'
#' @return A database pool
#' @export
#' @import pool
setup_pool <- function(driver, ...) {
  tryCatch({
    if (is_testing()) {
      dbPool(driver, ...)
    } else {
      dbPool(
        drv = driver,
        ...,
        dbname = config_get("db_name"),
        host = Sys.getenv("DB_HOST"),
        user = config_get("db_user"),
        password = Sys.getenv("DB_PASSWORD")
      )
    }
  }, error = function(e) {
    e
  })
}

#' Find if current use is admin
#'
#' @param user App user. Given by state$user.
#' @param con Database pool.
#'
#' @return Boolean
#' @export
is_user_admin <- function(user, con) {
  admins <- dbReadTable(
    con,
    config_get("db_admins_name")
  )
  tmp <- admins[
    tolower(admins[[config_get("admin_user_col")]]) == user,
    config_get("admin_type_col")
  ]
  if (length(tmp)) grepl("admin", tmp, ignore.case = TRUE) else FALSE
}

#' Adds tooltip to table header
#'
#' @param value Text.
#' @param tooltip tooltip.
#'
#' @return Adds tooltip to a given tag.
with_tooltip <- function(value, tooltip) {
  tags$abbr(
    style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
    title = tooltip, value
  )
}

#' Find project list to lock
#'
#' @param dat Data to process.
#' @param is_admin Whether the current user belongs to an admin list.
#' @param user App user.
#'
#' @return A boolean vector indicating whether the row should be locked. This
#' is useful later for JS.
find_projects_to_lock <- function(dat, is_admin, user) {
  if (is_admin) {
    rep(FALSE, nrow(dat))
  } else {
    # For a given user, we unlock all rows where she/he is the
    # last editor so we can still provide corrections.
    tmp <- which(dat$last_updated_by == user)
    dont_lock <- dat$locked
    if (length(tmp > 0)) {
      dont_lock[tmp] <- FALSE
    }
    dont_lock
  }
}

#' Get first version of each row
#'
#' TBD
#'
#' @param dat Database data.
#'
#' @return A dataframe.
#' @export
#' @importFrom rlang .data
get_first_version <- function(dat) {
  dat |>
    mutate(row_names = as.numeric(.data$row_names)) |>
    group_by(.data$row_names) |>
    slice_min(id) |>
    ungroup() |>
    arrange(.data$row_names)
}

#' Get last version of each row
#'
#' TBD
#'
#' @param dat Database data.
#'
#' @return A dataframe.
#' @export
get_last_version <- function(dat) {
  dat |>
    mutate(row_names = as.numeric(.data$row_names)) |>
    group_by(.data$row_names) |>
    slice_max(id) |>
    ungroup() |>
    arrange(.data$row_names)
}

#' Customize columns content
#'
#' Allows to setup a diff system so as to see whether data have changed.
#' Any change is depicted with red text next to the current value.
#'
#' @param dat Obtained from \link{get_first_version}.
#'
#' @return A list of options to pass to a reactable columns options.
#' @note Only editable columns are considered.
#' @keywords internal
define_columns_diff <- function(dat) {
  data_cols <- find_cols_to_edit(dat)

  defs <- lapply(data_cols, \(col) {
    colDef(
      cell = JS("
      function(cellInfo, state) {
        let isChanged = '';
        let initVal = initData[cellInfo.column.name][cellInfo.index];
        if (initVal !== null && initVal !== cellInfo.value) {
          isChanged = `<span style=\"color: red;\">(old: ${initVal})</span>`;
        }
        return `<div>${cellInfo.value} ${isChanged}</div>`
    }
  "),
  html = TRUE
    )
  })
  names(defs) <- data_cols
  defs
}

#' Customize hidden columns
#'
#' For each hidden column, indicate reactable to hide it
#'
#' @param dat Data.
#'
#' @return A list of options to pass to a reactable columns options.
#' @keywords internal
define_hidden_cols <- function(dat) {
  cols_to_hide <- c(
    invisible_internal_cols,
    config_get("hidden_cols")
  )

  setNames(
    lapply(cols_to_hide, \(col) {
      colDef(show = FALSE)
    }),
    cols_to_hide
  )
}

# These are the columns added by the app but which don't need to be shown to the user.
invisible_internal_cols <- c(
  "id",
  "row_names",
  "validated",
  "locked",
  "timestamp"
)

# These are the columns added by the app and have to be visible.
visible_internal_cols <- c(
  "status",
  "validate",
  "last_updated_by",
  "feedback",
  "comment"
)

# These are the columns added by the app to the existing data
internal_cols <- c(
  visible_internal_cols,
  invisible_internal_cols
)

#' Find columns to edit
#'
#' Used by \link{define_columns_diff}.
#'
#' @param dat Initial data without the editor columns.
#'
#' @return A vector of columns.
find_cols_to_edit <- function(dat) {
  if (!is.null(config_get("edit_cols"))) {
    config_get("edit_cols")
  } else {
    cols <- colnames(dat)
    cols[-which(cols %in% internal_cols)]
  }
}

#' Find columns to show
#'
#' Used by \link{define_columns_diff}.
#' Either filtered columns if the config is not empty.
#' Otherwise return all columns except the app internal ones.
#'
#' @param dat Initial data without the editor columns.
#'
#' @return A vector of columns.
find_cols_to_show <- function(dat) {
  cols <- colnames(dat)
  if (!is.null(config_get("filter_cols"))) {
    to_show <- which(cols %in% config_get("filter_cols"))
    cols[to_show]
  } else {
    cols[-which(cols %in% internal_cols)]
  }
}

#' Split data cols
#'
#' Cols are split into editable cols and showable cols.
#'
#' @param dat Data to process.
#'
#' @return A list with 2 entries.
split_data_cols <- function(dat) {
  list(
    to_edit = c("comment", find_cols_to_edit(dat)),
    to_show = c(visible_internal_cols, find_cols_to_show(dat), invisible_internal_cols)
  )
}

#' Create validate column html tags
#'
#' @keywords internal
create_validate_col <- function() {
  JS(
    sprintf(
      "function(cellInfo, state) {
         if (cellInfo.row.status === '%s') {
           return null;
         } else if (cellInfo.row.status === '%s') {
           return `
             <div>
               <button
                 onclick=\"Shiny.setInputValue('accept-row', ${cellInfo.index + 1}, {priority: 'event'})\"
                 class='btn btn-success btn-sm'
               >
                 <i class=\"fas fa-check\" role=\"presentation\" aria-label=\"check icon\"></i>
               </button>
               <button
                 onclick=\"Shiny.setInputValue('reject-row', ${cellInfo.index + 1}, {priority: 'event'})\"
                 class='btn btn-danger btn-sm'
               >
                 <i class=\"fas fa-xmark\" role=\"presentation\" aria-label=\"xmark icon\"></i>
               </button>
             </div>
           `
         } else if (cellInfo.row.validated) {
           return `
             <button
               onclick=\"Shiny.setInputValue('reject-row', ${cellInfo.index + 1}, {priority: 'event'})\"
               class='btn btn-danger btn-sm'
             >
               <i class=\"fas fa-xmark\" role=\"presentation\" aria-label=\"xmark icon\"></i>
             </button>
           `
         }
      }",
      config_get("status_ok"),
      config_get("status_review")
    )
  )
}

#' Create status column tag
#'
#' @keywords internal
create_status_col <- function() {
  JS(
    sprintf(
      "function(cellInfo, state) {
         let colorClass;
         switch (cellInfo.value) {
           case '%s':
             colorClass = 'bg-secondary';
             break;
           case '%s':
             colorClass = 'bg-warning';
             break;
           case '%s':
             colorClass = 'bg-danger';
             break;
           case '%s':
             colorClass = 'bg-success';
             break;
         }
        return `<span class=\"badge ${colorClass}\">${cellInfo.value}</span>`
      }",
      config_get("status_ok"),
      config_get("status_review"),
      config_get("status_rejected"),
      config_get("status_accepted")
    )
  )
}

#' Create reactable columns config
#'
#' Initialise reactable column config for the data editor.
#'
#' @param state App state.
#'
#' @return A list to pass to \link{edit_data_server}.
create_table_cols <- function(state) {
  tmp <- c(
    # Internal columns defs
    define_columns_diff(state$first_version),
    define_hidden_cols(state$first_version),
    list(
      last_updated_by = colDef(name = "Last updated by"),
      validate = colDef(
        html = TRUE,
        show = if (state$is_admin) TRUE else FALSE,
        align = "center",
        header = with_tooltip("validate", "Validate current row?"),
        cell = create_validate_col()
      ),
      status = colDef(
        html = TRUE,
        cell = create_status_col()
      )
    ),
    # User defined column defs
    if (length(config_get("col_defs")) > 0) config_get("col_defs")
  )

  # Align
  lapply(tmp, \(el) {
    el$align <- "center"
    el
  })
}
