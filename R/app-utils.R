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
    locked = rep(FALSE, nrow(dat)),
    validated = rep(NA, nrow(dat)),
    timestamp = Sys.time()
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
#' @export
generate_new_id <- function(dat) {
  max(dat$id) + 1
}

#' Setup database pool
#'
#' Establish a database connection with the
#' provided driver.
#'
#' @param driver Default to Postgres.
#'
#' @return A database pool
#' @export
#' @import pool
setup_pool <- function(driver = RPostgres::Postgres()) {
  tryCatch({
    dbPool(
      drv = driver,
      dbname = config_get("db_name"),
      host = Sys.getenv("DB_HOST"),
      user = config_get("db_user"),
      password = Sys.getenv("DB_PASSWORD")
    )
  }, error = function(e) {
    e
  })
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
  if (is.null(user)) {
    user <- tryCatch(
      system("whoami", intern = TRUE),
      warning = function(w) {
        "unknown"
      }
    )
  }
  tolower(user)
}

#' Check if we can find the connect user
#'
#' @param loader Screen loader to display feedback message
#' in case of error.
#'
#' @keywords internal
check_if_user_logged <- function(loader) {
  tryCatch(whoami(), warning = function(w) {
    Sys.sleep(2)
    loader$update(
      html = tagList(
        p(
          sprintf(
            "%s. If the app
                  runs on Posit Connect, please ensure
                  to be connected ...",
            w
          )
        ),
        spin_flower()
      )
    )
  })
}

#' Find if current use is admin
#'
#' @param con Database pool
#'
#' @return Boolean
#' @export
is_user_admin <- function(con) {
  admins <- dbReadTable(
    con,
    config_get("db_admins_name")
  )
  tmp <- admins[admins[[config_get("admin_user_col")]] == whoami(), config_get("admin_type_col")]
  if (length(tmp)) tmp == "admin" else FALSE
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

#' Give a status to a row.
#'
#' There can be 4 statuses: OK (not changed), IN REVIEW (changed but not validated),
#' ACCEPTED (reviewed) and REJECTED (reviewed but needs further changes).
#'
#' @param dat Dataframe.
#'
#' @importFrom parallel mclapply detectCores
#'
#' @return A character vector containing statuses.
apply_status <- function(dat) {
  unlist(mclapply(seq_len(nrow(dat)), \(i) {
    tmp <- dat[i, ]
    is_locked <- tmp$locked
    is_validated <- tmp$validated

    if (is.na(is_validated)) {
      if (!is_locked) config_get("status_ok") else config_get("status_review")
    } else {
      if (is_validated) config_get("status_accepted") else config_get("status_rejected")
    }
  }, mc.cores = detectCores() / 2))
}

#' Find project list to lock
#'
#' @param dat Data to process.
#' @param is_admin Whether the current user belongs to an admin list.
#'
#' @return A boolean vector indicating whether the row should be locked. This
#' is useful later for JS.
find_projects_to_lock <- function(dat, is_admin) {
  if (is_admin) {
    rep(FALSE, nrow(dat))
  } else {
    # For a given user, we unlock all rows where she/he is the
    # last editor so we can still provide corrections.
    tmp <- which(dat$last_updated_by == whoami())
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
  dat %>%
    mutate(row_names = as.numeric(.data$row_names)) %>%
    group_by(.data$row_names) %>%
    slice_min(id) %>%
    ungroup() %>%
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
  dat %>%
    mutate(row_names = as.numeric(.data$row_names)) %>%
    group_by(.data$row_names) %>%
    slice_max(id) %>%
    ungroup() %>%
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
define_columns_diff <- function(dat) {
  data_cols <- find_data_cols(dat)

  defs <- lapply(data_cols, \(col) {
    colDef(
      cell = JS("
      function(cellInfo, state) {
        let isChanged = '';
        let initVal = initData[cellInfo.column.name][cellInfo.index];
        if (initVal !== cellInfo.value) {
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

#' Find the raw data cols
#'
#' Used by \link{define_columns_diff}.
#'
#' @param dat Initial data without the editor columns.
#'
#' @return A vector of columns.
find_data_cols <- function(dat) {
  cols <- colnames(dat)
  to_exclude <- which(cols %in% internal_cols)
  cols[-to_exclude]
}

#' Split data cols
#'
#' Cols are split into editable cols and showable cols.
#'
#' @param dat Data to process.
#'
#' @return A list with 2 entries.
split_data_cols <- function(dat) {
  filter_cols <- config_get("filter_cols")
  default_cols <- find_data_cols(dat)
  if (!is.null(filter_cols)) {
    to_keep <- which(default_cols %in% filter_cols)
    default_cols <- default_cols[to_keep]
  }

  list(
    to_edit = c("comment", default_cols),
    to_show = c(visible_internal_cols, default_cols, invisible_internal_cols)
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
  c(
    define_columns_diff(state$first_version),
    list(
      # Don't show helper columns
      id = colDef(show = FALSE),
      row_names = colDef(show = FALSE),
      timestamp = colDef(show = FALSE),
      locked = colDef(show = FALSE),
      validated = colDef(show = FALSE),
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
    )
  )
}
