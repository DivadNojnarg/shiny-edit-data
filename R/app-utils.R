#' Prepare data
#'
#' Add extra columns to given dataset needed by the editor app.
#'
#' @param dat Input data. Must be a dataframe or tibble.
#' @param board Board to save data.
#' @param pin_name Pin name.
#'
#' @return Save new data to a pin.
#' @export
prepare_data <- function(dat, board, pin_name) {
  board |> pin_write(
    cbind(
      status = rep("OK", nrow(dat)),
      last_updated_by = rep(NA, nrow(dat)),
      feedback = rep("", nrow(dat)),
      comment = rep("", nrow(dat)),
      do.call(rbind, lapply(1:100, \(x) dat)),
      locked = rep(FALSE, nrow(dat)),
      validated = rep(NA, nrow(dat))
    ),
    pin_name
  )
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
    user <- system("whoami", intern = TRUE)
    if (is.null(user)) user <- "test-user"
  }
  user
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
      if (!is_locked) "OK" else "IN REVIEW"
    } else {
      if (is_validated) "ACCEPTED" else "REJECTED"
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

#' Observer to handle row validation
#'
#' Rows can be rejected/accepted. This provides an observer to do so ...
#'
#' @param action One of `accept` or `reject`.
#' @param state App state.
#' @param board Pins board.
handle_validate_row <- function(action = c("accept", "reject"), state, board) {

  input <- get("input", parent.frame(n = 1))

  observeEvent(input[[sprintf("%s-row", action)]], {
    showModal(
      modalDialog(
        title = sprintf("You're about to %s the current changes", action),
        "Are you sure about your choice. Please confirm.",
        textAreaInput("feedback", "", placeholder = "optional feedback"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(sprintf("%s_ok", action), "OK")
        )
      )
    )
  })

  observeEvent(input[[sprintf("%s_ok", action)]], {
    removeModal()
    pin_dat <- state$data_cache
    pin_dat[input[[sprintf("%s-row", action)]], "status"] <- paste0(toupper(action), "ED")
    pin_dat[input[[sprintf("%s-row", action)]], "validated"] <- if (action == "accept") TRUE else FALSE
    pin_dat[input[[sprintf("%s-row", action)]], "feedback"] <- input$feedback
    board |> pin_write(pin_dat, "user-input-poc-data")
  })
}

#' Get a specific version of pinned data
#'
#' @param pin_name Pin name.
#' @param board Board.
#' @param versions Pins versions.
#' @param index Index to select
#'
#' @return The selected pin.
get_data_version <- function(pin_name, board, versions, index) {
  pin_read(
    board,
    pin_name,
    version = versions$version[index]
  )
}

#' Customize columns content
#'
#' Allows to setup a diff system so as to see whether data have changed.
#' Any change is depicted with red text next to the current value.
#'
#' @param dat Obtained from \link{get_data_version}.
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
  "validated",
  "locked"
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

#' Create reactable columns config
#'
#' Initialise reactable column config for the data editor.
#'
#' @param first_version Data for which to define the columns.
#' @param state App state.
#'
#' @return A list to pass to \link{edit_data_server}.
create_table_cols <- function(first_version, state) {
  c(
    define_columns_diff(first_version),
    list(
      # Don't show helper columns
      locked = colDef(show = FALSE),
      validated = colDef(show = FALSE),
      last_updated_by = colDef(name = "Last updated by"),
      validate = colDef(
        html = TRUE,
        show = if (state$is_admin) TRUE else FALSE,
        align = "center",
        header = with_tooltip("validate", "Validate current row?"),
        cell = JS(
          "function(cellInfo, state) {
              if (cellInfo.row.status === 'OK') {
                return null;
              } else if (cellInfo.row.status === 'IN REVIEW') {
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
          }")
      ),
      status = colDef(
        html = TRUE,
        cell = JS(
          "function(cellInfo, state) {
              let colorClass;
              switch (cellInfo.value) {
                case 'OK':
                  colorClass = 'bg-secondary';
                  break;
                case 'IN REVIEW':
                  colorClass = 'bg-warning';
                  break;
                case 'REJECTED':
                  colorClass = 'bg-danger';
                  break;
                case 'ACCEPTED':
                  colorClass = 'bg-success';
                  break;
              }
              return `<span class=\"badge ${colorClass}\">${cellInfo.value}</span>`
          }")
      )
    )
  )
}
