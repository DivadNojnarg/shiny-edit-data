library(shiny)
library(bslib)
library(pins)
library(datamods)
library(waiter)
library(rlang)
library(reactable)
library(htmltools)

users <- data.frame(
  name = c("olajoke", "david"),
  is_admin = c(FALSE, TRUE)
)

# Finds current user on Posit Connect or locally
whoami <- function(session = shiny::getDefaultReactiveDomain()) {
  # Posit Connect
  user <- session$user
  if (is.null(user)) {
    user <- "david"
  }
  user
}

# Add tooltip to table header (bslib fails ...)
with_tooltip <- function(value, tooltip) {
  tags$abbr(
    style = "text-decoration: underline; text-decoration-style: dotted; cursor: help",
    title = tooltip, value
  )
}

# Give a status to a row
apply_status <- function(dat) {
  vapply(seq_len(nrow(dat)), \(i) {
    tmp <- dat[i, ]
    is_locked <- tmp$locked
    is_validated <- tmp$validated
    if (!is_locked && !is_validated) {
      "TO DO"
    } else if (is_locked && !is_validated) {
      "IN REVIEW"
    } else if (is_validated) {
      "DONE"
    }
  }, FUN.VALUE = character(1))
}

ui <- function(request) {
  fluidPage(
    theme = bslib::bs_theme(version = 5L),
    tags$head(
      tags$script(src = "app.js")
    ),
    useWaiter(),
    waiterShowOnLoad(
      html = tagList(
        p("Hello!"),
        spin_flower()
      )
    ),
    tags$div(
      class = "bg-light p-5 rounded-lg m-3",
      tags$h1(class = "display-4", HTML(sprintf("Welcome %s", uiOutput("whoami", inline = TRUE)))),
      p(class = "lead", "Edit contracts dashboard ...")
    ),
    actionButton("reset", "Reset data"),
    span(class = "badge bg-primary", "Admin:", textOutput("is_admin", inline = TRUE)),
    uiOutput("highlight_changes"),
    edit_data_ui(id = "edit"),
    validation_ui("validation", display = "inline"),
    # To be able to use icons
    findDependencies(icon("check"))
  )
}

server <- function(input, output, session) {
  board <- board_connect()
  w <- Waiter$new()
  dat <- pin_reactive_read(board, "user-input-poc-data", interval = 1000)
  cache <- reactiveValues(dat = NULL, init_hash = NULL, hash = NULL, has_changed = NULL)

  # Is user admin or not (will run once per user session)
  is_admin <- users[users$name == whoami(), "is_admin"]

  output$is_admin <- renderText(is_admin)
  output$whoami <- renderText(whoami())

  # INIT DATA --------------------------------------------------------------

  observeEvent(input$reset, {
    board |> pin_write(
      cbind(
        iris,
        comment = rep("", nrow(iris)),
        last_updated_by = rep(NA, nrow(iris)),
        status = rep("", nrow(iris)),
        validated = rep(FALSE, nrow(iris)),
        locked = rep(FALSE, nrow(iris))
      ),
      "user-input-poc-data"
    )
  })

  observeEvent(cache$dat, {
    w$show()$update(
      html = tagList(
        p("Initializing app ..."),
        spin_flower()
      )
    )
    Sys.sleep(1)
    w$hide()

  }, ignoreNULL = FALSE, once = TRUE)

  # Toggle all buttons according to the data state but not for admins
  observeEvent(dat(), {
    message("INIT DATA AND BUTTONS")
    # Create/update a cache which user can edit
    cache$dat <- dat()

    # Add validate button for admin
    if (is_admin) {
      cache$dat$validate <- rep(NA, nrow(cache$dat))
    }

    # Handle status column
    cache$dat$status <- apply_status(cache$dat)

    # Admin can edit all rows regardless of their locked state
    locked <- if (is_admin) {
      rep(FALSE, nrow(cache$dat))
    } else {
      # For a given user, we unlock all rows where she/he is the
      # last editor so we can still provide corrections.
      tmp <- which(cache$dat$last_updated_by == whoami())
      dont_lock <- cache$dat$locked
      if (length(tmp > 0)) {
        dont_lock[tmp] <- FALSE
      }
      dont_lock
    }
    # Tell JS which button to lock/unlock
    session$sendCustomMessage("toggle-buttons", locked)
  })

  cols_to_edit <- reactive({
    to_edit <- !(colnames(cache$dat) %in% c("locked", "last_updated_by", "status", "validated", "validate"))
    colnames(cache$dat)[to_edit]
  })

  # LOCK BUTTON --------------------------------------------------------------

  ## Note: the confirm button for edit can be accessed via <module_id>-update
  # LOCK button
  observeEvent(input[["edit-update"]], {
    cache$has_changed <- NULL
    cache$hash <- NULL
    cache$init_hash <- NULL
    # If user accidentally closes modal without committing data
    # we'll unlock the current row.
    session$sendCustomMessage("close-modal-callback", input[["edit-update"]])

    # Pins is slow on connect so we must show a loader
    w$show()$update(
      html = tagList(
        p("Preparing the editor ..."),
        spin_flower()
      )
    )

    # Only lock is not locked
    if (!cache$dat[input[["edit-update"]], "locked"]) {
      message("LOCKING PROJECT")
      # prevents from reloading the data within the session
      pin_data <- cache$dat
      pin_data[input[["edit-update"]], "locked"] <- TRUE
      board |> pin_write(pin_data, "user-input-poc-data")
    }
    w$hide()
  })

  # PREVENT SAVE UNCHANGED DATA --------------------------------------------------------------

  edit_vals <- reactive({
    sapply(
      grep("edit-col", names(input), value = TRUE),
      \(el) input[[el]]
    )
  })

  observeEvent(req(length(edit_vals()) > 0), {
    if (is.null(cache$hash)) {
      cache$has_changed <- FALSE
      cache$hash <- rlang::hash(edit_vals())
      cache$init_hash <- cache$hash
      return(NULL)
    }

    if (hash(edit_vals()) != cache$init_hash) {
      if (hash(edit_vals()) != cache$hash) {
        cache$hash <- rlang::hash(edit_vals())
        cache$has_changed <- TRUE
      }
    } else {
      cache$hash <- rlang::hash(edit_vals())
      cache$has_changed <- FALSE
    }
  })
  # Block the save result button if data have not changed
  observeEvent(cache$has_changed, {
    session$sendCustomMessage("can-save", cache$has_changed)
  })

  # SAVE CHANGES OR UNLOCK --------------------------------------------------------------

  # When modal closed, we capture which button we should unlock
  modal_closed <- reactive({
    req(input[["edit-update"]])
    input[[sprintf("modal_%s_closed", input[["edit-update"]])]]
  })

  # Update data if difference.
  observeEvent(modal_closed(), {
    if (!is.null(cache$has_changed) && cache$has_changed) {
      pin_data <- res_edited()
      # invalidate whenever modified
      pin_data[input[["edit-update"]], "validated"] <- FALSE
      # Don't save the button column
      pin_data$validate <- NULL
      pin_data[input[["edit-update"]], "last_updated_by"] <- whoami()
      board |> pin_write(pin_data, "user-input-poc-data")
      message("UPDATING DATA")
    } else {
      # Unlock project for everyone in case of mistake
      if (is.na(cache$dat[input[["edit-update"]], "last_updated_by"])) {
        if (cache$dat[input[["edit-update"]], "locked"]) {
          message("UNLOCK EMPTY EDIT")
          pin_data <- cache$dat
          pin_data$validate <- NULL
          pin_data[input[["edit-update"]], "locked"] <- FALSE
          board |> pin_write(pin_data, "user-input-poc-data")
        }
      }
    }
  })

  # TABLE --------------------------------------------------------------
  table_cols <- list(
    # Don't show helper columns
    locked = colDef(show = FALSE),
    validated = colDef(show = FALSE),
    last_updated_by = colDef(name = "Last updated by"),
    status = colDef(
      html = TRUE,
      filterable = TRUE,
      cell = function(value, index, name) {
        badge_color <- switch(
          value,
          "TO DO" = "secondary",
          "IN REVIEW" = "danger",
          "DONE" = "success"
        )
        as.character(span(class = sprintf("badge bg-%s", badge_color), value))
      },
      filterInput = function(values, name) {
        tags$select(
          # Set to undefined to clear the filter
          onchange = sprintf("Reactable.setFilter('%s', '%s', event.target.value || undefined)", "edit-table", name),
          # "All" has an empty value to clear the filter, and is the default option
          tags$option(value = "", "All"),
          lapply(unique(values), tags$option),
          "aria-label" = sprintf("Filter %s", name),
          style = "width: 100%; height: 28px;"
        )
      }
    )
  )

  # Only admins can see the validate button
  if (is_admin) {
    table_cols$validate <- colDef(
      html = TRUE,
      align = "center",
      header = with_tooltip("validate", "Validate current row?"),
      cell = function(value, index, name) {
        if (index == 1) Sys.sleep(1)
        as.character(
          tags$button(
            disabled = if (
              cache$dat[index, "validated"] ||
              cache$dat[index, "status"] != "IN REVIEW"
            ) {
              NA
            },
            onclick = sprintf("Shiny.setInputValue('validate-row', %s, {priority: 'event'})", index),
            class = "btn btn-success",
            icon("check")
          )
        )
      }
    )
  }

  res_edited <- edit_data_server(
    id = "edit",
    # Hide "locked" column to end users
    data_r = reactive(cache$dat),
    add = FALSE,
    delete = FALSE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_edit = cols_to_edit(),
    var_mandatory = cols_to_edit(),
    reactable_options = list(
      # Note: pagination messes with the button disabled state on re-render
      pagination = FALSE,
      compact = TRUE,
      columns = table_cols,
      # This is for applying color to rows with CSS
      rowClass = function(index) {
        paste0("table-row-", index)
      }
    )
  )

  output$highlight_changes <- renderUI({
    changes <- which(cache$dat$locked == TRUE)
    req(length(changes) > 0)
    tagList(lapply(changes, \(change) {
      tags$style(sprintf(
        ".table-row-%s { background: var(--bs-gray-400); transition: background 1s cubic-bezier(0.785, 0.135, 0.15, 0.86); color: white; }",
        change
      ))
    }))
  })

  # VALIDATE A ROW --------------------------------------------------------------

  # TO DO: find a way to disable validate button if
  # data is already valid or if still in TODO state.
  observeEvent(input[["validate-row"]], {
    message("VALIDATE ROW")
    pin_dat <- cache$dat
    pin_dat[input[["validate-row"]], "status"] <- "DONE"
    pin_dat[input[["validate-row"]], "validated"] <- TRUE
    board |> pin_write(pin_dat, "user-input-poc-data")
  })
}

shinyApp(ui, server)
