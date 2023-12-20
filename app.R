# NOTE: this app is a prof of concept of table editor.
# It is not production ready: needs to be packaged, modularised
# and tested.

library(shiny)
library(bslib)
library(pins)
library(datamods)
library(waiter)
library(rlang)
library(reactable)
library(htmltools)
library(parallel)

# depends on DivadNojnarg/datamods

board <- board_connect()
pin_name <- "user-input-poc-data"
versions <- pin_versions(board, pin_name)

first_version <- get_data_version(
  pin_name,
  board,
  versions,
  nrow(versions)
)

cols_to_edit <- c("comment", find_data_cols(first_version))

# Will show diff for real data columns
# between very first value and current

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
    div(
      class = "d-flex",
      actionButton("reset", "Reset data", class = "mx-2")
    ),
    uiOutput("highlight_changes"),
    edit_data_ui(id = "edit"),
    # To be able to use icons
    findDependencies(icon("check"))
  )
}

server <- function(input, output, session) {
  w <- Waiter$new()
  dat <- pin_reactive_read(board, "user-input-poc-data", interval = 1000)
  cache <- reactiveValues(
    init = TRUE,
    dat = NULL,
    init_hash = NULL,
    hash = NULL,
    has_changed = NULL,
    is_admin = NULL
  )

  output$whoami <- renderText(whoami())

  # INIT DATA --------------------------------------------------------------

  # Use admin mode: http://127.0.0.1:6505/?admin
  observeEvent(session$clientData$url_search, {
    query <- names(parseQueryString(session$clientData$url_search))
    if(is.null(query)) {
      cache$is_admin <- FALSE
    } else {
      if ("admin" %in% query) cache$is_admin <- TRUE
    }
  })

  observeEvent(input$reset, {
    board |> pin_write(
      cbind(
        status = rep("OK", nrow(iris)),
        last_updated_by = rep(NA, nrow(iris)),
        feedback = rep("", nrow(iris)),
        comment = rep("", nrow(iris)),
        do.call(rbind, lapply(1:100, \(x) iris)),
        locked = rep(FALSE, nrow(iris)),
        validated = rep(NA, nrow(iris))
      ),
      "user-input-poc-data"
    )
  })

  observeEvent(req(cache$init), {
    w$show()$update(
      html = tagList(
        p("Initializing app ..."),
        spin_flower()
      )
    )
    session$sendCustomMessage("send-init-data", first_version)
  })

  # Reload data
  observeEvent(dat(), {
    message("INIT DATA AND BUTTONS")
    # Create/update a cache which user can edit
    cache$dat <- dat()

    # Add validate button for admin
    cache$dat$validate <- rep(NA, nrow(cache$dat))

    # Handle status column
    cache$dat$status <- apply_status(cache$dat)

    # Initial locking
    locked <- find_projects_to_lock(cache$dat[1:10, ], cache$is_admin)
    # Tell JS which button to lock/unlock
    session$sendCustomMessage("toggle-buttons", locked)
  })

  # LOCK BUTTON --------------------------------------------------------------

  ## Note: the confirm button for edit can be accessed via <module_id>-update
  # LOCK button
  lock_row_server(
    "lock_row",
    reactive(input[["edit-update"]]),
    cache,
    board,
    w
  )

  # PREVENT SAVE UNCHANGED DATA --------------------------------------------------------------
  # Detect any change in the cols inputs
  edit_vals <- reactive({
    sapply(
      grep("edit-col", names(input), value = TRUE),
      \(el) input[[el]]
    )
  })
  allow_save_server("allow_save", cache, edit_vals)


  # SAVE CHANGES OR UNLOCK --------------------------------------------------------------

  # When modal closed, we capture which button we should unlock
  modal_closed <- reactive({
    req(input[["edit-update"]])
    input[[sprintf("modal_%s_closed", input[["edit-update"]])]]
  })

  save_data_server(
    "save_data",
    modal_closed,
    res_edited,
    reactive(input[["edit-update"]]),
    board,
    cache
  )

  # TABLE -------------------------------------------------------------
  res_edited <- edit_data_server(
    id = "edit",
    data_r = reactive({
      cache$dat[, c(visible_internal_cols, find_data_cols(first_version), invisible_internal_cols)]
    }),
    use_notify = FALSE,
    add = FALSE,
    delete = FALSE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_edit = cols_to_edit,
    var_mandatory = cols_to_edit,
    reactable_options = list(
      searchable = TRUE,
      # Note: pagination messes with the button disabled state on re-render
      pagination = TRUE,
      compact = TRUE,
      columns = create_table_cols(first_version, cache),
      # This is for applying color to rows with CSS
      rowClass = function(index) {
        paste0("table-row-", index)
      }
    )
  )

  current_page <- reactive({
    getReactableState("edit-table", "page")
  })

  # Toggle row based on pagination state
  observeEvent(current_page(), {
    # Hide loader when data are rendered
    if (cache$init) {
      w$hide()
      cache$init <- FALSE
    }

    range <- seq(current_page() * 10 - 9,  current_page() * 10)
    dat <- cache$dat[range, ]
    # Admin can edit all rows regardless of their locked state
    locked <- find_projects_to_lock(dat, cache$is_admin)
    # Tell JS which button to lock/unlock
    session$sendCustomMessage("toggle-buttons", locked)
  })

  output$highlight_changes <- renderUI({
    changes <- which(cache$dat$locked == TRUE)
    req(length(changes) > 0)
    tagList(lapply(changes, \(change) {
      tags$style(sprintf(
        ".table-row-%s { background: var(--bs-gray-200); transition: background 1s cubic-bezier(0.785, 0.135, 0.15, 0.86); color: black; }",
        change
      ))
    }))
  })

  # VALIDATE A ROW --------------------------------------------------------------
  handle_validate_row("accept", cache, board)
  handle_validate_row("reject", cache, board)
}

shinyApp(ui, server)
