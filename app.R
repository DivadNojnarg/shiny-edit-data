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
  output$whoami <- renderText(whoami())

  # RESET DATA -------------------------------------------------------------
  # Only for debugging


  # INIT DATA --------------------------------------------------------------
  w <- Waiter$new()
  dat <- pin_reactive_read(board, pin_name, interval = 1000)
  state <- reactiveValues(
    init = TRUE,
    data_state = NULL,
    init_hash = NULL,
    hash = NULL,
    has_changed = NULL,
    is_admin = NULL
  )

  init_server("init", state, w, first_version)

  # Reload data
  observeEvent(dat(), {
    message("INIT DATA AND BUTTONS")
    # Create/update a state which user can edit
    state$data_state <- dat()

    # Add validate button for admin
    state$data_state$validate <- rep(NA, nrow(state$data_state))

    # Handle status column
    state$data_state$status <- apply_status(state$data_state)

    # Initial locking
    locked <- find_projects_to_lock(state$data_state[1:10, ], state$is_admin)
    # Tell JS which button to lock/unlock
    session$sendCustomMessage("toggle-buttons", locked)
  })

  # LOCK BUTTON --------------------------------------------------------------

  ## Note: the confirm button for edit can be accessed via <module_id>-update
  # LOCK button
  lock_row_server(
    "lock_row",
    reactive(input[["edit-update"]]),
    state,
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
  allow_save_server("allow_save", state, edit_vals)


  # SAVE CHANGES OR UNLOCK --------------------------------------------------------------

  # When modal closed, we capture which button we should unlock
  modal_closed <- reactive({
    req(input[["edit-update"]])
    input[[sprintf("modal_%s_closed", input[["edit-update"]])]]
  })

  save_data_server(
    "save_data",
    modal_closed,
    state,
    res_edited,
    reactive(input[["edit-update"]]),
    board
  )

  # TABLE -------------------------------------------------------------
  res_edited <- edit_data_server(
    id = "edit",
    data_r = reactive({
      state$data_state[, c(visible_internal_cols, find_data_cols(first_version), invisible_internal_cols)]
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
      columns = create_table_cols(first_version, state),
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
    if (state$init) {
      w$hide()
      state$init <- FALSE
    }

    range <- seq(current_page() * 10 - 9,  current_page() * 10)
    dat <- state$data_state[range, ]
    # Admin can edit all rows regardless of their locked state
    locked <- find_projects_to_lock(dat, state$is_admin)
    # Tell JS which button to lock/unlock
    session$sendCustomMessage("toggle-buttons", locked)
  })

  output$highlight_changes <- renderUI({
    changes <- which(state$data_state$locked == TRUE)
    req(length(changes) > 0)
    tagList(lapply(changes, \(change) {
      tags$style(sprintf(
        ".table-row-%s { background: var(--bs-gray-200); transition: background 1s cubic-bezier(0.785, 0.135, 0.15, 0.86); color: black; }",
        change
      ))
    }))
  })

  # VALIDATE A ROW --------------------------------------------------------------
  handle_validate_row("accept", state, board)
  handle_validate_row("reject", state, board)
}

shinyApp(ui, server)
