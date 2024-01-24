#' Server
#'
#' Core server function.
#'
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#'
#' @import pins
#' @importFrom datamods edit_data_server
#' @importFrom reactable colDef JS getReactableState reactableTheme
#'
#' @noRd
#' @keywords internal
server <- function(input, output, session) {
  send_message <- make_send_message(session)

  output$whoami <- renderText(whoami())

  # INIT DATA --------------------------------------------------------------
  pin_name <- config_get("pin_name")
  board <- getShinyOption("board")
  first_version <- getShinyOption("first_version")

  # Allow user to pass in external column filters
  cols <- split_data_cols(first_version)

  w <- Waiter$new()
  input_dat <- pin_reactive_read(board, pin_name, interval = 1000)
  state <- reactiveValues(
    init = TRUE,
    data_cache = NULL,
    init_hash = NULL,
    hash = NULL,
    has_changed = NULL,
    is_admin = NULL
  )

  init_server("init", state, w, first_version, input_dat)

  # RESET DATA -------------------------------------------------------------
  # Only for debugging
  if (!config_get("production")) reset_server("reset", board, w)

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

  # TABLE -------------------------------------------------------------
  res_edited <- edit_data_server(
    id = "edit",
    data_r = reactive({
      state$data_cache[, cols$to_show]
    }),
    use_notify = FALSE,
    add = FALSE,
    delete = FALSE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_edit = cols$to_edit,
    var_mandatory = cols$to_edit,
    reactable_options = list(
      searchable = TRUE,
      # Note: pagination messes with the button disabled state on re-render
      pagination = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = create_table_cols(isolate(state$data_cache[, cols$to_show]), state),
      # This is for applying color to rows with CSS
      rowClass = function(index) {
        paste0("table-row-", index)
      },
      theme = reactive({
        if (input$app_theme == "light") {
          NULL
        } else {
          reactableTheme(
            color = "hsl(233, 9%, 87%)",
            backgroundColor = "hsl(233, 9%, 19%)",
            borderColor = "hsl(233, 9%, 22%)",
            stripedColor = "hsl(233, 12%, 22%)",
            highlightColor = "hsl(233, 12%, 24%)",
            inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
            pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
          )
        }
      })
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

    range <- seq(current_page() * 10 - 9, current_page() * 10)
    dat <- state$data_cache[range, ]
    # Admin can edit all rows regardless of their locked state
    locked <- find_projects_to_lock(dat, state$is_admin)
    # Tell JS which button to lock/unlock
    send_message("toggle-buttons", value = locked)
  })

  output$highlight_changes <- renderUI({
    changes <- which(state$data_cache$locked == TRUE)
    req(length(changes) > 0)
    tagList(lapply(changes, \(change) {
      tags$style(sprintf(
        ".table-row-%s { background: var(--bs-gray-200); transition: background 1s cubic-bezier(0.785, 0.135, 0.15, 0.86); color: black; }",
        change
      ))
    }))
  })

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

  # VALIDATE A ROW --------------------------------------------------------------
  handle_validate_row("accept", state, board)
  handle_validate_row("reject", state, board)
}
