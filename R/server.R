#' Server
#'
#' Core server function.
#'
#' @param input,output Input and output list objects
#' containing said registered inputs and outputs.
#' @param session Shiny session.
#'
#' @importFrom datamods edit_data_server
#' @importFrom reactable colDef getReactableState reactableTheme
#'
#' @noRd
#' @import DBI
#' @import dplyr
#' @keywords internal
server <- function(input, output, session) {
  send_message <- make_send_message(session)

  output$whoami <- renderText(state$user)

  # INIT DATA --------------------------------------------------------------
  w <- Waiter$new()
  state <- reactiveValues(
    init = TRUE,
    init_hash = NULL,
    hash = NULL,
    has_changed = NULL,
    is_admin = NULL,
    connected = FALSE,
    cols = NULL,
    col_types = getShinyOption("col_types"),
    first_version = NULL,
    user = NULL
  )

  init_server("init", state, getShinyOption("pool"), w)

  input_data <- refresh_data_server(
    "refresh",
    state,
    # So we also update unlock based on the pagination state
    current_page = reactive({
      trigger <- if (is.null(input$current_page)) {
        pagination_state()
      } else {
        input$current_page
      }
      req(trigger)
      print(trigger)
      trigger
    }),
    con = getShinyOption("pool"),
    w
  )

  # RESET DATA -------------------------------------------------------------
  # Only for debugging
  if (!config_get("production")) reset_server("reset", getShinyOption("pool"), w)

  # LOCK BUTTON --------------------------------------------------------------

  ## Note: the confirm button for edit can be accessed via <module_id>-update
  # LOCK button
  lock_row_server(
    id = "lock_row",
    trigger = reactive(input[["edit-update"]]),
    dat =  res_edited,
    state,
    con = getShinyOption("pool"),
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

  allow_save_server(
    id = "allow_save",
    trigger = edit_vals,
    state
  )

  # TABLE -------------------------------------------------------------
  res_edited <- edit_data_server(
    id = "edit",
    data_r = input_data,
    use_notify = FALSE,
    add = FALSE,
    delete = FALSE,
    download_csv = FALSE,
    download_excel = FALSE,
    modal_easy_close = FALSE, # Don't change, this is needed to unlock projects
    var_edit = split_data_cols(input_data())$to_edit,
    var_mandatory = split_data_cols(input_data())$to_edit,
    reactable_options = list(
      searchable = TRUE,
      # Note: pagination messes with the button disabled state on re-render
      pagination = TRUE,
      bordered = TRUE,
      compact = TRUE,
      columns = create_table_cols(state),
      # This is for applying color to rows with CSS
      rowClass = function(index) {
        sprintf("table-row-%s", index)
      }#, # TO DO: understand why bslib does not work on connect.
      #theme = reactive({
      #  if (input$app_theme == "light") {
      #    NULL
      #  } else {
      #    reactableTheme(
      #      color = "hsl(233, 9%, 87%)",
      #      backgroundColor = "hsl(233, 9%, 19%)",
      #      borderColor = "hsl(233, 9%, 22%)",
      #      stripedColor = "hsl(233, 12%, 22%)",
      #      highlightColor = "hsl(233, 12%, 24%)",
      #      inputStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      #      selectStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      #      pageButtonHoverStyle = list(backgroundColor = "hsl(233, 9%, 25%)"),
      #      pageButtonActiveStyle = list(backgroundColor = "hsl(233, 9%, 28%)")
      #    )
      #  }
      #})
    )
  )

  pagination_state <- reactive({
    getReactableState("edit-table", "page")
  })

  observeEvent(pagination_state(), {
    # Hide loader when data are rendered
    w$hide()
  }, once = TRUE)

  # Bypass reactable pagination reset default. Necessary to go back to
  # the right page after data are updated: see this issues ->
  # https://stackoverflow.com/questions/71998920/why-are-react-table-pagination-filter-and-sort-reset-automatically-when-its-t
  observeEvent(input_data(), {
    val <- input$current_page
    val <- if (is.null(val)) getReactableState("edit-table", "page")
    send_message("register-pagination", TRUE)
    send_message("go-to-page", page = val)
  })

  output$highlight_changes <- renderUI({
    changes <- which(input_data()$locked == TRUE)
    req(length(changes) > 0)
    tagList(lapply(changes, \(change) {
      tags$style(sprintf(
        ".table-row-%s {
          background: var(--bs-gray-200);
          transition: background 1s cubic-bezier(0.785, 0.135, 0.15, 0.86);
          color: black;
        }",
        change
      ))
    }))
  })

  # SAVE CHANGES OR UNLOCK --------------------------------------------------------------

  # When modal closed, we capture which button we should unlock
  unlock_row_server(
    id = "unlock_row",
    trigger = reactive(input[["edit-close_modal"]]),
    dat = res_edited,
    row_index = reactive(input[["edit-update"]]),
    con = getShinyOption("pool")
  )

  save_data_server(
    id = "save_data",
    trigger = reactive(input[["edit-update_row"]]),
    new_data = res_edited,
    row_index = reactive(input[["edit-update"]]),
    state = state,
    con = getShinyOption("pool")
  )

  # VALIDATE A ROW --------------------------------------------------------------
  validate_row_server("accept", reactive(input[["accept-row"]]), res_edited, state, getShinyOption("pool"))
  validate_row_server("reject", reactive(input[["reject-row"]]), res_edited, state, getShinyOption("pool"))
}
