library(shiny)
library(bslib)
library(pins)
library(datamods)
library(waiter)

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5L),
  tags$head(
    tags$script(src = "app.js")
  ),
  useWaiter(),
  actionButton("reset", "Reset data"),
  edit_data_ui(id = "edit"),
  validation_ui("validation", display = "inline")
)

server <- function(input, output, session) {
  board <- board_connect()
  w <- Waiter$new()
  dat <- pin_reactive_read(board, "user-input-poc-data", interval = 1000)

  cache <- reactiveValues(dat = NULL)

  observeEvent(input$reset, {
    board |> pin_write(
      cbind(
        iris,
        comment = rep("", nrow(iris)),
        #last_updated_by = rep(NA, nrow(iris)),
        locked_by = rep(NA, nrow(iris)),
        locked = rep(FALSE, nrow(iris))),
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

  # Toggle all buttons according to the data state
  observeEvent(dat(), {
    # Create/update a cache which user can edit
    cache$dat <- dat()
    session$sendCustomMessage("toggle-buttons", cache$dat$locked)
  })

  col_names <- reactive({
    to_keep <- !(colnames(cache$dat) %in% c("locked", "locked_by"))
    colnames(cache$dat)[to_keep]
  })

  ## Note: the confirm button for edit can be accessed via <module_id>-update
  # LOCK button
  observeEvent(input[["edit-update"]], {

    # If user accidentally closes modal without commiting data
    # we'll unlock the current row.
    session$sendCustomMessage("close-modal-callback", input[["edit-update"]])

    # prevents from reloading the data within the session
    w$show()$update(
      html = tagList(
        p("Preparing the editor ..."),
        spin_flower()
      )
    )
    pin_data <- cache$dat
    pin_data[input[["edit-update"]], "locked"] <- TRUE
    pin_data[input[["edit-update"]], "locked_by"] <- system("whoami", intern = TRUE)
    board |> pin_write(pin_data, "user-input-poc-data")
    w$hide()
  })

  # When modal closed, we capture which button we should unlock
  modal_closed <- reactive({
    req(input[["edit-update"]])
    input[[sprintf("modal_%s_closed", input[["edit-update"]])]]
  })

  # TO DO: do we want to block the save result button if data have not changed?

  # UNLOCK button: either updated data or closed modal without update.
  observeEvent(
    c(
      res_edited(),
      modal_closed()
    ), {
    cache$dat <- cbind(
      res_edited(),
      locked = dat()$locked,
      locked_by = dat()$locked_by
    )
    # prevents from reloading the data within the session
    pin_data <- cache$dat
    pin_data[input[["edit-update"]], "locked"] <- FALSE
    pin_data[input[["edit-update"]], "locked_by"] <- NA
    #pin_data[input[["edit-update"]], "last_updated_by"] <- system("whoami", intern = TRUE)
    board |> pin_write(pin_data, "user-input-poc-data")
  }, ignoreInit = TRUE)

  # Server
  res_edited <- edit_data_server(
    id = "edit",
    # Hide "locked" column to end users
    data_r = reactive(cache$dat[, col_names()]),
    add = FALSE,
    delete = FALSE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_edit = col_names(),
    var_mandatory = col_names()
  )

  # Cleanup if crashed or when closed... Avoids
  # to accidentally have locked rows by a given user.
  session$onSessionEnded(function() {
    # Only cleanup the current user rows ...
    isolate({
      to_update <- which(cache$dat$locked_by == system("whoami", intern = TRUE))
      if (length(to_update) > 0) {
        cache$dat$locked_by[to_update] <- rep(NA, length(to_update))
        cache$dat$locked[to_update] <- rep(FALSE, length(to_update))
        board |> pin_write(cache$dat, "user-input-poc-data")
      }
    })
  })
}

shinyApp(ui, server)
