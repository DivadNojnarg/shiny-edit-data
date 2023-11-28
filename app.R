library(shiny)
library(bslib)
library(pins)
library(datamods)
library(waiter)
library(waldo)

users <- data.frame(
  name = c("olajoke", "david"),
  is_admin = c(FALSE, FALSE)
)

whoami <- function(session = shiny::getDefaultReactiveDomain()) {
  # Posit Connect
  user <- session$user
  if (is.null(user)) {
    user <- "olajoke"
  }
  user
}

ui <- function(request) {
  fluidPage(
    theme = bslib::bs_theme(version = 5L),
    tags$head(
      tags$script(src = "app.js")
    ),
    useWaiter(),
    tags$div(
      class = "bg-light p-5 rounded-lg m-3",
      tags$h1(class = "display-4", HTML(sprintf("Welcome %s", uiOutput("whoami")))),
      p(class = "lead", "Edit contracts dashboard ...")
    ),
    actionButton("reset", "Reset data"),
    span(class = "badge bg-primary", "Admin:", textOutput("is_admin")),
    edit_data_ui(id = "edit"),
    validation_ui("validation", display = "inline")
  )
}

server <- function(input, output, session) {
  board <- board_connect()
  w <- Waiter$new()
  dat <- pin_reactive_read(board, "user-input-poc-data", interval = 1000)
  cache <- reactiveValues(dat = NULL)

  # Is user admin or not (will run once per user session)
  is_admin <- users[users$name == whoami(), "is_admin"]

  output$is_admin <- renderText(is_admin)
  output$whoami <- renderText(whoami())

  observeEvent(input$reset, {
    board |> pin_write(
      cbind(
        iris,
        comment = rep("", nrow(iris)),
        last_updated_by = rep(NA, nrow(iris)),
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

  cols_to_show <- reactive({
    to_keep <- !(colnames(cache$dat) %in% c("locked"))
    colnames(cache$dat)[to_keep]
  })

  cols_to_edit <- reactive({
    to_edit <- !(colnames(cache$dat) %in% c("locked", "last_updated_by"))
    colnames(cache$dat)[to_edit]
  })

  ## Note: the confirm button for edit can be accessed via <module_id>-update
  # LOCK button
  observeEvent(input[["edit-update"]], {
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

  # When modal closed, we capture which button we should unlock
  modal_closed <- reactive({
    req(input[["edit-update"]])
    input[[sprintf("modal_%s_closed", input[["edit-update"]])]]
  })

  # TO DO: do we want to block the save result button if data have not changed?

  # Update data if difference.
  observeEvent(modal_closed(), {
      tmp <- cbind(
        res_edited(),
        locked = dat()$locked
      )

      # Only save if there is a difference
      res <- waldo::compare(tmp, cache$dat)
      if (length(res) > 0) {
        cache$dat <- tmp
        pin_data <- cache$dat
        pin_data[input[["edit-update"]], "last_updated_by"] <- whoami()
        board |> pin_write(pin_data, "user-input-poc-data")
        message("UPDATING DATA")
      } else {
        # Unlock project for everyone in case of mistake
        if (is.na(cache$dat[input[["edit-update"]], "last_updated_by"])) {
          if (cache$dat[input[["edit-update"]], "locked"]) {
            message("UNLOCK EMPTY EDIT")
            pin_data <- cache$dat
            pin_data[input[["edit-update"]], "locked"] <- FALSE
            board |> pin_write(pin_data, "user-input-poc-data")
          }
        }
      }
    })

  # Server
  res_edited <- edit_data_server(
    id = "edit",
    # Hide "locked" column to end users
    data_r = reactive(cache$dat[, cols_to_show()]),
    add = FALSE,
    delete = FALSE,
    download_csv = FALSE,
    download_excel = FALSE,
    var_edit = cols_to_edit(),
    var_mandatory = cols_to_edit()
  )
}

shinyApp(ui, server)
