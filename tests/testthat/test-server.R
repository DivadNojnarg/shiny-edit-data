options(
  "yaml.eval.expr" = TRUE,
  "app.config.path" = system.file("./config.yml", package = "tableEditor")
)

app <- run(setup_board())

testServer(app, {
  state$is_admin <- FALSE

  # Reactive values
  expect_null(state$data_cache)
  expect_true(state$init)
  expect_null(state$init_hash)
  expect_null(state$hash)
  expect_null(state$has_changed)
  expect_false(state$is_admin)

  # Output/reactives
  expect_error(output$highlight_changes)
  expect_error(modal_closed())
  expect_null(current_page())
  expect_length(edit_vals(), 0)

  session$flushReact()
  expect_identical(nrow(input_dat()), nrow(state$data_cache))

  #session$setInputs(`edit-update` = 1)
  #session$flushReact()
  #session$setInputs(`edit-col_5` = "comment", `edit-col_6` = 1000)
  #expect_length(edit_vals(), 2)
  #expect_length(state$hash, 1)
  #expect_identical(state$init_hash, state$hash)
  #expect_true(state$has_changed)
  #expect_identical(edit_vals()[["edit-col_5"]], "comment")
  #browser()
  #session$setInputs(`edit-update_row` = 1, "modal_1_closed" = 1)
  #print(state$data_cache[1, ])
  #print(res_edited()[1, ])
})
