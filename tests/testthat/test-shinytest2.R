library(shinytest2)

options(
  "yaml.eval.expr" = TRUE,
  "app.config.path" = system.file("./config.yml", package = "tableEditor")
)

board <- setup_board()
pin_name <- config_get("pin_name")
if (pin_exists(board, pin_name)) {
  message("Removing and creating new test pin")
  pin_delete(board, pin_name)
  prepare_data(iris, board, pin_name)
}

test_that("{shinytest2} recording: test-local", {
  app <- AppDriver$new(variant = platform_variant(), name = "test-local", height = 1176,
      width = 807)
  app$set_inputs(waiter_hidden = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$wait_for_idle()
  app$expect_screenshot()
  app$set_inputs(`edit-update` = 1, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_hidden = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(`edit-col_10` = "setosa")
  app$click("edit-update_row")
  app$set_inputs(`edit-col_6` = 5.1)
  app$set_inputs(`edit-col_7` = 3.5)
  app$set_inputs(`edit-col_8` = 1.4)
  app$set_inputs(`edit-col_9` = 0.2)
  app$set_inputs(`edit-col_5` = "")
  app$set_inputs(`vscomp-search-input-5832` = "")
  app$wait_for_idle()
  app$expect_screenshot()
  app$set_inputs(`edit-col_5` = "test")
  app$click("edit-update_row")
  app$set_inputs(modal_1_closed = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$wait_for_idle()
  app$expect_screenshot()
  app$set_inputs(app_theme = "dark")
  app$set_window_size(width = 807, height = 1176)
  app$wait_for_idle()
  app$expect_screenshot()
  app$set_inputs(app_theme = "light")
  app$set_window_size(width = 807, height = 1176)
  app$set_inputs(`edit-update` = 2, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_shown = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(waiter_hidden = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$click("edit-update_row")
  app$set_inputs(`edit-col_6` = 4.9)
  app$set_inputs(`edit-col_7` = 3)
  app$set_inputs(`edit-col_5` = "")
  app$set_inputs(`vscomp-search-input-6656` = "")
  app$set_inputs(`edit-col_10_open` = TRUE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`edit-col_10` = "versicolor")
  app$set_inputs(`edit-col_10_open` = FALSE, allow_no_input_binding_ = TRUE)
  app$set_inputs(`edit-col_9` = 1.2)
  app$set_inputs(`edit-col_8` = 2.4)
  app$set_inputs(`edit-col_7` = 4)
  app$set_inputs(`edit-col_6` = 5.9)
  app$set_inputs(`edit-col_5` = "test 2")
  app$click("edit-update_row")
  app$set_inputs(modal_1_closed = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$set_inputs(modal_2_closed = TRUE, allow_no_input_binding_ = TRUE, priority_ = "event")
  app$wait_for_idle()
  app$expect_screenshot()
  app$click("reset-reset")
  app$wait_for_idle()
  app$expect_screenshot()
})