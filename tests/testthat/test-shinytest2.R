library(shinytest2)
library(reactable)

test_that("{shinytest2} recording: edit-data-poc", {
  app <- AppDriver$new(variant = platform_variant(), name = "edit-data-poc", height = 1294,
      width = 1525)
  app$expect_screenshot(delay = 3)
})
