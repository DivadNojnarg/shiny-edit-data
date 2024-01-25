library(testthat)

test_dir("./testthat/", reporter = c("progress", "fail"))
shinytest2::test_app(reporter = c("progress", "fail"))
