options(
  "yaml.eval.expr" = TRUE,
  "app.config.path" = system.file("./config.yml", package = "tableEditor")
)

board <- setup_board()
test_that("setup_board works", {
  expect_s3_class(board, "pins_board")
})

pin_name <- config_get("pin_name")

# Reset + create pin + test
if (pin_exists(board, pin_name)) pin_delete(board, pin_name)
test_that("Prepare data works", {
  prepare_data(iris, board, pin_name)
  expect_true(pin_exists(board, pin_name))
})

first_version <- get_first_version(pin_name, board)
dat <- pin_read(board, pin_name)

session <- list()
session$user <- "plop"
state <- list()
state$is_admin <- FALSE

test_that("whoami works", {
  user <- whoami(session)
  expect_identical(user, session$user)
})

test_that("with_tooltip", {
  expect_snapshot(with_tooltip("tooltip", "value"))
})

#test_that("Apply status", {
#  out <- apply_status(dat)
#  expect_identical(unique(out), "OK")
#})

test_that("get_first_version works", {
  out <- get_first_version(pin_name, board)
  expect_identical(out, dat)
})

test_that("define_columns_diff works", {
  expect_snapshot(define_columns_diff(dat))
})

test_that("find_data_cols works", {
  out <- find_data_cols(dat)
  expect_identical(out, colnames(iris))
})

test_that("split_data_cols works", {
  out <- split_data_cols(dat)
  expect_type(out, "list")
  expect_length(out, 2)
  expect_identical(out$to_edit, c("comment", find_data_cols(dat)))
})

test_that("create_table_cols works", {
  expect_snapshot(create_table_cols(first_version, state))
  state$is_admin <- TRUE
  expect_snapshot(create_table_cols(first_version, state))
})
