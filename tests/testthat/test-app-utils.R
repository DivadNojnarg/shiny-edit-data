library(tableEditor)
library(DBI)
library(dplyr)

options(
  "yaml.eval.expr" = TRUE,
  "app.config.path" = system.file("./config.yml", package = "tableEditor")
)


test_that("create_timestamp works", {
  expect_true(is.numeric(create_timestamp()))
})

# Setup data
pool <- setup_pool(RSQLite::SQLite())
prepare_data(pool, iris, overwrite = TRUE)
dbWriteTable(
  pool,
  config_get("db_admins_name"),
  dplyr::tribble(
    ~user, ~channel, ~name, ~abteilung, ~position,
    "johndo1", "user", "John Doe", "IT", "Data Scientist",
    "davidgranjon", "admin", "David", "IT", "Leiter"
  )
)

# Mock state
state <- list()

# Mock session
session <- list()
session$user <- "plop"

test_that("Create pool works", {
  expect_s3_class(pool, "Pool")
})

test_that("prepare_data works", {
  expect_true(
    length(
      intersect(
        dbListTables(pool),
        c(
          config_get("db_data_name"),
          sprintf("%s_types", config_get("db_data_name")),
          config_get("db_admins_name")
        )
      )
    ) == 3
  )

  test <- dbGetQuery(pool, sprintf("SELECT * FROM %s", config_get("db_data_name")))
  expect_identical(nrow(test), nrow(iris))
})

test_that("find_factor_columns works", {
  types <- dbReadTable(pool, sprintf("%s_types", config_get("db_data_name")))
  expect_equal(tableEditor:::find_factor_columns(types), "Species")
})

test_that("Restore col type works", {
  dat <- dbReadTable(pool, config_get("db_data_name"))
  state$col_types <- dbReadTable(pool, sprintf("%s_types", config_get("db_data_name")))
  factor_cols <- tableEditor:::find_factor_columns(state$col_types)
  tmp <- dat |>
    mutate(across(all_of(factor_cols), tableEditor:::restore_col_type))

  expect_true(is.factor(tmp$Species))
})

test_that("Generate new id works", {
  dat <- dbReadTable(pool, config_get("db_data_name"))
  expect_identical(generate_new_id(dat), nrow(dat) + 1)
})

test_that("whoami works", {
  user <- tableEditor:::whoami(session)
  expect_identical(user, session$user)
})

test_that("is_user_admin", {
  state$is_admin <<- is_user_admin("davidgranjon", pool)
   expect_true(state$is_admin)
})

test_that("with_tooltip", {
  expect_snapshot(tableEditor:::with_tooltip("tooltip", "value"))
})

test_that("get_first_version works", {
  expect_s3_class(get_first_version(dbReadTable(pool, config_get("db_data_name"))), "data.frame")
})

test_that("define_columns_diff works", {
  expect_snapshot(tableEditor:::define_columns_diff(dbReadTable(pool, config_get("db_data_name"))))
})

test_that("find_cols_to_edit works", {
  out <- tableEditor:::find_cols_to_edit(dbReadTable(pool, config_get("db_data_name")))
  expect_identical(out, config_get("edit_cols"))
})

test_that("split_data_cols works", {
  out <- tableEditor:::split_data_cols(dbReadTable(pool, config_get("db_data_name")))
  expect_type(out, "list")
  expect_length(out, 2)
  expect_identical(out$to_edit, c("comment", tableEditor:::find_cols_to_edit(dbReadTable(pool, config_get("db_data_name")))))
})

test_that("create_table_cols works", {
  state$first_version <- dbReadTable(pool, config_get("db_data_name"))
  expect_snapshot(tableEditor:::create_table_cols(state))
  state$is_admin <- FALSE
  expect_snapshot(tableEditor:::create_table_cols(state))
})

pool::poolClose(pool)
