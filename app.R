# Launch the ShinyApp
# do not remove to keep push deploy button
# from RStudio
pkgload::load_all(
	reset = TRUE,
	helpers = FALSE
)

options(
  "yaml.eval.expr" = TRUE,
  "app.config.path" = system.file("./config.yml", package = "tableEditor")
)

pool <- setup_pool(RSQLite::SQLite())
tables <- dbListTables(pool)
if (!length(tables)) {
  message("SETTING UP TABLES")
  prepare_data(pool, iris, overwrite = TRUE)
  dbWriteTable(
    pool,
    config_get("db_admins_name"),
    dplyr::tribble(
      ~user, ~channel, ~name, ~abteilung, ~position,
      "johndo1", "user", "John Doe", "IT", "Data Scientist",
      "davidgranjon", "admin", "David", "IT", "Leiter"
    ),
    overwrite = TRUE
  )
}

run(pool)
