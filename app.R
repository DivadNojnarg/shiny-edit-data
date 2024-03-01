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

app_pool <- setup_pool(RSQLite::SQLite())
tables <- dbListTables(app_pool)
if (!length(tables)) {
  message("SETTING UP TABLES")
  prepare_data(app_pool, iris, overwrite = TRUE)
  dbWriteTable(
    app_pool,
    config_get("db_admins_name"),
    dplyr::tribble(
      ~user, ~channel, ~name, ~abteilung, ~position,
      "johndo1", "user", "John Doe", "IT", "Data Scientist",
      "davidgranjon", "admin", "David", "IT", "Leiter"
    ),
    overwrite = TRUE
  )
}

run(app_pool)
