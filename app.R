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

run(setup_pool(RPostgres::Postgres()))
