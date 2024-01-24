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

board <- setup_board()
# Create pin for CICD
if (identical(Sys.getenv("TESTTHAT"), "true")) {
  prepare_data(iris, board, config_get("pin_name"))
}

run(board)
