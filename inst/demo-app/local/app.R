# Launch the ShinyApp
# do not remove to keep push deploy button
# from RStudio

library(pins)
library(tableEditor)

# Prepare data: if they don't exist
#my_board <- board_local(versioned = TRUE)
#pin_delete("my-test-data", board = my_board)
# Will prepare a 15000 rows table (replicate iris 100 times ...)
# Useful to test performances with larger tables.
#prepare_data(iris, my_board, "my-test-data")

# Config path
options("yaml.eval.expr" = TRUE, "app.config.path" = "./config.yml")

run()
