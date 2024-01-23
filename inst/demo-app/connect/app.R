# Launch the ShinyApp
# do not remove to keep push deploy button
# from RStudio

library(htmlwidgets)
library(tableEditor)
# Config path
options("yaml.eval.expr" = TRUE, "app.config.path" = "./config.yml")

run(setup_board())
