# Launch the ShinyApp
# do not remove to keep push deploy button
# from RStudio

library(htmlwidgets)
# Config path
options("yaml.eval.expr" = TRUE, "app.config.path" = "./config.yml")

tableEditor::run()
