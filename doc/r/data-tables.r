##------------------------------------------------------------------------------
## Data table loading (CSV files)
##------------------------------------------------------------------------------

data.path <- file.path("..", "..", "data")

catch.data.file <- "catch.csv"
survey.history.file <- "survey-history.csv"
sens.params.file <- "sensitivity-descriptions.csv"

cat("Loading all data tables (csv files) from ", data.path, "\n")
catches <- load.catches(file.path(data.path, catch.data.file))
survey.history <- load.survey.history(file.path(data.path, survey.history.file))
sens.desc <- read.csv(file.path(data.path, sens.params.file),
                      comment.char = "#",
                      header = FALSE)

cat("All data tables have been loaded ", data.path,"\n")
