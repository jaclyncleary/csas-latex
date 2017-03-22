################################################################################
## Data table loading
################################################################################
data.path <- file.path("..", "..", "data")

catch.data.file <- "landings-tac-history.csv"
survey.history.file <- "survey-history.csv"

cat("Loading all data tables (csv files) from ", data.path, "\n")
catches <- load.catches(file.path(data.path, catch.data.file))
landings.vs.tac <- catches[[2]]
catches <- catches[[1]]
survey.history <- load.survey.history(file.path(data.path, survey.history.file))

cat("All data tables have been loaded ", data.path,"\n")
