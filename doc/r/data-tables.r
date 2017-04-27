##------------------------------------------------------------------------------
## Data table loading (CSV files)
##------------------------------------------------------------------------------

data.path <- file.path("..", "..", "data")

catch.data.file <- "catch.csv"
survey.history.file <- "survey-history.csv"
sens.params.file <- "sensitivity-descriptions.csv"
trawl.bio.file <- "trawl-obs-len-wt-age.csv"
gear.names.file <- "gear-names.csv"
bio.file <- "bio.csv"

cat("Loading all data tables (csv files) from ", data.path, "\n")
catches <- load.csv(data.path, catch.data.file)
survey.history <- load.csv(data.path, survey.history.file)
sens.desc <- load.csv(data.path, sens.params.file)
trawl.bio <- load.csv(data.path, trawl.bio.file, header = FALSE)
gear.names <- load.csv(data.path, gear.names.file, header = FALSE)
bio <- load.csv(data.path, bio.file)
cat("All data tables have been loaded ", data.path, "\n")
