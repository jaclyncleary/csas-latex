##------------------------------------------------------------------------------
## Data table loading (CSV files)
##------------------------------------------------------------------------------

data.path <- file.path("..", "..", "data")

catch.data.file <- "catch.csv"
harvest.activity.file <- "harvesting-activities.csv"


cat("Loading all data tables (csv files) from ", data.path, "\n")
catches <- load.csv(data.path,
                    catch.data.file)
harvest.activity <- load.csv(data.path,
                             harvest.activity.file,
                             header = FALSE)
