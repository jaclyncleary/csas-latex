##------------------------------------------------------------------------------
## Data table loading (CSV files)
##------------------------------------------------------------------------------

data.path <- file.path("..", "..", "data")

catch.data.file <- "catch.csv"
harvest.activity.file <- "harvesting-activities.csv"
management.file <- "management-decisions.csv"
gear.names.file <- "gear-names.csv"

cat("Loading all data tables (csv files) from ", data.path, "\n")
catches <- load.csv(data.path,
                    catch.data.file)
harvest.activity <- load.csv(data.path,
                             harvest.activity.file,
                             header = FALSE)
management.activity <- load.csv(data.path,
                                management.file,
                                header = FALSE)
gear.names <- load.csv(data.path,
                       gear.names.file,
                       header = FALSE)
