##------------------------------------------------------------------------------
## Data table loading (CSV files)
##------------------------------------------------------------------------------

data.path <- file.path("..", "..", "data")

catch.data.file <- "catch.csv"
survey.history.file <- "survey-history.csv"
sens.params.file <- "sensitivity-descriptions.csv"
trawl.bio.file <- "trawl-obs-len-wt-age.csv"
gear.names.file <- "gear-names.csv"
prop.female.file <- "proportion-female.csv"
bio.file <- "bio.dat"


cat("Loading all data tables (csv files) from ", data.path, "\n")
catches <- load.csv(data.path, catch.data.file)
survey.history <- load.csv(data.path, survey.history.file)
sens.desc <- load.csv(data.path, sens.params.file)
trawl.bio <- load.csv(data.path, trawl.bio.file)
prop.female <- load.csv(data.path, prop.female.file)
gear.names <- load.csv(data.path, gear.names.file, header = FALSE)

## bio is an ascii file which was saved from iscam-gui using:
## save(bio, "bio.dat", ascii = TRUE)
load(file.path(data.path, bio.file))
cat("All data tables have been loaded ", data.path, "\n")
