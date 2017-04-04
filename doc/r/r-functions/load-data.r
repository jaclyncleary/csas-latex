load.catches <- function(fn){
  ## Reads in the catch csv file and returns it as a data frame
  ##
  ## fn is the filename with relative path

  read.csv(fn, stringsAsFactors = FALSE)
}

load.survey.history <- function(fn){
  ## Reads in the survey history csv file and returns it as a data frame
  ##
  ## fn is the filename with relative path
  read.csv(fn)
}
