load.catches <- function(fn ## fn is the filename with relative path
                         ){
  ## Reads in the catches file, splits it up into two data frames,
  ##  one for catches and one for the catches/tac.
  ## In the catches data frame, any NA's will be replaced by zeroes.
  ## In the landings/tac table, they will remain NAs
  catches <- read.csv(fn)
  landings.vs.tac <- as.data.frame(cbind(Year = catches$Year,
                                         Ustotal = catches$Ustotal,
                                         CANtotal = catches$CANtotal,
                                         TOTAL = catches$TOTAL,
                                         TAC = catches$TAC,
                                         TACCAN = catches$TACCAN,
                                         TACUSA = catches$TACUSA))

  landings.vs.tac <- as.data.frame(cbind(landings.vs.tac,
                                         landings.vs.tac$Ustotal / landings.vs.tac$TACUSA * 100,
                                         landings.vs.tac$CANtotal / landings.vs.tac$TACCAN * 100,
                                         landings.vs.tac$TOTAL / landings.vs.tac$TAC * 100))

  colnames(landings.vs.tac) <- c("Year", "Ustotal", "CANtotal",
                                 "TOTAL", "TAC", "TACCAN", "TACUSA",
                                 "USATTAIN", "CANATTAIN", "ATTAIN")
  catches <- catches[,!names(catches) %in% c("TAC", "TACCAN", "TACUSA")]
  catches[is.na(catches)] <- 0
  return(list(catches = catches, landings.vs.tac = landings.vs.tac))
}

load.survey.history <- function(fn ## fn is the filename with relative path
                                ){
  ## Reads in the survey history file and returns it as a data frame

  dat <- read.csv(fn)
  return(dat)
}
