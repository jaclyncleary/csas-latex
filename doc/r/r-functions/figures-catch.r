agg <- function(d, nm, weight.factor){
  ## Aggregate the data frame d by Year for the column name given in nm
  ## Also applies the weight factor
  d <- d[, c("Year", nm)]
  ct <- aggregate(d[, nm],
                  list(d$Year),
                  sum,
                  na.rm = TRUE)
  colnames(ct) <- c("year", "catch")
  ct$catch <- ct$catch / weight.factor
  ct
}

get.area.codes <- function(areas = c("3C",
                                     "3D",
                                     "5A",
                                     "5B",
                                     "5C",
                                     "5D",
                                     "5E")){
  ## Returns a vector of area codes correspponding to the areas given
  ## 3 = 3C, 4 = 3D, 5 = 5A, 6 = 5B, 7 = 5C, 8 = 5D, 9 = 5E
  ## Area hash table
  ahash <- matrix(ncol = 2, nrow = 7)
  ahash[1,] <- c(3, "3C")
  ahash[2,] <- c(4, "3D")
  ahash[3,] <- c(5, "5A")
  ahash[4,] <- c(6, "5B")
  ahash[5,] <- c(7, "5C")
  ahash[6,] <- c(8, "5D")
  ahash[7,] <- c(9, "5E")

  result <- ahash[ahash[,2] %in% areas,]
  if(class(result) == "character"){
    ## areas was a single value
    ret <- result[1]
  }else{
    ## areas was a vector
    ret <- result[,1]
  }
  as.numeric(ret)
}

make.catch.discard.plot <- function(catch.df,
                                    s.yr,
                                    e.yr,
                                    areas = c("3C",
                                              "3D",
                                              "5A",
                                              "5B",
                                              "5C",
                                              "5D",
                                              "5E"),
                                    weight.factor = 1e6,
                                    stacked = TRUE,
                                    leg = "topright",
                                    space.betw = 0.5,
                                    opacity = 75,
                                    add = FALSE,
                                    ind.letter = NULL){
  ## Returns a stacked barplot figure with catch and discards for the gear
  ##  requested. Assumes area, group, and sex are not used
  ##
  ## catch.df - the catch dataframe as found in catch.csv file
  ## s.yr - year to start at
  ## e.yr - year to end at
  ## areas - areas to include in the plot (aggregated)
  ## weight.factor - factor to divide the weight by
  ## stacked - if TRUE, make a stacked barplot, else side-by-side
  ## leg - where to place the legend
  ## space.betw - space between the bars
  ## opacity - transparency of the filled bars
  ## add - if TRUE, par will not be restored on exit

  if(!add){
    oldPar <- par(no.readonly = TRUE)
    on.exit(par(oldPar))
  }

  yrs <- start.yr:end.yr
  catches <- catches[catches$Year %in% yrs,]

  area.codes <- get.area.codes(areas)
  catches <- catches[catches$AreaCode %in% area.codes,]

  ct <- agg(catches, "CatchKG", weight.factor)
  d.ct <- agg(catches, "DiscardedKG", weight.factor)

  tab <- as.matrix(cbind(ct$catch, d.ct$catch))

  col.1 <- get.shade(1, opacity)
  col.2 <- get.shade(2, opacity)

  b <- barplot(t(tab),
               axes = FALSE,
               col = c(col.1, col.2),
               border = c("black", "black"),
               ylim = c(0, 1.1 * max(apply(tab, 1, sum))),
               las = 2)

  cex <- 0.7
  axis(2)
  axis(1,
       at = b,
       labels = yrs,
       tick = TRUE)
  box()
  x.label <- "Year"
  y.label <- "Catch (1000 t)"

  mtext(side = 1, line = 2, x.label)
  mtext(side = 2, line = 2, y.label)

  if(!is.null(leg)){
    leg.lst <- c("Catch", "Discards")
    leg.shade.cols <- c(col.1, col.2)
    leg.border.cols <- c("black", "black")
    legend(leg,
           leg.lst,
           col = leg.border.cols,
           fill = leg.shade.cols,
           bty = "n")
  }

  if(!is.null(ind.letter)){
    panel.letter(ind.letter)
  }
}
