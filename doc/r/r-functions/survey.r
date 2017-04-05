make.survey.indices.table <- function(model,
                                      digits = 3,
                                      xcaption = "default",
                                      xlabel   = "default",
                                      font.size = 9,
                                      space.size = 10,
                                      placement = "H"){
  ## Returns an xtable in the proper format for survey indices.
  ## This function ignores area, group, sex and timing columns.
  ##
  ## model - a model object
  ## digits - number of digits after decimal point to show in the indices
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  curr.func.name <- get.curr.func.name()

  dat <- model$dat
  indices <- dat$indices
  indices.df <- as.data.frame(do.call(rbind, indices))
  gear.nums <- unique(indices.df$gear)

  indices <- lapply(indices,
                    function(x){
                      tmp <- as.data.frame(x[,c("iyr", "it", "wt")])
                      ## Change the wt to CV
                      tmp$wt <- 1 / tmp$wt
                      tmp})

  if(dat$has.gear.names){
    gear.names <- dat$gear.names[gear.nums]
    ## change gear names to be multi-line
    lst <- lapply(gear.names,
                  function(x){p <- strsplit(as.character(x), " +")[[1]]})
    gear.names <- sapply(lst, function(x){latex.mlc(x)})
  }else{
    cat0(curr.func.name, "Warning - no gear names were set in the data file.")
    gear.names <- sapply(gear.nums, function(x)paste0("Gear ", x))
  }
  tab <- data.frame()
  for(ind in 1:length(indices)){
    if(!nrow(tab)){
      tab <- indices[[ind]]
    }else{
      suppressWarnings(
        tab <- merge(tab, indices[[ind]], by = "iyr", all = TRUE)
      )
    }
  }
  tab[,-1] <- f(tab[,-1], 3)
  tab[,1] <- as.character(tab[,1])
  tab[] <- lapply(tab, function(x)gsub(" +NA$", "", x))

  colnames(tab) <- c("",
                     rep(c(latex.bold("Index"),
                           latex.bold("CV")),
                         length(indices)))

  ## Add the extra header spanning multiple columns
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$pos[[2]] <- nrow(tab)
  addtorow$command <-
    c(paste0("\\toprule ",
             latex.mrow(2, "*", latex.bold("Year")),
             paste(sapply(1:length(gear.names),
                    function(x){paste0(latex.amp(),
                                       latex.mcol(2,
                                                  "c",
                                                  latex.bold(gear.names[x])))}),
                   collapse = ""),
             latex.nline,
             "\\midrule"),
      "\\bottomrule")

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(as.data.frame(tab),
               caption = xcaption,
               label = xlabel,
               align = getAlign(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        add.to.row = addtorow,
        table.placement = placement,
        hline.after = c(0),
        NA.string = "--",
        booktabs = TRUE)
}
