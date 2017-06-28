make.prop.female.table <- function(tab,
                                   digits = 3,
                                   xcaption = "default",
                                   xlabel   = "default",
                                   font.size = 9,
                                   space.size = 10,
                                   placement = "H"){
  ## Build the table of proportion female data
  ##
  ## tab - the table of data as read in by load.csv()
  ## digits - number of digits after decimal point to show in the indices
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  colnames(tab) <- c(latex.bold("Year"),
                     latex.mlc(c("Fishery",
                                 "Coastwide")),
                     latex.mlc(c("Fishery",
                                 "WCVI")),
                     latex.mlc(c("Fishery",
                                 "QCS+HS")),
                     latex.mlc(c("Survey",
                                 "QCSSS")),
                     latex.mlc(c("Survey",
                                 "HSMAS")),
                     latex.mlc(c("Survey",
                                 "HSSS")),
                     latex.mlc(c("Survey",
                                 "WCVI")))

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(as.data.frame(tab),
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        hline.after = c(0),
        booktabs = TRUE)
}
