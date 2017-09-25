make.maturity.table <- function(mat,
                                digits = 3,
                                xcaption = "default",
                                xlabel   = "default",
                                font.size = 9,
                                space.size = 10,
                                placement = "H"){
  ## mat - the maturity vectors data as loaded from maturity-vectors.csv
  ## digits - number of decimal places for the values
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## placement - latex code for placement of the table in document

  tab <- lapply(unique(mat$Stock),
              function(x){
                st <- mat[mat$Stock == x,]
                mdl <- lapply(unique(st$Model),
                            function(y){
                              st.mdl <- mat[mat$Stock == x & mat$Model == y,]
                              c(x, y, st.mdl$Maturity)
                            })
                do.call(rbind, mdl)
              })
  tab <- do.call(rbind, tab)
  age <- unique(mat$Age)
  ## Format table data
  tab[, 3:7] <- f(as.numeric(tab[, 3:7]), 4)
  tab[, 8:11] <- f(as.numeric(tab[, 8:11]), 1)

  colnames(tab) <- c(latex.bold("Stock"),
                     latex.bold("Model"),
                     latex.bold(age))
  addtorow <- list()
  addtorow$pos <- list()
  addtorow$pos[[1]] <- -1
  addtorow$command <- paste0("\\toprule",
                             latex.amp(2),
                             latex.mcol(length(age),
                                        "c",
                                        latex.bold("Maturity at age")),
                             latex.nline)

  size.string <- latex.size.str(font.size, space.size)
  print(xtable(tab,
               caption = xcaption,
               label = xlabel,
               align = get.align(ncol(tab))),
        caption.placement = "top",
        include.rownames = FALSE,
        sanitize.text.function = function(x){x},
        size = size.string,
        table.placement = placement,
        add.to.row = addtorow,
        booktabs = TRUE)

}
