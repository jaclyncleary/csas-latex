make.activities.table <- function(tab,
                                  xcaption = "default",
                                  xlabel   = "default",
                                  font.size = 9,
                                  space.size = 10,
                                  placement = "H"){
  ## Build the table of proportion female data
  ##
  ## tab - the table of data as read in by load.csv()
  ## xcaption - caption to appear in the calling document
  ## xlabel - the label used to reference the table in latex
  ## font.size - size of the font for the table
  ## space.size - size of the vertical spaces for the table
  ## digits - number of decimal points on % columns
  ## placement - latex code for placement of the table in document

  colnames(tab) <- c("",
                     latex.bold("HG"),
                     latex.bold("PRD"),
                     latex.bold("CC"),
                     latex.bold("SOG"),
                     latex.bold("HWCVI"))

  fsc <- c("\\midrule FSC",
           latex.mlc(c("SOK",
                       "(harvest of wild",
                       "SOK, closed ponds)"),
                     FALSE),
           latex.mlc(c("Whole herring",
                       "SOK",
                       "spawn-on-",
                       "boughs"),
                     FALSE),
           latex.mlc(c("SOK",
                       "(closed and open",
                       "ponds) spawn-on-",
                       "boughs"),
                     FALSE),
           latex.mlc(c("Whole herring",
                       "and spawn-on-",
                       "boughs"),
                     FALSE),
           latex.mlc(c("Whole herring",
                       "SOK (closed and",
                       "open ponds),",
                       "spawn-on-boughs"),
                     FALSE),
           " \\bottomrule ")
           ##paste0(latex.nline, " \\bottomrule "))

  tab <- rbind(tab, fsc)

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
        ##hline.after = c(0),
        booktabs = TRUE)
}
