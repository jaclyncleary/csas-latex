make.catches.table <- function(catches,
                               start.yr,
                               end.yr,
                               weight.factor = 1000,
                               xcaption = "default",
                               xlabel   = "default",
                               font.size = 9,
                               space.size = 10,
                               placement = "H"){
  ## Returns an xtable
  ## Assumes multi-line-cell macro (\mlc) is defined in the latex code
  ## mlc() and bold() are located in utilities.r
  ##
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## weight.factor - divide the weights by this factor
  ## xcaption - Caption to use
  ## xlabel - Latex label to use
  ## font.size - Size of the font for the table
  ## space.size - Size of the spaces for the table
  ## placement - Latex code for placement of table

  catches <- catches[,c("Year",
                        "atSea_US_MS",
                        "atSea_US_CP",
                        "US_shore",
                        "USresearch",
                        "Ustotal",
                        "CAN_JV",
                        "CAN_Shoreside",
                        "CAN_FreezeTrawl",
                        "CANtotal",
                        "TOTAL")]
  colnames(catches) <-
    c(bold("Year"),
      mlc(c("US", "Mother-", "ship")),
      mlc(c("US", "Catcher", "Processor")),
      mlc(c("US", "Shore-", "based")),
      mlc(c("US", "Research")),
      mlc(c("US", "Total")),
      mlc(c("CAN", "Joint", "Venture")),
      mlc(c("CAN", "Shore-", "side")),
      mlc(c("CAN", "Freezer", "Trawler")),
      mlc(c("CAN", "Total")),
      bold("Total"))
  ## Filter for correct years to show and make thousand-seperated numbers (year
  ##  assumed to be column 1)
  catches <- catches[catches[,1] >= start.yr & catches[,1] <= end.yr,]
  ## -1 below means leave the years alone and don't comma-seperate them
  catches[,-1] <-f(catches[-1])

  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{",font.size,"}{",space.size,"}\\selectfont")
  return(print(xtable(catches,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(catches))),
               caption.placement = "top",
               include.rownames = FALSE,
               table.placement = placement,
               sanitize.text.function = function(x){x},
               size = size.string))
}

make.catches.plot <- function(catches,
                              leg.y.loc = 430,
                              leg.cex = 1
                              ){
  ## Plot the catches in a stacked-barplot with legend
  years <- catches$Year
  catches <- catches[,c("CAN_forgn",
                        "CAN_JV",
                        "CAN_Shoreside",
                        "CAN_FreezeTrawl",
                        "US_foreign",
                        "US_JV",
                        "atSea_US_MS",
                        "atSea_US_CP",
                        "US_shore")]
  cols <- c(rgb(0, 0.8, 0),
            rgb(0, 0.6, 0),
            rgb(0.8, 0, 0),
            rgb(0.4, 0, 0),
            rgb(0, 0.2, 0),
            rgb(0, 0.4, 0),
            rgb(0, 0, 0.7),
            rgb(0, 0, 0.4),
            rgb(0, 0, 1))
  legOrder <- c(6, 5, 2, 1, 4, 3, NA, NA, 9, 8, 7)
  oldpar <- par()
  par(las = 1,
      mar = c(4, 4, 6, 2) + 0.1,
      cex.axis = 0.9)
  tmp <- barplot(t(as.matrix(catches)) / 1000,
                 beside = FALSE,
                 names = catches[,1],
                 col = cols,
                 xlab = "Year",
                 ylab = "",
                 cex.lab = 1,
                 xaxt = "n",
                 mgp = c(2.2, 1, 0))
  axis(1,
       at = tmp,
       labels = years,
       line = -0.12)
  grid(NA,
       NULL,
       lty = 1,
       lwd = 1)
  mtext("Catch (thousand t)",
        side = 2,
        line = 2.8,
        las = 0,
        cex = 1.3)
  barplot(t(as.matrix(catches)) / 1000,
          beside = FALSE,
          names = catches[,1],
          col = cols,
          xlab = "Year",
          ylab = "",
          cex.lab = 1,
          xaxt = "n",
          add = TRUE,
          mgp = c(2.2, 1, 0))
  legend(x = 0,
         y = leg.y.loc,
         c("Canadian Foreign",
           "Canadian Joint-Venture",
           "Canadian Shoreside",
           "Canadian Freezer Trawl",
           "U.S. Foreign",
           "U.S. Joint-Venture",
           "U.S. MS",
           "U.S. CP",
           "U.S. Shore-based")[legOrder],
         bg = "white",
         horiz = FALSE,
         xpd = NA,
         cex = leg.cex,
         ncol = 3,
         fill = cols[legOrder],
         border = cols[legOrder],
         bty = "n")
  par <- oldpar
}

make.landings.tac.table <- function(landings.vs.tac,
                                    start.yr,
                                    end.yr,
                                    xcaption = "default",
                                    xlabel   = "default",
                                    font.size = 9,
                                    space.size = 10,
                                    placement = "H",
                                    tabular.env = "tabular"
                                    ){
  ## Returns an xtable in the proper format for the executive summary landings
  ##  vs. TAC for management performance section
  ##
  ## start.yr - the first year to show in the table
  ## end.yr - the last year to show in the table
  ## xcaption - Caption to use
  ## xlabel - Latex label to use
  ## font.size - Size of the font for the table
  ## space.size - Size of the spaces for the table
  ## placement - Latex code for placement of table
  ## tabular.env - Type of table, e.g. "tabular" or "tabularx"

  tab <- landings.vs.tac

  ## Filter for correct years to show and make thousand-seperated numbers (year assumed to be column 1)
  tab <- tab[tab$Year >= start.yr & tab$Year <= end.yr,]
  tab[,-c(1, 8, 9, 10)] <- f(tab[,-c(1, 8, 9, 10)])

  ## Round the proportions to one decimal place
  tab[,8] <- paste0(f(tab[,8], 1),"\\%")
  tab[,9] <- paste0(f(tab[,9], 1),"\\%")
  tab[,10] <- paste0(f(tab[,10], 1),"\\%")
  ## Switch TACCAN and TACUSA columns for consistency
  tmp <- tab[,6]
  tab[,6] <- tab[,7]
  tab[,7] <- tmp
  colnames(tab) <- c("\\textbf{Year}",
                     "\\mlc{\\textbf{US}\\\\\\textbf{landings (t)}}",
                     "\\mlc{\\textbf{Canadian}\\\\\\textbf{landings (t)}}",
                     "\\mlc{\\textbf{Total}\\\\\\textbf{landings (t)}}",
                     "\\mlc{\\textbf{Coast-wide}\\\\\\textbf{(US+Canada)}\\\\\\textbf{catch}\\\\\\textbf{target (t)}}",
                     "\\mlc{\\textbf{US}\\\\\\textbf{catch}\\\\\\textbf{target (t)}}",
                     "\\mlc{\\textbf{Canada}\\\\\\textbf{catch}\\\\\\textbf{target (t)}}",
                     "\\mlc{\\textbf{US}\\\\\\textbf{proportion}\\\\\\textbf{of catch}\\\\\\textbf{target}\\\\\\textbf{removed}}",
                     "\\mlc{\\textbf{Canada}\\\\\\textbf{proportion}\\\\\\textbf{of catch}\\\\\\textbf{target}\\\\\\textbf{removed}}",
                     "\\mlc{\\textbf{Total}\\\\\\textbf{proportion}\\\\\\textbf{of catch}\\\\\\textbf{target}\\\\\\textbf{removed}}")
  ## Make the size string for font and space size
  size.string <- paste0("\\fontsize{", font.size, "}{", space.size, "}\\selectfont")
  return(print(xtable(tab,
                      caption = xcaption,
                      label = xlabel,
                      align = get.align(ncol(tab),
                                        first.left = FALSE, just = "c"),
                      digits = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1)),
               caption.placement = "top",
               include.rownames = FALSE,
               table.placement = placement,
               tabular.environment = tabular.env,
               sanitize.text.function = function(x){x},
               size = size.string))
}
