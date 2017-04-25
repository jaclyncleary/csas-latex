make.hcr.plot <- function(){
  ## Makes the harvest control rule plot, which is an illustrative plot only.

  plot(c(0, 30),
       c(0, 0.4),
       type = "n",
       axes = FALSE,
       xaxs = "i",
       yaxs = "i",
       xlab = "Stock Status",
       ylab = "Removal Rate")
  ## Plot the vertical bars (LRP and USR)
  segments(4, 0, 4, 0.4, col="black", lty=3, lwd=1)
  segments(9, 0, 9, 0.4, col="black", lty=3, lwd=1)
  ## Plot removal rate lines
  segments(0, 0, 4, 0,   col="blue", lty=1, lwd=4)
  segments(4, 0, 9, 0.3, col="blue", lty=1, lwd=4)
  segments(9, 0.3, 30, 0.3, col="blue", lty=1, lwd=4)
  box()
  text(4, 0.35, "LRP", col="red")
  text(9, 0.35, "USR", col="red")
  text(20,0.33, "LRR", col="red")
}
