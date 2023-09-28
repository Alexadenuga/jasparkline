#' Just A Spark-line plot function
#'
#' @param dat 
#' @param trans 
#' @param colo 
#' @param label_x 
#' @param label_y 
#' @param the_title 
#'
#' @return a spark-line plot with transparent colours
#' @export
#'
#' @examples dat <- data.frame(x = 1:100, y = rnorm(100), z = rnorm(100) + 2)
#'           the_spark_plot(dat=dat, trans = 0.3, colo = "red", label_x = "Data point", label_y = "Value", the_title = "Sparkline Plot")
#' 
the_spark_plot <- function(dat, trans, colo, label_x, label_y, the_title ){
  num_columns <- ncol(dat)
  plot(1, type = "n", xlim = c(1, nrow(dat)), ylim = c(0, max(dat)), xlab = label_x, ylab = label_y, xaxt = "n")
  
  # Plot the first column (X1) as a normal blue line
  lines(1:nrow(dat), dat[, 1], col = colo)
  
  # Plot the remaining columns as transparent blue lines
  for (i in 2:ncol(dat)) {
    lines(1:nrow(dat), dat[, i], col = rgb(1, 0, 0, alpha = trans))
  }
  
  # Add a title (optional)
  title("the_title")
  
  # Add axis labels
  axis(1, at = 1:nrow(dat), labels = 1:nrow(dat))
  axis(2, at = seq(0, 1, by = 0.25))
  
  # Add a legend (optional)
  legend("topright", legend = colnames(dat), col = c(colo, rep(rgb(1, 0, 0, alpha = 0.5), ncol(dat) - 1)), lty = 1)
}
