#' Plot a function
#'
#' @param func the function to plot
#' @param xm minimum value of the interval of representatiion
#' @param xM maximumvalue of the interval of representatiion
#' @param main titre of the plot
#' @param xlab label of axis x
#' @param ylab label of axis y
#'
#' @return
#' @export
#'
#' @examples
#'
plotFunction<-function(func, xm, xM, main, xlab, ylab){

  fonction<-parse(text=func)
  fonction<-eval(fonction)
  x <- seq(xm, xM, by=1)
  val <- data.frame(x = x, y = fonction(x))
  p<-(
    ggplot2::ggplot(data = val, ggplot2::aes(x = x, y = y))+#, color="red")) +
      ggplot2::geom_line(color="red") +
      ggplot2::scale_x_continuous(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab) +
      ggplot2::ggtitle(main)
  )

}
