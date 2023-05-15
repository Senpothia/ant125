#' Plots a function
#'
#' @param func the function to plot given in string form
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
#' f<-"X^2 + 2*X +3"
#' p<-plotFunction(f, 0, 20, "title", "axis x name", "axis y name")
#' print(p)
#'
plotFunction<-function(func, xm, xM, main, xlab, ylab){

  X <- seq(xm, xM, by=1)
  fonction<-parse(text=func)
  Y<-eval(fonction)

  val <- data.frame(x = X, y = Y)
  p<-(
    ggplot2::ggplot(data = val, ggplot2::aes(x = x, y = y))+#, color="red")) +
      ggplot2::geom_line(color="red") +
      ggplot2::scale_x_continuous(name = xlab) +
      ggplot2::scale_y_continuous(name = ylab) +
      ggplot2::ggtitle(main)
  )

}
