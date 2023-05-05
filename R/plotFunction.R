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
