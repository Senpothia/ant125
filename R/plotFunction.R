plotFunction<-function(func, xm, xM, main, xlab, ylab){
  
  fonction<-parse(text=func)
  fonction<-eval(fonction)
  x <- seq(xm, xM, by=1)
  val <- data.frame(x = x, y = fonction(x))
  p<-(
    ggplot(data = val, aes(x = x, y = y))+#, color="red")) + 
      geom_line(color="red") + 
      scale_x_continuous(name = xlab) +
      scale_y_continuous(name = ylab) +
      ggtitle(main)
  )
  
}