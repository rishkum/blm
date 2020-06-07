ggpretay <- function (plot, title = "Enter Title", subtitle = " Enter Subtitle", 
          xaxis = " enter x", yaxis = " enter y") 
{
  plot + ggnewlabs(title = title, subtitle = subtitle) + ggplot2::xlab(paste(xaxis)) + 
    ggplot2::ylab(paste(yaxis)) + ggpretty()
}
