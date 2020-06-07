ggpretay <- function (plot, title = "Enter Title", subtitle = " Enter Subtitle", 
          xaxis = " enter x", yaxis = " enter y") 
{
  plot + ggnewlabs(title = title, subtitle = subtitle) + ggplot2::xlab(paste(xaxis)) + 
    ggplot2::ylab(paste(yaxis)) + ggpretty()
}


ggpretty <- function (font = "Muli") {
  font <- font
  ggplot2::theme(plot.title = ggplot2::element_text(family = font, 
                                                    size = 16, face = "bold", color = "#1f6cb4", margin = ggplot2::margin(0, 
                                                                                                                          0, 0, 0), hjust = 0), plot.title.position = "plot", 
                 plot.subtitle = ggplot2::element_text(family = font, 
                                                       size = 14, margin = ggplot2::margin(9, 0, 0, 0)), 
                 plot.caption = ggplot2::element_blank(), legend.position = "top", 
                 legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = font, size = 12, 
                                                     color = "#222222"), axis.title = ggplot2::element_text(size = 12), 
                 axis.text = ggplot2::element_text(family = font, size = 12, 
                                                   color = "#222222"), axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, 
                                                                                                                                    b = 10), size = 12), axis.ticks = ggplot2::element_blank(), 
                 axis.line = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(size = 14, hjust = 0))
}
