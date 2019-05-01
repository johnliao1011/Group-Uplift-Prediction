
Custom_Plot <- function(data, yLim, LineType, LineColor, Pch, Lty, TransAlpha, LineWidth,main =FALSE, Title){
  
  # summary of the group each bootstrap generate
  summary <- data%>% apply( 2, function(x)(length(x)-1-length(which(is.na(x)))))%>%
    as.data.frame()%>%
    rename(group = ".")%>%
    mutate(index = rownames(.))
  
  # generate a blank plot
  if(main ==TRUE){
    plot(x = seq(0, 100, by = 10), y = seq(0, 1, by = 0.1), type="l", lwd=0,ylim = yLim,
         ylab="Cumulative Incremental Gains", xlab="Proportion of population targeted (%)", main =Title)
    
    lines(x=seq(0, 100, by = 10), y = seq(0, 1, by = 1/10), lwd=4, lty="dashed")
  }
  
  group <- summary$group
  index <- summary$index
  
  for (i in c(1:ncol(data))) {
    upliftValue<-data[, index[i]]%>%na.omit()
    rgbDetect <- col2rgb(LineColor)
    lines(x = seq(0, 100, by = 100/(length(upliftValue)-1)), y = upliftValue, type = LineType, pch = Pch,
          col = rgb(rgbDetect[1], rgbDetect[2], rgbDetect[3], maxColorValue = 255, alpha = TransAlpha), lwd=LineWidth,  lty=Lty) 
  }
}





