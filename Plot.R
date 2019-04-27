
Custom_Plot <- function(data, yLim, LineType, LineColor, LineWidth,main =FALSE){
  
  # summary of the group each bootstrap generate
  summary <- data%>% apply( 2, function(x)(length(x)-1-length(which(is.na(x)))))%>%
    as.data.frame()%>%
    rename(group = ".")%>%
    mutate(index = rownames(.))
  
  # generate a blank plot
  if(main ==TRUE){
    plot(x = seq(0, 100, by = 10), y = seq(0, 1, by = 0.1), type="l", lwd=0,ylim = yLim,
         ylab="Cumulative Incremental Gains", xlab="Proportion of population targeted (%)")
    
    lines(x=seq(0, 100, by = 10), y = seq(0, 1, by = 1/10), lwd=4, lty="dashed")
  }
  
  group <- summary$group
  index <- summary$index
  
  for (i in c(1:ncol(data))) {
    upliftValue<-data[, index[i]]%>%na.omit()
    lines(x = seq(0, 100, by = 100/(length(upliftValue)-1)), y = upliftValue, type = LineType, pch = 19,
          col = LineColor, lwd=LineWidth) 
  }
}



Custom_Plot(data = bootstrap.1.1.1.1_tma.RF$gain.ratio, yLim = c(0,2), 
            LineType = "b", LineColor = "yellowgreen", LineWidth = 2, main = TRUE)

Custom_Plot(data = bootstrap.1.1.1.1_oma.RF$gain.ratio, yLim = c(0,2), 
            LineType = "b", LineColor = "orangered", LineWidth = 2)


