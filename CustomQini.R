######################################################################
# Qini 
######################################################################
CustomQini <- function(x, direction = 1, plotit = TRUE, ...) {
  
  if (!inherits(x, "performance"))
    stop("uplift: x is not of class performance")
  
  ### check valid arguments
  if (!direction %in% c(1, 2))
    stop("uplift: direction must be either 1 or 2")
  
  perf <- x
  groups <- nrow(perf)
  
  
  # Calculating the incremental gains. 
  # - First the cumulitative sum of the treated and the control groups are calculated with respect to the total population in each group
  #   at the specified decile.
  # - Afterwards we calculate the percentage of the total amount of people (both treatment and control) are present in each decile.
  r.cumul.y1_ct1 <- cumsum(perf[,"n.y1_ct1"]) / cumsum(perf[,"n.ct1"])
  r.cumul.y1_ct0 <- cumsum(perf[,"n.y1_ct0"]) / cumsum(perf[,"n.ct0"])
  deciles <- seq(1 / groups, 1, 1 / groups)
  
  if (direction == 1) {
  
    ### Model Incremental gains 
    inc.gains <- c(0.0, (r.cumul.y1_ct1 - r.cumul.y1_ct0) * deciles)
  
    ### Overall incremental gains
    overall.inc.gains <- sum(perf[, "n.y1_ct1"]) / sum(perf[, "n.ct1"]) - sum(perf[, "n.y1_ct0"]) / sum(perf[, "n.ct0"])
  
  } else {
    
    ### Model Incremental gains 
    inc.gains <- c(0, (r.cumul.y1_ct0 - r.cumul.y1_ct1) * deciles)
      
    ### Overall incremental gains
    overall.inc.gains <- sum(perf[, "n.y1_ct0"]) / sum(perf[, "n.ct0"]) - sum(perf[, "n.y1_ct1"]) / sum(perf[, "n.ct1"]) 
    
 }
  
  ### Random incremental gains
  random.inc.gains <- c(0, cumsum(rep(overall.inc.gains / groups, groups)))
  
  ### Compute area under the model incremental gains (uplift) curve 
  x <- c(0.0, seq(1 / groups, 1, 1 / groups))
  y <- inc.gains
  
  auuc <- 0
  auuc.rand <- 0
  
  for (i in 2:length(x)) {
    auuc <- auuc + 0.5 * (x[i] - x[i-1]) * (y[i] + y[i-1])
  }

  ### Compute area under the random incremental gains curve
  y.rand <- random.inc.gains

  for (i in 2:length(x)) {
    auuc.rand <- auuc.rand + 0.5 * (x[i] - x[i-1]) * (y.rand[i] + y.rand[i-1])
  }
  
  ### Compute the difference between the areas (Qini coefficient)
  Qini <- auuc - auuc.rand
  miny <- 100 * min(c(random.inc.gains, inc.gains))
  maxy <- 100 * max(c(random.inc.gains, inc.gains))
  
  plot(inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type ="b",
       col = "blue", lty = 2, xlab = "Proportion of population targeted (%)", 
       ylab = "Cumulative incremental gains (pc pt)", ylim = c(miny, maxy))
  lines(random.inc.gains * 100 ~ c(0, seq(100 / groups, 100, 100 / groups)), type = "l", col = "red", lty = 1)
  legend("topright", c("Model", "Random"), 
         col=c("blue", "red"), lty=c(2,1))
    
  res <- list(Qini = Qini,
              inc.gains = inc.gains,
              random.inc.gains = random.inc.gains)
  
  return(res)
  
}





### dig inside the code of estimate the response rate in each segment
data.frame("experiment"=i, "group"=perf[,1], "n.ct1"=perf[,2],"n.ct0"=perf[,3],
                        "n.y1_ct1"=perf[,4],"n.y1_ct0"= perf[,5], "r.y1_ct1"=perf[,6], "r.y1_ct0"=perf[,7], 
                        "uplift"=perf[,8])%>%
  mutate(r.cumul.y1_ct1 = cumsum(perf[,"n.y1_ct1"]) / cumsum(perf[,"n.ct1"]))%>%
  mutate(r.cumul.y1_ct0 = cumsum(perf[,"n.y1_ct0"]) / cumsum(perf[,"n.ct0"]))



