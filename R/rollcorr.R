#' rollcorr
#' 
#' Live Rolling Correlation Plot by Eran Raviv
#' See http://eranraviv.com/blog/live-rolling-correlation-plot/
#' 
#' @return null
#' @author Eran Raviv
#' @export
rollcorr <- function(){
  symetf <- c('XLY','XLP','XLE','XLF','XLV','XLI','XLB','XLK','XLU','SPY')
  end <- format(Sys.Date(),"%Y-%m-%d") 
  start <-format(Sys.Date() - 365,"%Y-%m-%d")
  dat0 <- getSymbols(symetf[1], src="google", from=start, to=end, auto.assign = F, warnings = FALSE,symbol.lookup = F)
  n <- NROW(dat0)
  l <- length(symetf)
  dat <- array(dim = c(n,NCOL(dat0),l)) ; ret = matrix(nrow = n, ncol = l) 
  for (i in 1:l){
    dat0 <- getSymbols(symetf[i], src="google", from=start, to=end, auto.assign = F,warnings = FALSE,symbol.lookup = F)
    dat[1:n,,i] <- dat0 
    ret[2:n,i] <- dat[2:n,4,i]/dat[1:(n-1),4,i] - 1
  }
  rolcor = NULL
  h = 10 # 10 bussiness days is two weeks
  for (i in 2:(n-h)){
    rolcor[i+h] <- mean(cor(ret[i:(i+h),])[lower.tri(cor(ret[i:(i+h),]))]) # just the rolling average correlation
  }
  par( mfrow = c(2,1), bg = "white", bty ="n", fg = gray(0.3) ,font.lab = 6, font.axis = 6, #xaxp =  c(x1, x2, n = 2)
       font.main = 6, col.axis = gray(0.3) , col.lab = gray(0.3) , pch = 21,  tck = -0.02, #tck is the length of the axis spikes 
       xaxs = "r")  # Graph parameters
  lwd1 <- 2.5
  plot(rolcor~index(dat0), ty = "l",lwd = lwd1, xlab = "Time",ylab = "Average Correlation", main = "Two Weeks Rolling Correlation")
  plot(dat[1:n,4,10]~index(dat0), ty = "l", lwd = lwd1, xlab = "Time", ylab = "SPY Price Level",main ="SPY Price Level")
  invisible();
}
