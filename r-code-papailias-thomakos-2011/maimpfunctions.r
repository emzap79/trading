# Example code for implementing the long-only version of the 
# improved MA approach in the paper:
#
# Papailias, Fotis and Thomakos, Dimitrios D.,
# "An Improved Moving Average Technical Trading Rule,
# (September 11, 2011). Available at SSRN: http://ssrn.com/abstract=1926376 
#
# Originally written *for web use* & (C) by Dimitrios Thomakos on Dec. 05, 2011
# dimitrios.thomakos@gmail.com, thomakos@quantf.com
#
# Changes by Dimitrios Thomakos on Dec. 06, 2011
# Additions and changes by Fotis Papailias on Dec. 07, 2011
# papailias@quantf.com
#
# Kent Russell & Bob Fulks provided useful suggestions that improved 
# the current implementation
# 
# Last update: December 11, 2011 - THIS UPDATE SUPERSEDES ALL PREVIOUS UPDATES
#
# All material is provided for use as is, with no guarrantees, either expressed or implied.
# Copyright (C) under the authors' names Papailias, Fotis and Thomakos, Dimitrios for both
#
# The software and methodology described hereIN and in both papers that are available at
# http://ssrn.com/abstract=1926376 and http://ssrn.com/abstract=1958906
# and can be licensed for commercial purposes, for either private or corporate use.
#
#-------------------------------------------------------------------------#
# Please inform us if you are using this material for research or trading #
#-------------------------------------------------------------------------#
# 
# PLEASE MAINTAIN THIS HEADER IN ALL COPIES OF THIS FILE THAT YOU USE

# Call the zoo library
library(zoo)

# Function to compute percentage returns
pchange <- function(x) { exp(diff(log(x)))-1 }

# The weighted mean function
wmean <- function(x) { n <- length(x); w <- seq(n); return(sum(w*x)/sum(w)) }

# Explicit code for improved MA signals
ma.trading.improved <- function(x,k1=20,k2=50,mtype="simple",exitt=0.0)
{
	# Keep attributes for later use, need next day as well, 
	# although this is done for convenience, it does not 
	# have to be the next trading day and its not used for
	# anything thereafter...
	dates <- as.Date(c(index(x),index(x)[length(x)]+1))

	# Pass the data as a matrix; this strips all other attributes
	x <- as.matrix(x)
	
	# Select correctly the closing price if OHLC is loaded
	if(NCOL(x) > 1) x <- x[,4]
	
	# Cross-checks
  # Fotis ok, Dec. 07, 2011
	if ((k1 <= 0) | (k2 <= 0) | ((k1 > k2) & (k2 > 0))) { stop("k1,k2 must both be positive and k1 < k2") }

	# Full sample
	Nobs <- NROW(x)
	
	# Daily return
	ret <- pchange(x)
	
	# Initialize moving average values
	ma.values <- matrix(NA,nrow=Nobs+1,ncol=2)
	
	# Initialize signal storage
	signals.ma1 <- matrix(0,nrow=Nobs+1,ncol=1)
	signals.ma2 <- matrix(0,nrow=Nobs+1,ncol=1)
	signals.mac <- matrix(0,nrow=Nobs+1,ncol=1)
	signals.im1 <- matrix(0,nrow=Nobs+1,ncol=1)
	signals.im2 <- matrix(0,nrow=Nobs+1,ncol=1)
	signals.imc <- matrix(0,nrow=Nobs+1,ncol=1)
	signals.iv1 <- matrix(0,nrow=Nobs+1,ncol=1)
	signals.iv2 <- matrix(0,nrow=Nobs+1,ncol=1)
	signals.ivc <- matrix(0,nrow=Nobs+1,ncol=1)
	
	# Initialize markers of entries for improved MA
	mark1 <- -1
	mark2 <- -1
	markc <- -1
	pentry1 <- -1
	pentry2 <- -1
	pentryc <- -1
	
	# Do things via a standard loop to maintain consistency 
	for (i in seq(k2,Nobs,1)) # fotis change/edit
	{
		# Compute the moving averages # fotis change/edit
		if (mtype == "simple") 
		{
			ma1 <- mean(x[seq(i-k1+1,i,1)])
			ma2 <- mean(x[seq(i-k2+1,i,1)]) 
    
      # Save the MA values # fotis change/edit
      ma.values[i,] <- c(ma1,ma2)  

		}
		if (mtype == "weighted") 
		{ 
			ma1 <- wmean(x[seq(i-k1+1,i,1)]) 
			ma2 <- wmean(x[seq(i-k2+1,i,1)]) 
      
      # Save the MA values # fotis change/edit
      ma.values[i,] <- c(ma1,ma2)
		}
    
    # fotis add, Dec. 07, 2011
    if (mtype=="exponential")
    { 
            # Get the starting signal to be used in the EMA calc., fotis
            if(i==k2)
            {
              ma1 <- mean(x[seq(i-k1+1,i,1)])
              ma2 <- mean(x[seq(i-k2+1,i,1)])
              
              # Save the MA values # fotis change/edit
  	          ma.values[i,] <- c(ma1,ma2)
            }
            else
            {
              ma1 <- ((x[i]-ma.values[i-1,1])*(2/(k1+1)))+ma.values[i-1,1]
              ma2 <- ((x[i]-ma.values[i-1,2])*(2/(k2+1)))+ma.values[i-1,2]
              
              # Save the MA values 
             ma.values[i,] <- c(ma1,ma2)
            }
    }

		# Get the last observed price
		plast <- x[i]

		# Version AB of improved MA: once you exit you wait for next signal;
		#                            exiting is controlled by the exit threshold
		#
		# Stay
    # Fotis ok, Dec. 07, 2011
		if ((mark1 > 0) & (pentry1 > 0) & (plast >= pentry1)) { signals.im1[i+1] <- 1 }
		if ((mark2 > 0) & (pentry2 > 0) & (plast >= pentry2)) { signals.im2[i+1] <- 1 }
		if ((markc > 0) & (pentryc > 0) & (plast >= pentryc)) { signals.imc[i+1] <- 1 }
		# Exit
    # Fotis check, Dec. 06, 2011
		if ((pentry1 > 0) & (plast < pentry1*(1-exitt))) { signals.im1[i+1] <- 0; mark1 <- 0 }
		if ((pentry2 > 0) & (plast < pentry2*(1-exitt))) { signals.im2[i+1] <- 0; mark2 <- 0 }
		if ((pentryc > 0) & (plast < pentryc*(1-exitt))) { signals.imc[i+1] <- 0; markc <- 0 }
		
		# Version BB of improved MA: once you exit you can re-enter when above the current
		#                            entry price; exiting is controlled by the exit threshold
		#
    # Fotis ok, Dec. 07, 2011
		# Stay
		if ((pentry1 > 0) & (plast >= pentry1) & (plast > ma1)) { signals.iv1[i+1] <- 1 }
		if ((pentry2 > 0) & (plast >= pentry2) & (plast > ma2)) { signals.iv2[i+1] <- 1 }
		if ((pentryc > 0) & (plast >= pentryc) & (ma1   > ma2)) { signals.ivc[i+1] <- 1 }
		# Exit
    # Fotis ok, Dec. 07, 2011
		if ((pentry1 > 0) & (plast < pentry1*(1-exitt))) { signals.iv1[i+1] <- 0 }
		if ((pentry2 > 0) & (plast < pentry2*(1-exitt))) { signals.iv2[i+1] <- 0 }
		if ((pentryc > 0) & (plast < pentryc*(1-exitt))) { signals.ivc[i+1] <- 0 }	
		
		# Here are the standard MA signals
		if (plast > ma1) { signals.ma1[i+1] <- 1 }
		if (plast > ma2) { signals.ma2[i+1] <- 1 }
		if (ma1 > ma2)   { signals.mac[i+1] <- 1 }
		
		# Here are the initializations of the improved MAs, both versions
    # Fotis ok, Dec. 07, 2011
		if (signals.ma1[i+1] > signals.ma1[i]) { pentry1 <- x[i+1]; signals.im1[i+1] <- 1; signals.iv1[i+1] <- 1; mark1 <- 1 }
		if (signals.ma2[i+1] > signals.ma2[i]) { pentry2 <- x[i+1]; signals.im2[i+1] <- 1; signals.iv2[i+1] <- 1; mark2 <- 1 }
		if (signals.mac[i+1] > signals.mac[i]) { pentryc <- x[i+1]; signals.imc[i+1] <- 1; signals.ivc[i+1] <- 1; markc <- 1 }		
	    
    }
	
	# Bind together and correctly aling things and returns
	bind1 <- data.frame(c(x,NA),ma.values,signals.ma1,signals.ma2,signals.mac,
						signals.im1,signals.im2,signals.imc,signals.iv1,signals.iv2,signals.ivc,c(ret,NA,NA))
	bind2 <- data.frame(cbind(signals.ma1,signals.ma2,signals.mac,signals.im1,signals.im2,signals.imc,signals.iv1,signals.iv2,signals.ivc)*
						matrix(c(ret,NA,NA),nrow=Nobs+1,ncol=9),c(ret,NA,NA))
	colnames(bind1) <- c("Price","MA1","MA2","MA1-S","MA2-S","MAC-S","IMA1-S","IMA2-S","IMC-S","IV1-S","IV2-S","IVC-S","Return")
	colnames(bind2) <- c("Ret-MA1","Ret-MA2","Ret-MAC","Ret-IMA1","Ret-IMA2","Ret-IMC","Ret-IV1","Ret-IV2","Ret-IVC","Ret-BnH")
	  
  bind3 <- apply(bind2+1,2,cumprod)
  # Fotis ok, Dec. 07, 2011
  
	# Convert to zoo objects
	bind1 <- zoo(bind1,order.by=dates)
	bind2 <- zoo(bind2,order.by=dates)
	bind3 <- zoo(bind3,order.by=dates)
	
	# Done!
	return(list(values=bind1,returns=bind2,cumulative=bind3))
}

# Function to compute the drawdowns of a strategy
maxDDD <- function(cumret,doplot=F,...)
{
	N <- NROW(cumret)
	highmark <- matrix(0,N,1)
	drawdown <- matrix(0,N,1)
	duration <- matrix(0,N,1)
	
	for (i in seq(2,N,1))
	{
		highmark[i] <- max(c(highmark[i-1],cumret[i]))
		#drawdown[i] <- (1+highmark[i])/(1+cumret[i]) - 1 # Fotis check, Dec. 06, 2011
		drawdown[i] <- 1-(1+cumret[i])/(1+highmark[i]) 
		if (drawdown[i] != 0) { duration[i] <- duration[i-1]+1 }
	}
	
	to   <- which.max(drawdown)
    from <- double(NROW(to))
    for (i in 1:NROW(to)) { from[i] <- max(which(drawdown[1:to[i]] == 0)) }
	
	return(list(drawdown=drawdown,duration=duration,
	maxDD=max(drawdown),maxD=max(duration),maxFrom=from,maxTo=to))
}

# Function to compute statistics on matrices
matrix.stats <- function(out)
{
	# Basic statistics
	mu <- apply(out,2,mean)*252
	sg <- apply(out,2,sd)*sqrt(252)
	sr <- mu/sg
	maxr <- apply(out,2,max)
	minr <- apply(out,2,min)
	cret <- apply(1+out,2,cumprod)-1
	cumr <- cret[NROW(cret),]
	maxDD <- apply(cret,2,function(x) maxDDD(x)$maxDD)
	maxDu <- apply(cret,2,function(x) maxDDD(x)$maxD)

	# P/L ratio, winrate and profit factor
	pwp <- function(ret)
	{
		# P/L ratio
		pnl <- abs(mean(subset(ret,ret > 0))/mean(subset(ret,ret < 0)))
		# Winning rate
		winrate <- (sum(ret > 0,na.rm=T)/length(na.omit(ret)))
		# Expectation index
		expind <- (1+abs(pnl))*winrate - 1

		# Return
		return(c(pnl,winrate,expind))
	}

	pnl <- apply(out,2,function(x) pwp(x)[1])
	wnr <- apply(out,2,function(x) pwp(x)[2])
	exi <- apply(out,2,function(x) pwp(x)[3])

	stats <- rbind(mu,sg,sr,maxr,minr,cumr,maxDD,maxDu,pnl,wnr,exi)
	rownames(stats) <- c("Average","Volatility","Sharpe","Max","Min","Cumulative",
				"Drawdown","Duration","Profit/Loss","Win Rate","Expectation")
	colnames(stats) <- colnames(out)
	
	# Done!
	return(list(statistics=stats,cum.return=cret))
	#return(stats)
}
