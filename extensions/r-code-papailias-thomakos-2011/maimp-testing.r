# Example code for implementing the long-only version of the 
# improved MA approach in the paper:
#
# Papailias, Fotis and Thomakos, Dimitrios D.,
# "An Improved Moving Average Technical Trading Rule,
# (September 11, 2011). Available at SSRN: http://ssrn.com/abstract=1926376 
#
# Written for *web upload* by Dimitrios Thomakos on Dec. 5, 2011
# dimitrios.thomakos@gmail.com, thomakos@quantf.com
#
# Changes by Dimitrios Thomakos on Dec. 6, 2011
#
# Additions and changes by Fotis Papailias on Dec. 07, 2011
# papailias@quantf.com
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

# Clear things to start from scratch
rm(list=ls(all=TRUE))

# Source the function file
source("maimpfunctions.r")

# Load the example data of SP500 from 1950's
#xxx <- read.csv("SP500.csv",header=T)
#dates  <- as.Date(xxx[,1],format="%d/%m/%Y")
#prices <- zoo(xxx[,2],order.by=dates)

#x<-prices
# or uncomment the lines below to load something online
 library(quantmod)
 ticker <- "BND"
 start.date <- "2005-01-01"
 x <- getSymbols(ticker,auto.assign=F,from = start.date, to = "2011-11-30")
 prices <- Ad(x)

# Select a type of MA, "simple" or "weighted" or "exponential" (Fotis add, Dec. 07, 2011)
ma.type <- "exponential"

# Select the two parameters
k1 <- 20
k2 <- 50

# Select a % for the exit threshold, default is 0, enter as 0.03 for 3%
xt.thresh <- 0

# Call the function to do things
out <- ma.trading.improved(prices,k1,k2,mtype=ma.type,exitt=xt.thresh)

# Plot the cumulative return of everything
windows()
plot(out$cumulative,main="Cumulative return of MA and IMA strategies and buy & hold")

# Print some statistics

print(matrix.stats(na.omit(out$returns))$statistics,digits=3)

windows()
# Fotis
# Graph for comparison and intution
# uncomment lines 73 and 111 if you want to print the graph in .pdf
cum.ret.all <- as.matrix(out$cumulative)
cum.ret.all <- na.omit(cum.ret.all)
bh <- cum.ret.all[,10]
dates <- rownames(cum.ret.all)

#start pdf
#pdf(file="IMA.pdf", height=8.27, width=11.69, paper="a4")

par(mfrow=c(3,1))
titles.all <- c("Fast", "Slow", "Cross-Over")

ig <- 1
while(ig<=3)
{
x1 <- cum.ret.all[,ig]
x2 <- cum.ret.all[,ig+3]
x3 <- cum.ret.all[,ig+6]

mmin <- min(x1, x2, x3, bh)
mmax <- max(x1, x2, x3, bh)

plot(x1, type="l", col="black", lwd=1, ylim=c(mmin,mmax), 
 xlab="", ylab="Cum. Ret.", axes=FALSE)
lines(x2, type="l", lty=1, lwd=1, col="green")
lines(x3, type="l", lty=1, lwd=1, col="red")
lines(bh, type="l", lty=1, lwd=1, col="blue")

indic <- c(round(NROW(x1)*0.15), round(NROW(x1)*0.30),
             round(NROW(x1)*0.45), round(NROW(x1)*0.60), round(NROW(x1)*0.75),
              round(NROW(x1)*0.90), NROW(x1))
xpos <- list(1, indic[1], indic[2], indic[3], indic[4],indic[5],indic[6], indic[7])
labels1 <- c(dates[1], dates[indic[1]], dates[indic[2]], dates[indic[3]],
             dates[indic[4]], dates[indic[5]], dates[indic[6]], dates[indic[7]])

axis(side=1, at = xpos, labels = labels1, tick = TRUE, line = 0,
     lwd = 1, lwd.ticks = 1, col = NULL, col.ticks = NULL, hadj = NA, padj = NA)

axis(side=2)
box()

title(main=titles.all[ig], sub=ma.type, xlab=ticker, col.main="black",font.main=2)
ig<-ig+1
}

#dev.off()
#stop pdf

# Fotis
# uncomment the following lines to print the signals output and
# understand the 100% of the method's conceptual and how the strategies change.
#pr.signs <- as.matrix(out$values)
#print(pr.signs)