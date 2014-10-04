# vim:fdm=marker:isk-=(,.,_

#######################################################################
#                         Configuration                               #
#######################################################################
library("MASS")
library("chron")
library("pastecs")
library("lubridate")
library("psych")    # describe()
library("ggplot2")
library("stargazer")
library("rgl")
library("plot3D")

# Financial Tools
library(latticeExtra)
library(reshape2)
suppressPackageStartupMessages(
  library(googleVis)
)
library(quantmod)
library(PerformanceAnalytics)
library(xtsExtra)
library(devtools)
library(TTR)

# set workingdir
setwd("/home/zapata/Dokumente/Finance")

###############################################################################
# Load Systematic Investor Toolbox (SIT)
# http://systematicinvestor.wordpress.com/systematic-investor-toolbox/
###############################################################################
con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
    source(con)
close(con)
 
    #*****************************************************************
    # Load historical data
    #******************************************************************
    load.packages('quantmod')  
 
    # data from http://thebonnotgang.com/tbg/historical-data/
    spath = 'c:/Desktop/'
    # http://stackoverflow.com/questions/14440661/dec-argument-in-data-tablefread
        Sys.localeconv()["decimal_point"]
        Sys.setlocale("LC_NUMERIC", "French_France.1252")
     
    data <- new.env()
    data$SPY = read.xts(paste0(spath,'SPY_1m.csv'),
        sep = ';', date.column = 3, format='%Y-%m-%d %H:%M:%S', index.class = c("POSIXlt", "POSIXt"))
 
    data$GLD = read.xts(paste0(spath,'GLD_1m.csv'),
        sep = ';', date.column = 3, format='%Y-%m-%d %H:%M:%S', index.class = c("POSIXlt", "POSIXt"))
 
    #*****************************************************************
    # Create plot for Nov 1, 2012 and 2013
    #******************************************************************
    layout(c(1,1,2))       
    plota(data$SPY['2012:11:01'], type='candle', main='SPY on Nov 1st, 2012', plotX = F)
    plota(plota.scale.volume(data$SPY['2012:11:01']), type = 'volume') 
 
    layout(c(1,1,2))       
    plota(data$SPY['2013:11:01'], type='candle', main='SPY on Nov 1st, 2013', plotX = F)
    plota(plota.scale.volume(data$SPY['2013:11:01']), type = 'volume')
