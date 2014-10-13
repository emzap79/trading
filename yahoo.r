# vim: fdm=marker
#!/usr/bin/env Rscript
readchart=F

symb <- "AAPL"
symb <- "CELG"
symb <- "BABA"
symb <- "DAI.DE"
symb <- "GOOG"
symb <- "POAHF"
symb <- "VOW3"
symb <- "TSLA"

# Packages                    {{{1
require(latticeExtra)
require(ggplot2)
require(reshape2)
suppressPackageStartupMessages(
  require(googleVis)
)
require(quantmod)
require(PerformanceAnalytics)
require(xtsExtra)
require(devtools)
require(TTR)
library(zoo)


#install.packages("RCurl")
#install.packages('ramnathv-rCharts-2c368c8.tar.gz', repos = NULL, type = 'source')
#require(rCharts)

# Retreive Data               {{{1
# Get Stock Data From Yahoo   {{{2
getprice <- function (x) {
    na.omit(getSymbols(x, src = "yahoo", auto.assign = FALSE))
}
udlyg <- getprice(symb)

## add legend
## http://r.789695.n4.nabble.com/main-title-in-plot-outer-TRUE-cut-off-td4204006.html
#title(outer=TRUE,adj=0,main = list("1 a)", cex=1.1,col="black", font=2), line = -1)

#addSMA(n = 5, on = 1, with.col = Cl, overlay = TRUE, col = "green"), # lips
#addSMA(n = 8, on = 1, with.col = Cl, overlay = TRUE, col = "red"), # teeth
#addSMA(n = 13, on = 1, with.col = Cl, overlay = TRUE, col = "blue"), # jaw

# Functions                   {{{1
# Create TAs                  {{{2
# Alligator                   {{{3
# http://stackoverflow.com/q/23090963/3569509
if(!exists("alligator", mode="function")) source("IKTrading/R/Alligator.R")

# Ichimoku                    {{{3
# https://github.com/IlyaKipnis/IKTrading/blob/master/R/ichimoku.R
if(!exists("ichimoku", mode="function")) source("IKTrading/R/ichimoku.R")

# Ichimoku
# @description The ichimoku indicator, as invented by Goichi Hosoda. It has five components.
# * The turning line is the average of the highest high and highest low of the past nFast periods.
# * The base line is computed the same way over the course of nMed periods.
# * Span A is the average of the above two calculations, projected nMed periods into the future.
# * Span B is the average of the highest high and lowest low over the past nSlow periods, also projected the same way.
# * Finally, the lagging span is the close, projected backwards by nMed periods.
# @param HLC an HLC time series
# @param nFast a fast period of days, default 8
# @param nMed a medium period of days, default 26
# @param nSlow a slow period of days, default 52
# @return The first four computations (turning line, base line, span A, span B), plotSpan (do NOT use this for backtesting, but for plotting),
# laggingSpan, and a lagged Span A and lagged Span B for comparisons with the lagging span, as per Ichimoku strategies.

# ChartSeries                 {{{2
# multi.col implements a color coding scheme used in some
# charting applications, and follows the following rules:
# • grey => Op[t] < Cl[t] and Op[t] < Cl[t-1]
# • white => Op[t] < Cl[t] and Op[t] > Cl[t-1]
# • red => Op[t] > Cl[t] and Op[t] < Cl[t-1]
# • black => Op[t] > Cl[t] and Op[t] > Cl[t-1]
funChart <- function (y) {
    chartSeries(y,
                name=paste0(nms," (",symb,")"),
                type = "candlesticks",  # type = c("auto", "candlesticks", "matchsticks", "bars","line"),
                bar.type = "hlc",
                multi.col = T,
                theme = chartTheme("white"),
                TA = c(addBBands(),         # Bolinger Bands
                       addMACD(),           # Moving Average Convergence Divergence
                       addVo()              # Volume
                       )
                )
}

# Other                       {{{2
# Create Chart                {{{1
# http://timelyportfolio.github.io/rCharts_time_series/history.html

# call chart function
nms <- paste(getQuote(symb, what=yahooQF("Name"))[,2])
udlyg <- getprice(symb); funChart(udlyg); zoomChart("last 12 weeks")

if (readchart) {

    ##  also easy zooming
    zoomChart("last 6 weeks")
    zoomChart("last 12 weeks")
    zoomChart("last 24 weeks")
    zoomChart("last 36 weeks")
    zoomChart()

    # Technical Indicators
    alligator(udlyg)

    # Ichimoku
    # zoomChart("last 12 weeks")
    addTA(ichimoku(HLC(udlyg)), on=1, lwd = 3, col = "blue")
    addTA(ichi[,2], on=1, col = "blue")

    # EMA's
    EMA9 <- EMA(Cl(udlyg), n = 9); addTA(EMA9, on=1, lwd = 3, col = "red")
    EMA20 <- EMA(Cl(udlyg), n = 20); addTA(EMA20, on=1, lwd = 3, col = "magenta")
    EMA50 <- EMA(Cl(udlyg), n = 50); addTA(EMA50, on=1, lwd = 3, col = "cyan")

    # William Percent
    addWPR()

    # Moving Average Convergence Divergence
    addMACD

}


##################################
#  performance of german stocks  #
##################################
# http://www.r-bloggers.com/displaying-german-stock-performance-with-r-using-ggplot2/

if (readchart) {
    #get list of Symbols for DAX-values
    l<- c("^GDAXI",
          "DB1.DE",
          "ADS.DE",
          "ALV.DE",
          "BAS.DE",
          "BAYN.DE",
          "BEI.DE",
          "BMW.DE",
          "CBK.DE",
          "DAI.DE",
          "DBK.DE",
          "DPW.DE",
          "DTE.DE",
          "EOAN.DE",
          "FME.DE",
          "FRE.DE",
          "HEI.DE",
          "HEN3.DE",
          "IFX.DE",
          "LHA.DE",
          "LIN.DE",
          "MAN.DE",
          "MEO.DE",
          "MUV2.DE",
          "RWE.DE",
          "SAP.DE",
          "SDF.DE",
          "SIE.DE",
          "TKA.DE",
          "VOW3.DE")
    
    getSymbols(l, from="2014-09-01")
    # getSymbols(l, from="last 6 weeks")
    l[1] <- "GDAXI"
    
    # Function to extract "adjusted prices" and build dataframe: Thanks to Zach
    # Mayer of moderntoolmaking.blogspot.com
    symbolFrame <- function(symbolList) {
    Data <- data.frame(NULL)
    for (S in symbolList) {
    Data <- cbind(Data,Ad(get(S)))
    }
    colnames(Data) <- symbolList
    return(Data)
    
    }
    
    Data <- symbolFrame(l[-1]) # build a dataframe without DAX istelf
    Data <- cbind(Ad(GDAXI), Data) # add DAX
    colnames(Data)[1] <- "DAX"
    tail(Data,2) #just to check - often Yahoo is not up to date and there are NAs in the last row
    #Data <- window(Data, start=start(Data), end=end(Data)-1) # code to delete last row...
    
    Return.calculate(Data, method="simple") -> Data.r #calculates the returns (simple)
    Data.r[is.na(Data.r)] <- 0 
    
    #builds frames for the respective perfromances on short, mid and long term
    mid.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,20)),1)))
    short.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,5)),1)))
    long.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,250)),1)))
    
    per.df <- data.frame(cbind(t(short.perf), t(mid.perf), t(long.perf)))
    
    colnames(per.df) <- c("short", "mid", "long")
    row.names(per.df)[1] <- "DAX"
    chart_title <- paste("Performance Comparison DAX values\n(latest data close of ",end(Data),")")
    z <- ggplot(data=per.df, aes(short, mid, label=rownames(per.df))) + geom_point(aes(color=long), size=4) +
    geom_text(hjust=0, vjust=0,size=4) + geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    scale_colour_gradient2(low="red", high="green", "250days\nPerformance") +
    scale_y_continuous("Mid Performance: 20days", labels= percent ) +
    scale_x_continuous("Short Performance: 5days", labels= percent )
    
    z + theme(legend.background = element_rect(colour = "black"))
}

###############
#  readchart  #
###############
readchart=T
