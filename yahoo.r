# vim: fdm=marker
#!/usr/bin/env Rscript
readchart=F

symb <- "AAPL"; nms <- "Apple"
symb <- "BABA"; nms <- "Alibaba"
symb <- "BMW.DE"; nms <- "BMW"
symb <- "DAI.DE"; nms <- "Daimler"
symb <- "GOOG"; nms <- "Google"
symb <- "POAHF"; nms <- "Porsche"   # OTC
symb <- "TSLA"; nms <- "Tesla"

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

#install.packages("RCurl")
#install.packages('ramnathv-rCharts-2c368c8.tar.gz', repos = NULL, type = 'source')
#require(rCharts)

# Retreive Data               {{{1
# Get Stock Data From Yahoo   {{{2
getprice <- function (x) {
    #     na.omit(getSymbols(x, src = "yahoo", from = "2012-01-01", auto.assign = FALSE))
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
jaw <- SMA(Cl(udlyg),13)
teeth <- SMA(Cl(udlyg),8)
lips <- SMA(Cl(udlyg),5)
alligator <- function (alg) {
    c(addTA(lag(jaw,8) , on = 1 , col ='blue'),
      addTA(lag(teeth,5) , on = 1 , col ='red'),
      addTA(lag(lips,3) , on = 1 , col ='green'))
}

# funAlligator <- function (a) {
#     c(addTA(lag(SMA(Cl(a),13),8), on = 1),
#       addTA(lag(SMA(Cl(a),8),5), on = 1),
#       addTA(lag(SMA(Cl(a),5),3), on = 1)
#       )
# }
#
# alligator <- newTA(FUN = funAlligator, # chartSeries will pass whole dataset,
#               lty   = c("solid", "solid", "dotted"),
#               legend.name = "The Alligator",
#               col   = c("green", "red", "blue")
#               )

# chart_Series(udlyg ,subset="2014::")
# add_TA(lag(jaw,8) , on = 1 , col ='blue')
# add_TA(lag(teeth,5) , on = 1 , col ='red')
# add_TA(lag(lips,3) , on = 1 , col ='green')

# Ichimoku                    {{{3
# https://github.com/IlyaKipnis/IKTrading/blob/master/R/ichimoku.R
if(!exists("ichimoku", mode="function")) source("TechnAnalyse/R/ichimoku.R")

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
                name=paste0(nms),
                type = "candlesticks",  # type = c("auto", "candlesticks", "matchsticks", "bars","line"),
                bar.type = "hlc",
                multi.col = T,
                theme = chartTheme("white"),
                TA = c(addBBands(),         # Bolinger Bands
                       addVo(),            # Volume
                       addMACD()            # Moving Average Convergence Divergence
                       )
                )
}

# Other                       {{{2
# Create Chart                {{{1
# http://timelyportfolio.github.io/rCharts_time_series/history.html

# call chart function
udlyg <- getprice(symb); funChart(udlyg)

if (readchart) {

    # Technical Indicators
    alligator(Cl(udylg))

    # Ichimoku
    ichi <- HLC(udlyg)
    addTA(ichimoku(ichi),on=1)

    # EMA's
    EMA20 <- EMA(Cl(udlyg), n = 20)
    EMA50 <- EMA(Cl(udlyg), n = 50)
    addTA(EMA20, on=1, lwd = 1, col = "pink")
    addTA(EMA50, on=1, lwd = 1, col = "cyan")

    # William Percent
    addWPR()

    # Moving Average Convergence Divergence
    addMACD

}
readchart=T

##  also easy zooming
zoomChart("last 8 weeks")
zoomChart()
