# vim: fdm=marker
#!/usr/bin/env Rscript
readchart=F

symb <- "AAPL"; nms <- "Apple"
symb <- "CELG"; nms <- "Celgene"    # http://www.lynxbroker.de/lynx-boersenblick/20141006/celgene-sieht-nach-wie-vor-sehr-stark-aus/
symb <- "BABA"; nms <- "Alibaba"
symb <- "DAI.DE"; nms <- "Daimler"
symb <- "GOOG"; nms <- "Google"
symb <- "POAHF"; nms <- "Porsche"   # OTC
symb <- "VOW3"; nms <- "Volkswagen"
symb <- "TSLA"; nms <- "Tesla"

# Planspiel
symb <- "NOK"; nms <- "Nokia"
symb <- "BMW.DE"; nms <- "BMW"
symb <- "HP"; nms <- "HP"
symb <- "UL"; nms <- "Unilever"
symb <- "PG"; nms <- "Procter & Gamble"
symb <- "NESR.DE"; nms <- "Nestlè"
symb <- "HNNMY"; nms <- "H & M"
symb <- "SZG.DE"; nms <- "Salzgitter"
symb <- "AMD"; nms <- "AMD"
symb <- "T"; nms <- "AT&T"
symb <- "ATL.MI"; nms <- "Atlantia SpA"
symb <- "IFX.DE"; nms <- "Infineon"

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
                name=paste0(nms),
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
udlyg <- getprice(symb); funChart(udlyg); zoomChart("last 24 months")

##  also easy zooming
zoomChart("last 36 weeks")
zoomChart("last 24 weeks")
zoomChart("last 12 weeks")
zoomChart()

if (readchart) {

    # Technical Indicators
    alligator(udlyg)

    # Ichimoku
    # zoomChart("last 12 weeks")
    ichimoku(HLC(udlyg))

    # EMA's
    EMA9 <- EMA(Cl(udlyg), n = 9); addTA(EMA9, on=1, lwd = 3, col = "red")
    EMA20 <- EMA(Cl(udlyg), n = 20); addTA(EMA20, on=1, lwd = 3, col = "magenta")
    EMA50 <- EMA(Cl(udlyg), n = 50); addTA(EMA50, on=1, lwd = 3, col = "cyan")

    # William Percent
    addWPR()

    # Moving Average Convergence Divergence
    addMACD

}
readchart=T

