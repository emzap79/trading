# vim: fdm=marker
#!/usr/bin/env Rscript
readchart=F

# Read From CSV               {{{1
# http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
symbs <- read.csv("tickers.txt",
                  header=T, skip=0,
                  sep=';', dec='.',
                  encoding='utf-8',
                  stringsAsFactors=F, fill=T,
                  blank.lines.skip=T,
                  na.strings=c(NA,"NA"," NA ","#N/A N/A"))

# medical "ebola"             {{{2
symb <- na.omit(symbs$medical)
names(symb) <- na.omit(symbs$med_names); names(symb)

# planspiel                   {{{2
symb <- na.omit(symbs$planspiel)
names(symb) <- na.omit(symbs$ps_names); names(symb)

# planspiel (alle)            {{{2
symb <- na.omit(symbs$ps_alle)
names(symb) <- na.omit(symbs$ps_alle_names); names(symb)
## aktien
symb <- symb[1:169]
## zertifikate
symb <- symb[169:length(symb)]

# choose ticker               {{{2
symb <- symb[2]

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
library(scales)


#install.packages("RCurl")
#install.packages('ramnathv-rCharts-2c368c8.tar.gz', repos = NULL, type = 'source')
#require(rCharts)

# Retreive Data               {{{1
# Get Stock Data From Yahoo   {{{2
getprice <- function (x) {
    na.omit(getSymbols(x, src = "yahoo", auto.assign = FALSE))
}

## add legend
## http://r.789695.n4.nabble.com/main-title-in-plot-outer-TRUE-cut-off-td4204006.html
#title(outer=TRUE,adj=0,main = list("1 a)", cex=1.1,col="black", font=2), line = -1)

#addSMA(n = 5, on = 1, with.col = Cl, overlay = TRUE, col = "green"), # lips
#addSMA(n = 8, on = 1, with.col = Cl, overlay = TRUE, col = "red"), # teeth
#addSMA(n = 13, on = 1, with.col = Cl, overlay = TRUE, col = "blue"), # jaw

# Functions                   {{{1
# Create TAs                  {{{2
# Alligator
# http://stackoverflow.com/q/23090963/3569509
if(!exists("alligator", mode="function")) source("IKTrading/R/Alligator.R")

# Ichimoku
# https://github.com/IlyaKipnis/IKTrading/blob/master/R/ichimoku.R
if(!exists("ichimoku", mode="function")) source("IKTrading/R/ichimoku.R")

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

# Other Functions             {{{2
# Best DAX performers
# if(!exists("funMovers", mode="function")) source("functions/DaxMovers.r")

# DAX performers recent weeks
# funMovers <- function(l)

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
    zoomChart("last 2 years")
    zoomChart()

    # Technical Indicators
    alligator(udlyg)

    # Ichimoku
    # zoomChart("last 12 weeks")
    #     addTA(ichimoku(HLC(udlyg)))
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

###############
#  readchart  #
###############
readchart=T
