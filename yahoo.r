# vim: fdm=marker
#!/usr/bin/env Rscript
# http://www.r-bloggers.com/stock-analysis-using-r/

source("/home/zapata/.R/packages.r")
setwd("/home/zapata/Dokumente/Finance")
# Assign Ticker               {{{1
# read from csv               {{{2
# http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
symbs <- read.csv("tickers.txt",
                  header=T, skip=0,
                  sep=';', dec='.',
                  encoding='utf-8',
                  stringsAsFactors=F, fill=T,
                  blank.lines.skip=T,
                  na.strings=c(NA,"NA"," NA ","#N/A N/A"))

# # medical "ebola"           {{{2
# symb <- na.omit(symbs$medical)
# names(symb) <- na.omit(symbs$med_names); names(symb)

# # dax                       {{{2
# symb <- na.omit(symbs$dax)
# names(symb) <- na.omit(symbs$dax_names); names(symb)

# # watchlist                 {{{2
# symb <- na.omit(symbs$watchlist)
# names(symb) <- na.omit(symbs$wl_names); names(symb)

# ## aktien
# # symb <- symb[1:169]
# ## zertifikate
# # symb <- symb[169:length(symb)]

# planspiel (alle)            {{{2
symb <- na.omit(symbs$planspiel_alle)
names(symb) <- na.omit(symbs$ps_alle_names[1:length(symb)]); names(symb)

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

# Functions/Indicators        {{{1
# Create TAs                  {{{2
# Alligator                   {{{3
# http://stackoverflow.com/q/23090963/3569509
if(!exists("alligator", mode="function")) source("TA/Alligator.R")

# Ichimoku                    {{{3
# http://www.quantf.com/blog/fotis-papailias/ichimoku-clouds-r-code-trading/938
if(!exists("ichimoku", mode="function")) source("TA/ichimoku.R")

# ChartSeries                 {{{2
# multi.col implements a color coding scheme used in some
# charting applications, and follows the following rules:
# • grey => Op[t] < Cl[t] and Op[t] < Cl[t-1]
# • white => Op[t] < Cl[t] and Op[t] > Cl[t-1]
# • red => Op[t] > Cl[t] and Op[t] < Cl[t-1]
# • black => Op[t] > Cl[t] and Op[t] > Cl[t-1]
funChart <- function (y,logrt=F) {
    chartSeries(y,
                name=paste0(nms," (",tick,")"),
                type = "candlesticks",  # type = c("auto", "candlesticks", "matchsticks", "bars","line"),
                bar.type = "hlc",
                multi.col = F,
                log.scale=logrt,
                theme = chartTheme("white"),
                TA = c(addBBands(),         # Bolinger Bands
                       addMACD(),           # Moving Average Convergence Divergence
                       addSAR(),            # Parabolic Stop and Reverse
                       addSMI(),            # Stochastic Momentum
                       addVo()              # Volume
                       )
                )
}

# Other Functions             {{{2
# Best DAX performers
# if(!exists("funMovers", mode="function")) source("FUN/DaxMovers.r")

# DAX performers recent weeks
# funMovers <- function(l)

# Other                       {{{2
# Create Chart                {{{1
# http://timelyportfolio.github.io/rCharts_time_series/history.html

# Source Indicators
# source("/home/zapata/Dokumente/Finance/indicators.r")

# choose ticker
tick <- symb[grep("procter", names(symb), ignore.case=T, value=F)]

# call chart function
nms <- paste(getQuote(tick, what=yahooQF("Name"))[,2])
udlyg <- getprice(tick); funChart(udlyg,logrt=T); zoomChart("last 24 weeks")

##  also easy zooming
# zoomChart("last 6 weeks")
# zoomChart("last 12 weeks")
# zoomChart("last 24 weeks")
# zoomChart("last 36 weeks")
# zoomChart("last 2 years")
# zoomChart()
