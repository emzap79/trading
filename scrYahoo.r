# vim: fdm=marker
#!/usr/bin/env Rscript
setwd("/home/zapata/Dokumente/Finance")
n <- 1

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
# Ticker                      {{{2
# http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
symbs <- read.csv("tickers.txt",
                  header=T, skip=0,
                  sep=';', dec='.',
                  encoding='utf-8',
                  stringsAsFactors=F, fill=T,
                  blank.lines.skip=T,
                  na.strings=c(NA,"NA"," NA ","#N/A N/A"))

# In Combination
# tickers <- na.omit(c(symbs$watchlist,symbs$medical))

# tickers <- na.omit(symbs$dax) # Dax
# tickers <- na.omit(symbs$medical) # Medical Stocks
tickers <- na.omit(symbs$watchlist) # Watchlist
# tickers <- tickers[2:length(tickers)]

# Start for-loop              {{{2
x11()
for (ticker in tickers) {
# Get Stock Data From Yahoo   {{{2

# http://timelyportfolio.github.io/rCharts_time_series/history.html
udlyg <- na.omit(getSymbols(ticker, src = "yahoo", from = "2012-01-01", auto.assign = FALSE))

# get companys name
# http://r.789695.n4.nabble.com/How-to-get-name-of-a-ticker-using-Quantmod-R-td4114044.html
nms <- paste(getQuote(ticker, what=yahooQF("Name"))[,2])

# Create Chart                {{{1
chartSeries(udlyg,
            #             subset="last 1 years",
            subset="last 4 months",
            name=paste0(nms," (",ticker,")"),
            type = "candlesticks",
            multi.col = F,
            log.scale = T,
            bar.type = "ohlc",
            theme = chartTheme("white"),
            up.col="white",dn.col="black",
            TA = c(addBBands(),
                   addMACD(),
                   addEMA(n = 20, wilder = FALSE, ratio=NULL, on = 1,
                          with.col = Cl, overlay = TRUE, col = "blue"),
                   addEMA(n = 38, wilder = FALSE, ratio=NULL, on = 1,
                          with.col = Cl, overlay = TRUE, col = "magenta"),
                   addEMA(n = 200, wilder = FALSE, ratio=NULL, on = 1,
                          with.col = Cl, overlay = TRUE, col = "cyan"),
                   addVo())
            )

# Sys.sleep(2)

# also easy zooming
# zoomChart("last 12 weeks")
# locator(1)

    # Leave Plot open until closed manually
    # http://stackoverflow.com/a/8168190/3569509
#     locator(1)


dev.copy2pdf(device="pdf", file=paste0("candle",n,".pdf"), out.type="pdf")
n <- n+1

}
