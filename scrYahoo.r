# vim: fdm=marker
#!/usr/bin/env Rscript
setwd("/home/zapata/Dokumente/Finance")

scan()
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

# tickers <- c("AAPL",    # Apple
#              "GOOGL",   # Google
#              "AMZN",    # Amazon
#              "MSFT",    # Microsoft
#              "TSLA")    # Tesla

# Planspiel                   {{{2
# http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
symbs <- scan(file="tickers.txt", sep=";",
              what=list(dax="",dax_names="",planspiel="",ps_names=""),
              skip=1)
tickers <- symbs$planspiel

for (ticker in tickers) {
# Get Stock Data From Yahoo   {{{2

# http://timelyportfolio.github.io/rCharts_time_series/history.html
udlyg <- na.omit(getSymbols(ticker, src = "yahoo", from = "2012-01-01", auto.assign = FALSE))

# get companys name
# http://r.789695.n4.nabble.com/How-to-get-name-of-a-ticker-using-Quantmod-R-td4114044.html
nms <- paste(getQuote(ticker, what=yahooQF("Name"))[,2])

# Create Chart                {{{1
    x11()
    chartSeries(udlyg,
                subset="last 8 weeks",
                name=paste0(nms," (",ticker,")"),
                type = "candlesticks",
                multi.col = T,
                bar.type = "ohlc",
                theme = chartTheme("white"),
                TA = c(addBBands(),
                       addMACD(),
                       addEMA(n = 20, wilder = FALSE, ratio=NULL, on = 1,
                              with.col = Cl, overlay = TRUE, col = "blue"),
                       addEMA(n = 50, wilder = FALSE, ratio=NULL, on = 1,
                              with.col = Cl, overlay = TRUE, col = "yellow"),
                       addVo())
                )

    # Leave Plot open until closed manually
    # http://stackoverflow.com/a/8168190/3569509
    locator(1)

    # also easy zooming
    #     zoomChart("last 12 weeks")
    #     locator(1)
}

