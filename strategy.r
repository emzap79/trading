# File: strategy.r
# Author: Jonas Petong
# Description: some trading strategy for r
# Last Modified: Oktober 15, 2014

# Hi All,

# I'm posting a small example of using signal formulas to help out other
# beginners that are getting started with quantstrat.  It also includes stuff
# copied from other examples such as the CUD custom indicator from
# timelyportfolio, reading a OHLC CSV file with intraday bars downloaded from
# Dukascopy, adding TA indicators to plots, and saving results to files.  I
# may have gone a bit off the deep end with the signal formulas.  I'm certain
# that there are smarter ways to do some of this.  I am also attaching a small
# CSV file to play with.

# Thanks in advance for comments, suggestions, and warnings.

# Rob Schmidt
# https://stat.ethz.ch/pipermail/r-sig-finance/2013q1/011439.html

# the frankenstein of signal formulas,  Rob Schmidt
# prices from online, or from a CSV file downloaded from dukascopy

# thanks to the people who made these great packages and
# thanks to all who made the examples from which I copied

csvPath = '/home/zapata/Dokumente/Finance'  #  these need to change
resultsPath = '/home/zapata/Dokumente/Finance/output'
setwd(resultsPath)

install.packages('quantstrat',
repos = c('http://rforge.net', 'http://R-Forge.R-project.org', 'http://cran.rstudio.org', 'http://cran.r-project.org'),
type = 'source')
library("quantstrat")

try(rm("order_book.frank", pos=.strategy), silent=TRUE)
try(rm("account.frank", "portfolio.frank", pos=.blotter), silent=TRUE)

csvName = '' # assign an empty string to choose daily data from yahoo
# example file downloaded from dukascopy for evaluation
#csvName = 'AMZN.US_Candlestick_15_m_BID_01.01.2013-12.01.2013.csv'

stratg = "frank"  #  a creature from a collection of parts
symbol = "AMZN"

cudPeriods = 4
fastPeriods = 15
slowPeriods = 2*fastPeriods
initEq = 1.0e6
orderSize = 100

# CUD is the n-period sum of (up days = 1) and (down days = -1)
CUD = function(cl, n)
{   temp = runSum(ifelse(ROC(cl, 1, type="discrete") > 0, 1, -1), n)
    colnames(temp) = "CUD"
    temp
}

currency("USD")
stock(symbol, currency="USD", multiplier=1)

if(csvName == '')  #  get price data from yahoo online
{   initDate = '2011-07-01'
    endDate = '2012-01-01'
    getSymbols(symbol, from=initDate, to=endDate, adjust=TRUE)
} else   #   get price data from a CSV file
{   dukasFormat = '%d.%m.%Y %H:%M:%S'
     csvData = read.csv(paste(csvPath, csvName, sep=''), sep=',',
header=TRUE)
    # ignore empty trading periods, makes the plots prettier
    csvData = csvData[(csvData$High > csvData$Low),]

    #initDate = ''  # if empty string calculate from the start of the data
    initDate = '2013-01-05 00:00:00'    # else set init date
    #endDate = ''   # if empty string calculate to the end of the data
    endDate = '2013-01-09 00:00:00'    # else set end date
    # apply the user's end date if it is specified
    if(initDate != '')
    {   initTime = as.POSIXct(strptime(initDate, '%Y-%m-%d %H:%M:%S'))
        csvData = csvData[(as.POSIXct(strptime(
                    csvData$Time, dukasFormat)) > initTime),]
    }
    if(endDate != '')
    {   endTime = as.POSIXct(strptime(endDate, '%Y-%m-%d %H:%M:%S'))
        csvData = csvData[(as.POSIXct(strptime(
                    csvData$Time, dukasFormat)) < endTime),]
    }
    symb = as.xts(csvData[,2:6],
        as.POSIXct(strptime(csvData[,1], dukasFormat)))
    colnames(symb) = c(paste(symbol,'.Open',sep=''),
                        paste(symbol,'.High',sep=''),
                        paste(symbol,'.Low',sep=''),
                        paste(symbol,'.Close',sep=''),
                        paste(symbol,'.Volume',sep=''))
    assign(symbol,symb)
    initDate = start(get(symbol))
    endDate = end(get(symbol))
}

# make some signal formula parts from indicators

cudUp = "(CUD > 0)"
cudDn = "(CUD < 0)"
smaUp = "(smaFast > smaSlow)"
smaDn = "(smaFast < smaSlow)"
closeAbvS = paste("(",symbol,".Close > smaSlow)",sep='')
closeBlwS = paste("(",symbol,".Close < smaSlow)",sep='')
closeAbvF = paste("(",symbol,".Close > smaFast)",sep='')
closeBlwF = paste("(",symbol,".Close < smaFast)",sep='')

# combine parts for entry/exit, long/short signal formulas

# goLong = cudUp
# exitLong = cudDn
# goShort = cudDn
# exitShort = cudUp

goLong = cudDn  # counter-trend madness
exitLong = cudUp
goShort = cudUp
exitShort = cudDn

# goLong = paste(cudUp,"&",closeAbvS,sep='')
# exitLong = paste(cudDn,"|",closeBlwS,sep='')
# goShort = paste(cudDn,"&",closeBlwS,sep='')
# exitShort = paste(cudUp,"|",closeAbvS,sep='')

# goLong = paste(smaUp,"&",closeAbvF,"&",cudUp,sep='')
# exitLong = paste(smaDn,"|",closeBlwF,sep='')
# goShort = paste(smaDn,"&",closeBlwF,"&",cudDn,sep='')
# exitShort = paste(smaUp,"|",closeAbvF,sep='')

# goLong = paste(smaUp,"&",closeAbvF,"&",cudUp,sep='')
# exitLong = paste(smaDn,"|",closeBlwS,sep='')
# goShort = paste(smaDn,"&",closeBlwF,"&",cudDn,sep='')
# exitShort = paste(smaUp,"|",closeAbvS,sep='')

initPortf(stratg, symbol, initDate=initDate)
initAcct(stratg, portfolios=stratg, initDate=initDate,
        initEq=initEq)
initOrders(portfolio=stratg, initDate=initDate)

strat = strategy(stratg)

strat = add.indicator(strategy=strat, name="SMA", label="smaFast",
                arguments=list(x=quote(Cl(mktdata)), n=fastPeriods))
strat = add.indicator(strategy=strat, name="SMA", label="smaSlow",
                arguments=list(x=quote(Cl(mktdata)), n=slowPeriods))
strat = add.indicator(strategy=strat, name="CUD", label="CUD",
                arguments=list(cl=get(symbol)[,4], n=cudPeriods))

strat = add.signal(strat, name="sigFormula", label="goLongSig",
            arguments = list(columns=c("smaFast","smaSlow","CUD"),
            formula = goLong, cross=TRUE))
strat = add.signal(strat, name="sigFormula", label="exitLongSig",
            arguments = list(columns=c("smaFast","smaSlow","CUD"),
            formula = exitLong, cross=TRUE))
strat = add.signal(strat, name="sigFormula", label="goShortSig",
            arguments = list(columns=c("smaFast","smaSlow","CUD"),
            formula = goShort, cross=TRUE))
strat = add.signal(strat, name="sigFormula", label="exitShortSig",
            arguments = list(columns=c("smaFast","smaSlow","CUD"),
            formula = exitShort, cross=TRUE))

strat = add.rule(strategy = strat, name='ruleSignal', type='enter',
                arguments = list(sigcol="goLongSig", sigval=TRUE,
                orderqty=orderSize, ordertype='market', orderside='long',
                osFUN=osMaxPos))
strat = add.rule(strategy = strat, name='ruleSignal', type='exit',
                arguments = list(sigcol="exitLongSig", sigval=TRUE,
                orderqty='all', ordertype='market', orderside='long'))
strat = add.rule(strategy = strat, name='ruleSignal', type='enter',
                arguments = list(sigcol="goShortSig", sigval=TRUE,
                orderqty=-orderSize, ordertype='market', orderside='short',
                osFUN=osMaxPos))
strat = add.rule(strategy = strat, name='ruleSignal', type='exit',
                arguments = list(sigcol="exitShortSig", sigval=TRUE,
                orderqty='all', ordertype='market', orderside='short'))

addPosLimit(stratg, symbol, timestamp=initDate,
            maxpos=orderSize, minpos=-orderSize)

out = applyStrategy(strategy=strat, portfolios=stratg, prefer='Open')
updatePortf(Portfolio=stratg)
updateAcct(name=stratg)

# make plots
dateRange = paste(initDate,'::',endDate,sep='')
chart.Posn(Portfolio = stratg, Symbol = symbol, Dates=dateRange)
add_SMA(n=fastPeriods, on=1, col='blue')
add_SMA(n=slowPeriods, on=1, col='tan')
plot(add_TA(CUD(cl=get(symbol)[,4],n=cudPeriods)))

# write the results to files
ob = getOrderBook(stratg)
write.zoo(ob[1], quote=TRUE,
        file = paste(stratg, "_", symbol, "_orderbook.csv",sep=''))
write.zoo(mktdata, quote=TRUE,
    file=paste(stratg, "_",symbol, "_mktdata.csv", sep=''))
write.zoo(getTxns(stratg, symbol), quote=TRUE,
        file = paste(stratg, "_", symbol, "_txns.csv", sep=''))
