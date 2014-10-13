#######################################################################
#                         Backtesting with R                          #
#######################################################################
# http://blog.fosstrading.com/2011/03/how-to-backtest-strategy-in-r.html

# run the command below if quantmod isn't already installed
# install.packages("quantmod")
# use the quantmod package (loads TTR, xts, and zoo)
require(quantmod)
# pull SPX data from Yahoo (getSymbols returns an xts object)
getSymbols("^GSPC")

# Step 2: Create your indicator
# The TTR package contains a multitude of indicators. The indicators are
# written to make it easy to combine them in creative and unconventional ways.
# Starting with revision 106 on R-forge, TTR has a DVI indicator.

# calculate DVI indicator
dvi <- DVI(Cl(GSPC)) # Cl() extracts the close price column

# Step 3: Construct your trading rule
# Since this trading rule is simple--we're long 100% if the DVI is below 0.5
# and short 100% otherwise--it can be written in a single line. More elaborate
# rules and/or position sizings can be done as well, but require more code
# (RSI(2) with Position Sizing is an example of more complex position sizing
# rules). Also notice that the signal vector is lagged, which avoids
# look-ahead bias.

# create signal: (long (short) if DVI is below (above) 0.5)
# lag so yesterday's signal is applied to today's returns
sig <- Lag(ifelse(dvi$dvi < 0.5, 1, -1))

# Step 4: The trading rules/equity curve
# As in Damian's example, the code below is a simplified approach that is
# frictionless and does not account for slippage. The code below takes today's
# percentage return and multiplies it by yesterday's signal / position size
# (always +/- 100% in this example). I also subset the system returns to match
# the results in the Excel file.

# calculate signal-based returns
ret <- ROC(Cl(GSPC))*sig    # ROC: rate of change of a series over n periods

# subset returns to match data in Excel file
ret <- ret['2009-06-02/2010-09-07']

# Step 5: Evaluate strategy performance
# Damian mentioned the importance of evaluating your strategy. Fortunately for
# R users, the PerformanceAnalytics package makes this easy. With a few lines
# of code we can view the drawdowns, downside risks, and a performance summary.

# use the PerformanceAnalytics package
# install.packages("PerformanceAnalytics")
require(PerformanceAnalytics)
# create table showing drawdown statistics
table.Drawdowns(ret, top=10)
# create table of downside risk estimates
table.DownsideRisk(ret)
# chart equity curve, daily performance, and drawdowns
charts.PerformanceSummary(ret)

# calculate Sharpe- and SortinoRatio
OmegaSharpeRatio(ret)
SharpeRatio(ret, Rf = 0, p = 0.95, FUN = c("StdDev", "VaR", "ES"), weights = NULL)
SortinoRatio(ret, MAR=0)

# That's all there is to backtesting a simple strategy in R. It wasn't that
# intimidating, was it? Please leave feedback if you're moving your
# backtesting from Excel to R and there's something you're hung up on or you
# have an awesome tip you'd like to share.

#######################################################################
#                               blabla                                #
#######################################################################
# http://statmath.wu.ac.at/~hauser/LVs/FinEtricsQF/ReadFinData_R.txt
# spc <- new.env()
# dax <- new.env()
# nasdaq <- new.env()
#
# setDefaults(getSymbols,src="yahoo")
# getSymbols("^GSPC", env = spc, from = "1995-01-01", to = "2014-07-18", return.class = "xts")
# # to = "2006-12-31")
# getSymbols("^GDAXI", env = dax, from = "1995-01-01", to = "2014-07-18", return.class = "xts")
# getSymbols("^NDX", env = nasdaq, from = "1995-01-01", to = "2014-07-18", return.class = "xts")
# # read in series are of xts type,  plot is different for xts and ts
#
# ## series are of class xts  (plot for xts =/= for ts)
#
# y <- merge(spc$GSPC[,4], dax$GDAXI[,4], nasdaq$NDX[,4], all = F) # Alle close Werte
# r <- diff(log(y))     # diff preserves the time-structure of y
#
# timeframe <- time(y)[-1] # time line of y (without first Index, as this is NA for returns)
# r <- r[timeframe,]
#
# before <- timeframe[timeframe < as.Date("2001-09-11")]
# after  <- timeframe[timeframe > as.Date("2001-09-11")]
#
# rb <- r[before,]
# ra <- r[after,]
#
# plot(rb[,1])
