# vim: fdm=marker
#######################################################################
#                  Identify Technical Patterns in R                   #
#######################################################################
# http://www.r-bloggers.com/classical-technical-patterns/
#
# To identify a price pattern I will follow steps as described in the
# Foundations of Technical Analysis paper:
# First, fit a smoothing estimator, a kernel regression estimator, to
# approximate time series. Next, determine local extrema, tops and bottoms,
# using fist derivative of the kernel regression estimator. Define classical
# technical patterns in terms of tops and bottoms. Search for classical
# technical patterns throughout the tops and bottoms of the kernel regression
# estimator.
source("/home/zapata/.R/packages.r")
setwd("/home/zapata/Dokumente/Finance")


# Let’s begin by loading historical prices for SPY:

tdays <- 250
trange <- "2010::"

# Assign Ticker               {{{1
# read from csv
# http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
symbs <- read.csv("tickers.txt",
                  header=T, skip=0,
                  sep=';', dec='.',
                  encoding='utf-8',
                  stringsAsFactors=F, fill=T,
                  blank.lines.skip=T,
                  na.strings=c(NA,"NA"," NA ","#N/A N/A"))

# dax
symb <- na.omit(symbs$planspiel_alle)
names(symb) <- na.omit(symbs$ps_alle_names[1:length(symb)]); names(symb)
ticker <- symb[grep("adidas", names(symb), ignore.case=T)]

# Pattern Matching       {{{1
if(!exists("pattern.db", mode="function")) source("rfinance2012.r")

# I put the logic for the entire process in to the plot.patterns() helper
# function. The plot.patterns() function first call find.extrema() function to
# determine extrema points, and next it calls find.patterns() function to find
# and plot patterns. Let’s find classical technical patterns in the last 150
# days of SPY history:

# Load Historical Data   {{{1
data = getSymbols(ticker, src = 'yahoo', from = '1970-01-01', auto.assign = F)
data = adjustOHLC(data, use.Adjusted=T)

# Find Classical Techical Patterns, based on
# Pattern Matching. Based on Foundations of Technical Analysis
# by A.W. LO, H. MAMAYSKY, J. WANG
# plot.patterns(data, tdays, ticker)

# The first step is to fit a smoothing estimator, a kernel regression
# estimator, to approximate time series. I used sm package to fit kernel
# regression:

y = as.vector( last( Cl(data), tdays) )
t = 1:len(y)

# fit kernel regression with cross-validatio
h = h.select(t, y, method = 'cv')
temp = sm.regression(t, y, h=h, display = 'none')

# find estimated fit
mhat = approx(temp$eval.points, temp$estimate, t, method='linear')$y

# The second step is to find local extrema, tops and bottoms, using first
# derivative of the kernel regression estimator. (more details in the paper on
# page 15):

temp = diff(sign(diff(mhat)))
# loc - location of extrema, loc.dir - direction of extrema
loc = which( temp != 0 ) + 1
loc.dir = -sign(temp[(loc - 1)])

# I put the logic for the first and second step into the find.extrema()
# function.

# The next step is to define classical technical patterns in terms of tops and
# bottoms. The pattern.db() function implements the 10 patterns described in
# the paper on page 12. For example, let’s have a look at the Head and
# Shoulders pattern. The Head and Shoulders pattern:

# is defined by 5 extrema points (E1, E2, E3, E4, E5)
# starts with a maximum (E1)
# E1 and E5 are within 1.5 percent of their average
# E2 and E4 are within 1.5 percent of their average

# The R code below that corresponds to the Head and Shoulders pattern is a
# direct translation, from the pattern description in the paper on page 12, and
# is very readable:

# Plot Patterns          {{{1
plot.patterns(data, tdays, ticker)

# It is very easy to define you own custom patterns and I encourage everyone to
# give it a try. To view the complete source code for this example, please have
# a look at the pattern.test() function in rfinance2012.r at github:
# https://github.com/systematicinvestor/SIT/blob/master/R/rfinance2012.r
