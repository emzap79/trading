# vim: fdm=marker
#!/usr/bin/env Rscript
# http://www.r-bloggers.com/stock-analysis-using-r/

source("/home/zapata/.R/packages.r")
# Assign Ticker                                                                     {{{1
# read from csv                                                                     {{{2
# http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
symbs <- read.csv("tickers.txt",
                  header=T, skip=0,
                  sep=';', dec='.',
                  encoding='utf-8',
                  stringsAsFactors=F, fill=T,
                  blank.lines.skip=T,
                  na.strings=c(NA,"NA"," NA ","#N/A N/A"))

# # medical "ebola"                                                                   {{{2
# symb <- na.omit(symbs$medical)
# names(symb) <- na.omit(symbs$med_names); names(symb)

# # dax                                                                               {{{2
# symb <- na.omit(symbs$dax)
# names(symb) <- na.omit(symbs$dax_names); names(symb)

# # watchlist                                                                         {{{2
# symb <- na.omit(symbs$watchlist)
# names(symb) <- na.omit(symbs$wl_names); names(symb)

# ## aktien
# # symb <- symb[1:169]
# ## zertifikate
# # symb <- symb[169:length(symb)]

# planspiel (alle)                                                                  {{{2
symb <- na.omit(symbs$planspiel_alle)
names(symb) <- na.omit(symbs$ps_alle_names[1:length(symb)]); names(symb)

# Retreive Data                                                                     {{{1
# Get Stock Data From Yahoo                                                         {{{2
getprice <- function (x) {
    na.omit(getSymbols(x, src = "yahoo", auto.assign = FALSE))
}

## add legend
## http://r.789695.n4.nabble.com/main-title-in-plot-outer-TRUE-cut-off-td4204006.html
#title(outer=TRUE,adj=0,main = list("1 a)", cex=1.1,col="black", font=2), line = -1)

#addSMA(n = 5, on = 1, with.col = Cl, overlay = TRUE, col = "green"), # lips
#addSMA(n = 8, on = 1, with.col = Cl, overlay = TRUE, col = "red"), # teeth
#addSMA(n = 13, on = 1, with.col = Cl, overlay = TRUE, col = "blue"), # jaw

# Functions                                                                         {{{1
# Create TAs                                                                        {{{2
# Alligator
# http://stackoverflow.com/q/23090963/3569509
if(!exists("alligator", mode="function")) source("IKTrading/R/Alligator.R")

# Ichimoku
# https://github.com/IlyaKipnis/IKTrading/blob/master/R/ichimoku.R
if(!exists("ichimoku", mode="function")) source("IKTrading/R/ichimoku.R")

# ChartSeries                                                                       {{{2
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

# Other Functions                                                                   {{{2
# Best DAX performers
# if(!exists("funMovers", mode="function")) source("functions/DaxMovers.r")

# DAX performers recent weeks
# funMovers <- function(l)

# Other                                                                             {{{2
# Create Chart                                                                      {{{1
# http://timelyportfolio.github.io/rCharts_time_series/history.html

# choose ticker
tick <- symb[grep("procter", names(symb), ignore.case=T)]

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

# technical indicators
# source("/home/zapata/Dokumente/Finance/indicators.r")

# The function for computing the Ichimoku cloud
# http://www.quantf.com/blog/fotis-papailias/ichimoku-clouds-r-code-trading/938
ichimoku <- function(data,pars)
{

# REMEMBER THAT THE DATA SHOULD BE IN ORDER
#
# HIGH, LOW and CLOSE
#
# ==========================================

# Number of observations
Nobs <- NROW(data)

# Get the three parameters
p1 <- pars[1]
p2 <- pars[2]
p3 <- pars[3]

# The maximum of these should be p3, check
if ((p1 > p2) | (p1 > p3) | (p2 > p3))
{
stop("parameters should enter in ascending order")
}
# Set the max
maxp <- p3

# You will leave out maxp observations
cloud.lines <- matrix(0,nrow=Nobs-maxp,ncol=5)
colnames(cloud.lines) <- c("Tenkan","Kijun","SenkouA","SenkouB","Chikou")

# Run a loop to make the computations
for (i in seq(maxp+1,Nobs,1))
{
# Compute the cloud lines
tenkan <- (max(data[seq(i-p1,i,1),1])+min(data[seq(i-p1,i,1),2]))/2
kijun <- (max(data[seq(i-p2,i,1),1])+min(data[seq(i-p2,i,1),2]))/2
senkouA<- (tenkan+kijun)/2
senkouB<- (max(data[seq(i-p3,i,1),1])+min(data[seq(i-p3,i,1),2]))/2
chikou <- data[i,3]

# Save in appropriate places
cloud.lines[(i-maxp),] <- c(tenkan,kijun,senkouA,senkouB,chikou)
}

# OK, now align them correctly: SenkouA and SenkouB are moved p2 periods forward
# while Chikou is moved p2 periods backward…
A1 <- rbind(cloud.lines[,1:2],matrix(NA,p2,2))
A2 <- rbind(matrix(NA,p2,2),cloud.lines[,3:4])
A3 <- c(cloud.lines[(p2+1):(Nobs-maxp),5],matrix(NA,2*p2,1))
new.cloud.lines <- cbind(A1,A2,A3)
colnames(new.cloud.lines) <- colnames(cloud.lines)

# Align the data as well
new.data <- rbind(data[(maxp+1):Nobs,],matrix(NA,p2,3))

rbind(data[(maxp+1):Nobs,],matrix(NA,p2,3))
colnames(new.data) <- colnames(data)

# OK, return everything
return(list(data=new.data,cloud.lines=new.cloud.lines))
}

# Set the ichimoku parameters
pars <- c(50,100,120)

x <- as.matrix(cbind(Hi(udlyg),Lo(udlyg),Cl(udlyg)))
colnames(x) <- c("High","Low","Close")
out <- ichimoku(x,pars)
out$cloud.lines

addTA(out[[2]])
