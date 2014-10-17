##################################
#  performance of german stocks  #
##################################
# http://www.r-bloggers.com/displaying-german-stock-performance-with-r-using-ggplot2/
library(quantmod)
library(ggplot2)
library(zoo)
library(RCurl)
library(XML)
setwd("/home/zapata/Dokumente/Finance")

#get list of Symbols for DAX-values
# http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
symbs <- read.csv("tickers.txt",
                  header=T, skip=0,
                  sep=';', dec='.',
                  stringsAsFactors=F, fill=T,
                  blank.lines.skip=T,
                  na.strings=c(NA,"NA"," NA ","#N/A N/A"))

# l <- na.omit(symbs$dax)
# names(l) <- na.omit(symbs$dax_names); names(l)
l <- na.omit(symbs$planspiel)
names(l) <- na.omit(symbs$ps_names); names(l)

# get last n month's start-date
# http://stackoverflow.com/a/13268816/3569509
timerange <- Sys.Date() - 365

getSymbols(l, from=timerange)
l[1] <- "GDAXI"

# Function to extract "adjusted prices" and build dataframe: Thanks to Zach
# Mayer of moderntoolmaking.blogspot.com
symbolFrame <- function(symbolList) {
    Data <- data.frame(NULL)
    for (S in symbolList) {
        Data <- cbind(Data,Ad(get(S)))
    }
    colnames(Data) <- symbolList
    return(Data)

}

Data <- symbolFrame(l[-1]) # build a dataframe without DAX istelf
Data <- cbind(Ad(GDAXI), Data) # add DAX
colnames(Data)[1] <- "DAX"
tail(Data,2) #just to check - often Yahoo is not up to date and there are NAs in the last row
#Data <- window(Data, start=start(Data), end=end(Data)-1) # code to delete last row...

Return.calculate(Data, method="simple") -> Data.r #calculates the returns (simple)
Data.r[is.na(Data.r)] <- 0

#builds frames for the respective perfromances on short, mid and long term
mid.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,20)),1)))
short.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,5)),1)))
long.perf <- as.data.frame(coredata(tail(cumsum(tail(Data.r,250)),1)))

per.df <- data.frame(cbind(t(short.perf), t(mid.perf), t(long.perf)))

colnames(per.df) <- c("short", "mid", "long")
row.names(per.df)[1] <- "DAX"
chart_title <- paste("Performance Comparison DAX values\n(latest data close of ",end(Data),")")
z <- ggplot(data=per.df, aes(short, mid, label=rownames(per.df))) + geom_point(aes(color=long), size=4) +
geom_text(hjust=0, vjust=0,size=4) + geom_vline(xintercept=0) + geom_hline(yintercept=0) +
scale_colour_gradient2(low="red", high="green", "250days\nPerformance") +
scale_y_continuous("Mid Performance: 20days", labels= percent ) +
scale_x_continuous("Short Performance: 5days", labels= percent )
z + theme(legend.background = element_rect(colour = "black"))
