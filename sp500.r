#vim:fdm=marker
## downloads historic prices for all constituents of SP500
library(zoo)
library(tseries)
library("tseries")

## read in list of constituents, with company name in first column and
## ticker symbol in second column
spComp <- read.csv("raw_data/sp500_constituents.csv" )

## specify time period
dateStart <- "1995-01-01"
dateEnd <- "2013-05-01"

## extract symbols and number of iterations
symbols <- spComp[, 2]
nAss <- length(symbols)

## download data on first stock as zoo object
z <- get.hist.quote(instrument = symbols[1], start = dateStart,
                    end = dateEnd, quote = "AdjClose",
                    retclass = "zoo", quiet = T)

## use ticker symbol as column name
dimnames(z)[[2]] <- as.character(symbols[1])

## download remaining assets in for loop
for (i in 2:nAss) {
   ## display progress by showing the current iteration step
   cat("Downloading ", i, " out of ", nAss , "\n")

   result <- try(x <- get.hist.quote(instrument = symbols[i],
                                     start = dateStart,
                                     end = dateEnd, quote = "AdjClose",
                                     retclass = "zoo", quiet = T))
   if(class(result) == "try-error") {
      next
   }
   else {
      dimnames(x)[[2]] <- as.character(symbols[i])

      ## merge with already downloaded data to get assets on same dates
      z <- merge(z, x)

   }


}

## save data
write.zoo(z, file = "data/all_sp500_price_data.csv", index.name = "time")
