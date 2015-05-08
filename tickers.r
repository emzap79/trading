# vim: fdm=marker
# http://www.ats.ucla.edu/stat/r/modules/raw_data.htm
symbs <- read.csv("tickers.txt",
                  header=T, skip=0,
                  sep=';', dec='.',
                  encoding='utf-8',
                  stringsAsFactors=F, fill=T,
                  blank.lines.skip=T,
                  na.strings=c(NA,"NA"," NA ","#N/A N/A"))
symbs

# choose ticker
# http://stackoverflow.com/a/7664655/3569509
toMatch <- c("Bilfinger","Lufthansa")
matches <- grep(paste(toMatch,collapse="|"), symbs$ps_alle_names, value=F, ignore.case=T)
cbind.na("^GDAXI",symbs$planspiel_alle[matches])

# write.table(symbs,file="tickers.csv",
#             append=F, sep=';', dec='.',
#             col.names=T, row.names=F,
#             fileEncoding='utf-8', eol="\n", na="NA")
