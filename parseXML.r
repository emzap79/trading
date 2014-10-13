#####################################################################
#                   parsing webcontent to R                         #
#####################################################################
# http://stackoverflow.com/a/1732454/3569509
require(RCurl)
require(XML)

################################################
#  http://stackoverflow.com/a/1847346/3569509  #
################################################
webpage <- getURL("http://www.haaretz.com")
webpage <- readLines(tc <- textConnection(webpage)); close(tc)
pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)
# parse the tree by tables
x <- xpathSApply(pagetree, "//*/table", xmlValue)
# do some clean up with regular expressions
x <- unlist(strsplit(x, "\n"))
x <- gsub("\t","",x)
x <- sub("^[[:space:]]*(.*?)[[:space:]]*$", "\\1", x, perl=TRUE)
x <- x[!(x %in% c("", "|"))]
x

#################################################
#  http://stackoverflow.com/a/17200181/3569509  #
#################################################
require(XML)
data <- xmlParse("http://forecast.weather.gov/MapClick.php?lat=29.803&lon=-82.411&FcstType=digitalDWML")
xml_data <- xmlToList(data)
xml_data
