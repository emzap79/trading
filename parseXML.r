#####################################################################
#                   parsing webcontent to R                         #
#####################################################################
require(RCurl)
require(XML)

################################################
#  http://stackoverflow.com/a/1847346/3569509  #
################################################
# http://stackoverflow.com/a/1732454/3569509
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


################################################
# get stock-tickers
################################################
data <- xmlParse("http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.industry%20where%20id%20in%20%28select%20industry.id%20from%20yahoo.finance.sectors%29&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys")

#############################################################################
#  http://www.r-bloggers.com/r-and-the-web-for-beginners-part-ii-xml-in-r/  #
#############################################################################
# install and load the necessary package
library(XML)


# Save the URL of the xml file in a variable

xml.url <- "http://www.w3schools.com/xml/plant_catalog.xml"
# xml.url <- "http://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.industry%20where%20id%20in%20%28select%20industry.id%20from%20yahoo.finance.sectors%29&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
xml.url <-  xml.url.stocks

# Use the xmlTreePares-function to parse xml file directly from the web
xmlfile <- xmlTreeParse(xml.url)

# the xml file is now saved as an object you can easily work with in R:
class(xmlfile)

# Use the xmlRoot-function to access the top node
xmltop = xmlRoot(xmlfile)

# have a look at the XML-code of the first subnodes:
# print(xmltop)[1:2]

# To extract the XML-values from the document, use xmlSApply:
plantcat <- xmlSApply(xmltop, function(x) xmlSApply(x, xmlValue))

# Finally, get the data in a data-frame and have a look at the first rows and columns
plantcat_df <- data.frame(t(plantcat),row.names=NULL)
plantcat_df[1:5,1:4]
plantcat_df$industry.210
plantcat_df

#################################################
#  http://stackoverflow.com/a/11421395/3569509  #
#################################################
install.packages("qmao",
                 repos = c('http://rforge.net',
                           'http://R-Forge.R-project.org',
                           'http://cran.rstudio.org',
                           'http://cran.r-project.org'),
                 type = 'source')

library("pander")
library("shiny")
library("gdata")
library("qmao")
require(devtools)
install_github('gsee/qmao')

ec <- getEarningsCalendar(from="2011-01-01", to="2012-07-01") #this may take a while
s <- unique(ec$Symbol)
length(s)
#[1] 12223
head(s, 20) #look at the first 20 Symbols
