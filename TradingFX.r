## TrueFX
## http://www.r-bloggers.com/tfx-package/
install.packages(c('XML','bitops','TFX','shiny'), repos =
                 'http://cran.r-project.org', dependencies = 'Imports')
library("TFX")

# http://cran.r-project.org/web/packages/TFX/index.html
# http://rpubs.com/gsee/TFX
# https://gist.github.com/4122626
library('shiny')
runGist('4122626')

## Not run:
## Unauthenticated
QueryTrueFX()
QueryTrueFX(pretty=FALSE)

## Must have a TrueFX(tm) account to run the following (Membership is free)
## Replace JSTrader and Ou812 with your username and password, respectively
id <- ConnectTrueFX("EUR/USD,AUD/JPY", u='emZap', p='BqQvPQdw', f='html')
QueryTrueFX(id)

browseURL(paste0("http://webrates.truefx.com/rates/connect.html?id=", id))

#view the Web API Developer Guide:
TrueFXRef()
