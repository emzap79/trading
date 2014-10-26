alligator <- function (alg) {

    jaw <- SMA(Cl(alg),13)
    teeth <- SMA(Cl(alg),8)
    lips <- SMA(Cl(alg),5)
    jaw <- lag(jaw,8)
    teeth <- lag(teeth,5)
    lips <- lag(lips,3)
    c(addTA(jaw , on = 1 , col ='blue'),
      addTA(teeth , on = 1 , col ='red'),
      addTA(lips , on = 1 , col ='green')
      )
}

# funAlligator <- function (a) {
#     c(addTA(lag(SMA(Cl(a),13),8), on = 1),
#       addTA(lag(SMA(Cl(a),8),5), on = 1),
#       addTA(lag(SMA(Cl(a),5),3), on = 1)
#       )
# }
#
# alligator <- newTA(FUN = funAlligator, # chartSeries will pass whole dataset,
#               lty   = c("solid", "solid", "dotted"),
#               legend.name = "The Alligator",
#               col   = c("green", "red", "blue")
#               )

# chart_Series(udlyg ,subset="2014::")
# add_TA(lag(jaw,8) , on = 1 , col ='blue')
# add_TA(lag(teeth,5) , on = 1 , col ='red')
# add_TA(lag(lips,3) , on = 1 , col ='green')
