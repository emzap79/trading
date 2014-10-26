# You can also merge data to view comparisons.
head(as.xts(merge(ORCL,IBM)))

funPointAndFigure(udlyg,tick)

# Technical Indicators
alligator(udlyg)

# Ichimoku
pars <- c(50,100,120)
x <- matrix(c(Hi(udlyg),Lo(udlyg),Cl(udlyg)),ncol=3)
colnames(x) <- c("High","Low","Close")
out <- ichimoku(x,pars)

# EMA's
EMA9 <- EMA(Cl(udlyg), n = 9); addTA(EMA9, on=1, lwd = 3, col = "red")
EMA20 <- EMA(Cl(udlyg), n = 20); addTA(EMA20, on=1, lwd = 3, col = "magenta")
EMA50 <- EMA(Cl(udlyg), n = 50); addTA(EMA50, on=1, lwd = 3, col = "cyan")

# William Percent
addWPR()

# Moving Average Convergence Divergence
addMACD

# Indicator                                         TTR Name        quantmod Name   {{{1
# Trend Indicator                                                                   {{{2
# Welles Wilder's Directional Movement Indicator    ADX             addADX
# Double Exponential Moving Average                 DEMA            addDEMA
# Exponential Moving Average                        EMA             addEMA
# Simple Moving Average                             SMA             addSMA
# Parabolic Stop and Reverse                        SAR             addSAR
# Exponential Volume Weighted Moving Average        EVWMA           addEVWMA
# Moving Average Convergence Divergence             MACD            addMACD
# Triple Smoothed Exponential Oscillator            TRIX            addTRIX
# Weighted Moving Average                           WMA             addWMA
# ZLEMA                                             ZLEMA           addZLEMA

# Volatility Indicator                                                              {{{2
# Average True Range                                ATR             addATR
# Bollinger Bands                                   BBands          addBBands
# Price Envelope N/A                                                addEnvelope

# Momentum Indicator                                                                {{{2
# Commodity Channel Index                           CCI             addCCI
# Chande Momentum Oscillator                        CMO             addCMO
# Detrended Price Oscillator                        DPO             addDPO
# momentum                                                          addMomentum
# Rate of Change                                    ROC             addROC
# Relative Strength Indicator                       RSI             addRSI
# Stocastic Momentum Index                          SMI             addSMI
# Williams %R                                       WPR             addWPR
#
# Volume Indicator                                                                  {{{2
# Chaiken Money Flow                                CMF             addCMF
# Volume N/A                                                        addVo

# This really just scratches the surface of what is possible with quantmod.
# For instance, see this post on using quantmod with gold related data.
