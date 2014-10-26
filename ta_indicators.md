# Datos sobre los Indicadores y el analisis tecnico en Ava.

# Indicadores en Charts

## *Accelerator/Decelerator (AC): *
- Mide Activacion o Desactivacion de fuerzas en el mercado
    - Sobre cero y tiene dos lineas verdes: BUY
    - Bajo cero y tiene dos lines rojas: SELL

## *Average Directional Movement (ADX):*
- Mide Fortaleza de un Trend, no direccion por si solo, es parte de Direcc. Mov. Indic. Syst.
- Es derivacion de dos *Direction Momentum Indicators (DI)*, que miden la direccion del Trend.
- ADX va de 0 a 100 (<20 fragil cambiando direccion, >40 fuerte puede cambiar direccion)
    - +DI (azul)>-DI (rojo): BUY
    - +DI (azul)<-DI (rojo): SELL

## *Alligator (Cocodrilo):*
- Mide presencia / ausencia de Trends, y la direccion del Trend. se suele usar conjuntamente con Eliot waves.
- 3 Moving Averages (MA): Linea azul (Alligator jaw), Linea roja (Alligator teeth), y la Linea verde (Alligator lips).
- 3 MA cruzados, cocodrilo duerme, mercado flat, cuando mas largo, mas de va a mover luego el mercado.
- 3 MA no cruzados y el precio de mercado esta por arriba de los MA: uptrend market, despierto y a la caza.
- 3 MA no cruzados y el precio de mercado esta por debajo de los MA: downtrend market, despierto y a la caza.

## *Average True Range (ATR): *
- Mide volatibilidad del mercado (en terminos absolutos), usualmente 14 dias promedio del TR, comparando H y L.
    - Cambios grandes y rapidos en precio de mercado, ATR es alto.
    - Mercado estable, ATR no es alto.
- Antes de un rise/fall significativo en el precio del mercado: el ATR es low/high.

## *Awesome Oscillator (AO):*
- Se usa mayoritariamente cuando no se puede establecer un Trend claramente.
- the saucer (reversed saucer) + dos Pikes
    - Saucer formed > cero line o cruzando de (-) a (+) o 2 pikes < cero line: BUY
    - Saucer formed < cero line o cruzando de (+) a (-) o 2 pikes > cero line: SELL

## *Bollinger Bands (BB):*
- Mide Volatilidad y Nivel relativo de precios sobre un periodo de tiempo. BB se ajustan a las condiciones del mercado.
- Mercado muy volatil: las BB se separan; Mercado menos volatil: las BB se acercan.
- BB se acercan: se espera cambio rotundo de precio
- Si el precio toca del borde de BB: el trend va a seguir
- Movimiento de precio originado en un borde de BB va a ir hacia el otro borde de BB.

## *Commodity Channel Index (CCI):*
- Mide overbought o oversold markets
    - Oversold (BUY) CCI <-100, cuando vuelven a entrar.
    - Overbought (SELL) CCI > +100, cuando vuelven a entrar.

## *DeMarker (Demarker, De Marker, DeM):*
- Price exhaustion y potenciales minimos y maximos del precio de mercado. fluctua entre cero y uno.
- Se utiliza para identificar risk levels  antes de abrir posiciones.
    - Indicator < 0.3: bullish price reversal se espera
    - Indicator > 0.7: bearish price reversal  se espera

## *Envelopes (Moving Average Envelope, Trading Bands):*
- Consiste de dos moving averages MA, uno por arriva y otro por abajo del precio (como dos limites)
- La alta volatilidad agranda el porcentaje de desviacion.
- NO predice, identifica trends, por que usa valores historicos.
    - Precio llega al limite inferior (piso) y dobla: BUY
    - Precio llega al limite superior (techo) y dobla: SELL

## *Fractals:*
- Detecta el limite inferior y superior del Trend actual, y puede predecir cambios de direccion en el trend.
- Cambios direccion trend, conjuntamente con MA, Fibonacci Retracement, and Alligator.

## *Gator Oscillator:*
- Se basa en Alligator indicator, demuestra el grado de divergence/convergence de smoothed Moving Averages.
- Ayuda a visualizar y localizar la ausencia o presencia de trends
- Visualisa acercamiento y cruce de smoothed Moving Averages (balance lines).

## *Linear Regression:*
- Metodo estadistico para seguir trends, resultados similares a Mov. Ave.(MA), pero en comparacion con el mismo tiene un tiempo de respuesta mayor a MA y y responde mejor a cambio de precios.
    - La direccion de LR marca si bullish or bearish trend.
    - El cruce de precio y de LR confirma el cambio en el trend.

## *Moving Average Convergence/Divergence (MACD):*
- Indicador que sigue el Trend (trend-following indicator). consiste de un 12-period Exponential Moving Average (EMA, green), un 26-period Exponential Moving Average (navy), y un bar chart (red) que muestra la diferencia entre ellos. MACD se usa cuando los precios varian en el price corridor.
- crossovers: MACD < signal line, which is a bearish signal (signal to sell). MACD > signal line, it is a bullish signal, (signal to buy).
- overbought/oversold: MA cambia MUCHO con respecto a longer Moving Average, va a tender a volver al promedio.
- divergences: cuando el precio varia mucho de MACD, quiere decir que el trend llego al final.
- MACD > signal line: BUY; MACD < signal line: SELL; MACD crossings over the zero line: Buy or Sell signo.

## *Momentum:*
- Como MACD tambien es trend-following indicator, compara cambios en el precio de cierre de hoy contra N dias atras.
    - Cuando el indicador toca fondo y dobla: BUY
    - Cuando el indicador llega al tope y dobla: SELL

## *Moving Average (MA):*
- Promedio de precios en un periodo determinado.
- Hay cuatro tipos de MA:  Simple MA, Exponential MA, Smoothed MA, and Weighted MA.
- Se usan para definir support and resistance, para enfatizar direccion de trend, y para smooth out price y volume fluctuations.
- La direccion del indicador muestra si bullish o bearish trend. El cruce de MA y Precio confirma el cambio en Trend.
    - Price > MA: BUY
    - Price < MA: SELL

## *Parabolic SAR (stop-and-reversal):*
- Analiza trending markets, define los Trends, tiene mucho en comun con MA, pero a diferencia de MA, se mueve con mas aceleracion y puede cambiar su posicion con respecto a niveles de precio
- Muy bueno para ver puntos de exit points. creacion de closing orders y trailing stop orders
    - Parabola SAR < Precio: bullish Short positions should be closed )
    - Parabola SAR > Price: bearish (Long positions should be closed).
- cada punto en SAR muestra stop-order para el actual precio pero NO para el proximo.

## *Relative Strength Index (RSI):*
- Oscilador que sigue el precio, va de 0 a 100, hay tres zonas basicas, 
    - the upper (overbought / getting overvalued ) zone-70 to 100-
    - the lower (oversold / getting undervalued) zone-0 to 30-
    - the middle zone-30 to 70-.
- Se usa principalmente para DAY TRADING, para determinar el estado del mercado.
    - Viene de the upper zone (overbought, > 70) entra a the middle zone, same direction.
    - Viene de the lower zone (oversold, < 30) y entra a the middle zone, same direction.
    - RSI peaks in the overbought zone: SELL
    - RSI peaks in the oversold zone: BUY

## *Standard Deviation:*
- Medicion estadistica de Volatilidad, se debe usar como parte de otros indicadores.
- SD High: prices change sharply; SD Low: prices are stable ; antes de cambios significativos, es LOW por lo general

## *Stochastic **oscillator**:*
- es un oscillator que me dice si el precio de cierre esta relativamente acorde al rango de precios para un determinado periodo.
- consiste de dos lines: la principal %K (green) y la secundaria %D (navy). se mide en una escala de scale from 0% to 100%.
- cuando lines (%D and %K) of the Stochastic < 20% lower zone y luego entra a middle zone, el rate lo va a seguir.
- cuando lines (%D and %K) of the Stochastic > 80% upper zone y luego entra a middle zone, el rate lo va a seguir.

## *Williams Percent Range (Williams %R, or %R):*
- es un momentum indicator que mide overbought/oversold levels, entre 0% y-100%.
    - 0% the closing price = period high. Conversely,
    --100% the closing price = identical to the period low.
    - 0 to-20% range: the market is overbought; this can be treated as a signal to sell
    --80 to-100% range: the market is oversold; this can be treated as a signal to buy
- las zonas en el grafico van al revez de SRI...

 *Mariano Guevara*, MBA
*Gerente Regional America Latina & Espana*
