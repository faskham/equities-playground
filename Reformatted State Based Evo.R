


tech.XIV <- function(filepath, ticker) {
  
  tech <- read.csv(paste(filepath, ticker, ".csv", sep=""))
  tech$Date <- as.POSIXct(tech$Date)
  tech <- tech[order(tech$Date),]
  tech$ticker <- ticker
  
  tech$next.gain <- NA
  for (i in 1:(length(tech$Close) - 1)) {
    tech$next.gain[i] <- tech$Close[i + 1] - tech$Open[i+1]
    tech$next.gain.per[i] <- tech$next.gain[i] / tech$Open[i+1]
  }
 
  tech$next.gain.7 <- NA
  for (i in 1:(length(tech$Close) - 6)) {
    tech$next.gain.7[i] <- tech$Close[i + 7] - tech$Open[i+1]
    tech$next.gain.7.per[i] <- tech$next.gain.7[i] / tech$Open[i+1]
  }
 
  tech$next.gain.30 <- NA
  for (i in 1:(length(tech$Close) - 29)) {
    tech$next.gain.30[i] <- tech$Close[i + 30] - tech$Open[i+1]
    tech$next.gain.30.per[i] <- tech$next.gain.30[i] / tech$Open[i+1]
  }
 
  #Calculate High-Low, High-prevClose, and Low-prevClose#
  tech$hilo <- tech$High - tech$Low
  tech$loclose[1] <- NA
  tech$hiclose[1] <- NA
  for (i in 2:length(tech$Close)) {
    tech$loclose[i] <- abs(tech$Low[i] - tech$Close[i - 1])
  }
  for (i in 2:length(tech$Close)) {
    tech$hiclose[i] <- abs(tech$High[i] - tech$Close[i - 1])
  }
  
  #TR Calculation: Max of three calculations above#
  tech$tr[1] <- NA
  for (i in 2:length(tech$Close)) {
    tech$tr[i] <-
      max(tech$hilo[i], tech$hiclose[i], tech$loclose[i], na.rm = TRUE)
  }
  
  #ATR - 10 day moving average of TR#
  tech$atr[1] <- NA
  tech$atr[2:10] <- NA
  for (i in 11:length(tech$Close)) {
    tech$atr[i] <- mean(tech$tr[(i - 9):i])
  }
  print("here")
  
  #+DM 1 Calculation#
  tech$hidiff.1[1] <- NA
  tech$lodiff.1[1] <- NA
  tech$p.DM.1[1] <- NA
  
  for (i in 2:length(tech$Close)) {
    tech$hidiff.1[i] <- tech$High[i] - tech$High[i - 1]
  }
  
  for (i in 2:length(tech$Close)) {
    tech$lodiff.1[i] <- tech$Low[i - 1] - tech$Low[i]
  }
  
  for (i in 2:length(tech$Close)) {
    if (tech$hidiff.1[i] > tech$lodiff.1[i]) {
      tech$p.DM.1[i] <- max(tech$hidiff.1[i],0)
    } else {
      tech$p.DM.1[i] <- 0
    }
  }
  
  #-DM 1 Calculation#
  tech$n.DM.1[1] <- NA
  for (i in 2:length(tech$Close)) {
    if (tech$lodiff.1[i] > tech$hidiff.1[i]) {
      tech$n.DM.1[i] <- max(tech$lodiff.1[i],0)
    } else {
      tech$n.DM.1[i] <- 0
    }
  }
  
  #TR14 Calculation#
  tech$tr.14[1] <- NA
  tech$tr.14[1:14] <- c(rep(NA,14))
  tech$tr.14[15] <- sum(tech$tr[2:15],na.rm = TRUE)
  for (i in 16:length(tech$Close)) {
    tech$tr.14[i] <-
      (tech$tr.14[i - 1] - (tech$tr.14[i - 1] / 14) + tech$tr[i])
  }
  
  #+DM 14 Calculation#
  tech$p.DM.14[1] <- NA
  tech$p.DM.14[1:14] <- c(rep(NA,14))
  tech$p.DM.14[15] <- sum(tech$p.DM.1[2:15],na.rm = TRUE)
  for (i in 16:length(tech$Close)) {
    tech$p.DM.14[i] <-
      (tech$p.DM.14[i - 1] - (tech$p.DM.14[i - 1] / 14) + tech$p.DM.1[i])
  }
  
  #-DM 14 Calculation#
  tech$n.DM.14[1] <- NA
  tech$n.DM.14[1:14] <- c(rep(NA,14))
  tech$n.DM.14[15] <- sum(tech$n.DM.1[2:15],na.rm = TRUE)
  for (i in 16:length(tech$Close)) {
    tech$n.DM.14[i] <-
      (tech$n.DM.14[i - 1] - (tech$n.DM.14[i - 1] / 14) + tech$n.DM.1[i])
  }
  
  #+DI 14 Calculation#
  tech$p.DI.14[1] <- NA
  tech$p.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$p.DI.14[i] <- (100 * (tech$p.DM.14[i] / tech$tr.14[i]))
  }
  
  #-DI 14 Calculation#
  tech$n.DI.14[1] <- NA
  tech$n.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$n.DI.14[i] <- (100 * (tech$n.DM.14[i] / tech$tr.14[i]))
  }
  
  #DI 14 Diff Calculation#
  tech$diff.DI.14[1] <- NA
  tech$diff.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$diff.DI.14[i] <- abs(tech$p.DI.14[i] - tech$n.DI.14[i])
  }
  
  #DI 14 Sum Calculation#
  tech$sum.DI.14[1] <- NA
  tech$sum.DI.14[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$sum.DI.14[i] <- tech$p.DI.14[i] + tech$n.DI.14[i]
  }
  
  #DX Calculation#
  tech$DX[1] <- NA
  tech$DX[1:14] <- c(rep(NA,14))
  for (i in 15:length(tech$Close)) {
    tech$DX[i] <- (100 * (tech$diff.DI.14[i] / tech$sum.DI.14[i]))
  }
  
  #ADX Calculation#
  tech$ADX[1] <- NA
  tech$ADX[1:27] <- c(rep(NA,27))
  tech$ADX[28] <- mean(tech$DX[15:28])
  for (i in 29:length(tech$Close)) {
    tech$ADX[i] <- (((tech$ADX[i - 1] * 13) + tech$DX[i]) / 14)
  }
  
  #20 Day Moving Average#
  require(zoo)
  
  tech$MA.20[1] <- NA
  tech$MA.20[1:19] <- c(rep(NA,19))
  tech$MA.20[20:length(tech$Close)] <-
    rollmean(tech$Close, 20)
  
  #20 Day Moving STD#
  tech$STD.20[1] <- NA
  tech$STD.20[1:19] <- c(rep(NA,19))
  for (i in 20:length(tech$Close)) {
    tech$STD.20[i] <- sd(tech$Close[(i - 19):i]) * (sqrt(19 / 20))
  }
  
  #Bollinger Bands(20,2)#
  tech$up.Bol <- tech$MA.20 + (2 * tech$STD.20)
  tech$down.Bol <- tech$MA.20 - (2 * tech$STD.20)
  tech$Bollinger <- tech$up.Bol - tech$down.Bol
  
  #A-D Line#
  tech$MF.Mult <-
    ((tech$Close - tech$Low) - (tech$High - tech$Close)) / (tech$High - tech$Low)
  
  tech$MF.Value <- tech$MF.Mult * tech$Volume
  
  tech$ADLine[1] <- tech$MF.Value[1]
  for (i in 20:length(tech$Close)) {
    tech$ADLine[i] <- (tech$ADLine[i - 1] + tech$MF.Value[i])
  }
  print("here2")
  #CCI 20-Day#
  require(lsr)
  
  tech$Typical.Price <-
    (tech$High + tech$Low + tech$Close) / 3
  
  tech$SMA.20[1] <- NA
  tech$SMA.20[1:19] <- c(rep(NA,19))
  tech$SMA.20[20:length(tech$Close)] <-
    rollmean(tech$Typical.Price, 20)
  
  tech$MAD.20[1] <- NA
  tech$MAD.20[1:19] <- c(rep(NA,19))
  for (i in 20:length(tech$Close)) {
    tech$MAD.20[i] <- aad(tech$Typical.Price[(i - 19):i])
  }
  
  tech$CCI.20 <-
    (tech$Typical.Price - tech$SMA.20) / (.015 * tech$MAD.20)
  
  #CMF 20-Day#
  tech$CMF.20[1] <- NA
  tech$CMF.20[1:19] <- c(rep(NA,19))
  tech$CMF.20[20:length(tech$Close)] <-
    rollmean(tech$MF.Value, 20) / rollmean(tech$Volume, 20)
  
  #OBV Calculation#
  tech$OBV[1] <- 0
  for (i in 2:length(tech$Close)) {
    if (tech$Close[i] > tech$Close[i - 1]) {
      tech$OBV[i] <- (tech$OBV[i - 1] + tech$Volume[i])
    }
    if (tech$Close[i] < tech$Close[i - 1]) {
      tech$OBV[i] <- (tech$OBV[i - 1] - tech$Volume[i])
    }
    else {
      tech$OBV[i] <- tech$OBV[i - 1]
    }
  }
  ############  
  #Money Flow Index#
  tech$updown[1] <- NA
  for (i in 2:length(tech$Close)) {
    if (tech$Typical.Price[i] > tech$Typical.Price[i - 1]) {
      tech$updown[i] <- 1
    }
    else {
      tech$updown[i] <- -1
    }
  }
  
  tech$Raw.Flow <- tech$Typical.Price * tech$Volume
  
  tech$Pos.Flow.1[1] <- NA
  for (i in 2:length(tech$Close)) {
    if (tech$updown[i] > 0) {
      tech$Pos.Flow.1[i] <- tech$Raw.Flow[i]
    }
    else {
      tech$Pos.Flow.1[i] <- 0
    }
  }
  
  tech$Neg.Flow.1[1] <- NA
  for (i in 2:length(tech$Close)) {
    if (tech$updown[i] < 0) {
      tech$Neg.Flow.1[i] <- tech$Raw.Flow[i]
    }
    else {
      tech$Neg.Flow.1[i] <- 0
    }
  }
  
  tech$Pos.Flow.20[1] <- NA
  tech$Pos.Flow.20[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    tech$Pos.Flow.20[i] <- sum(tech$Pos.Flow.1[(i - 19):i])
  }
  
  tech$Neg.Flow.20[1] <- NA
  tech$Neg.Flow.20[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    tech$Neg.Flow.20[i] <- sum(tech$Neg.Flow.1[(i - 19):i])
  }
  
  tech$Ratio.Flow.20 <- tech$Pos.Flow.20 / tech$Neg.Flow.20
  
  tech$Index.Flow.20 <- (100 - (100 / (1 + tech$Ratio.Flow.20)))
  
  #Stochastic Oscillator 14-Day#
  tech$hi.14[1] <- NA
  tech$hi.14[2:13] <- NA
  for (i in 14:length(tech$Close)) {
    tech$hi.14[i] <- max(tech$High[(i - 13):i])
  }
  
  tech$lo.14[1] <- NA
  tech$lo.14[2:13] <- NA
  for (i in 14:length(tech$Close)) {
    tech$lo.14[i] <- min(tech$Low[(i - 13):i])
  }
  
  tech$lo.14[1] <- NA
  tech$lo.14[2:13] <- NA
  tech$Osc.14 <-
    ((tech$Close - tech$lo.14) / (tech$hi.14 - tech$lo.14)) * 100
  
  #Williams %R 14-Day#
  tech$Will.14 <-
    ((tech$hi.14 - tech$Close) / (tech$hi.14 - tech$lo.14)) * (-100)
  
  #EMA 12, 20, 26 Day#
  tech$EMA.9[1] <- NA
  tech$EMA.9[2:8] <- NA
  tech$EMA.9[9] <- mean(tech$Close[1:9])
  for (i in 10:length(tech$Close)) {
    tech$EMA.9[i] <-
      (((2 / 10) * (tech$Close[i] - tech$EMA.9[i - 1])) + tech$EMA.9[(i-1)])
  }
  
  tech$EMA.12[1] <- NA
  tech$EMA.12[2:11] <- NA
  tech$EMA.12[12] <- mean(tech$Close[1:12])
  for (i in 13:length(tech$Close)) {
    tech$EMA.12[i] <-
      (((2 / 13) * (tech$Close[i] - tech$EMA.12[i - 1])) + tech$EMA.12[(i-1)])
  }
  
  tech$EMA.20[1] <- NA
  tech$EMA.20[2:19] <- NA
  tech$EMA.20[20] <- mean(tech$Close[1:20])
  for (i in 21:length(tech$Close)) {
    tech$EMA.20[i] <-
      (((2 / 21) * (tech$Close[i] - tech$EMA.20[i - 1])) + tech$EMA.20[(i-1)])
  }
  
  tech$EMA.26[1] <- NA
  tech$EMA.26[2:25] <- NA
  tech$EMA.26[26] <- mean(tech$Close[1:26])
  for (i in 27:length(tech$Close)) {
    tech$EMA.26[i] <-
      (((2 / 27) * (tech$Close[i] - tech$EMA.26[i - 1])) + tech$EMA.26[(i-1)])
  }
  
  #KC Bands#
  tech$kc.upper <- (tech$EMA.20 + (2 * tech$atr))
  tech$kc.lower <- (tech$EMA.20 - (2 * tech$atr))
  tech$kc.width <- tech$kc.upper - tech$kc.lower
  
  #Bollinger/Keltner Comparison#
  tech$bb.kc[1] <- NA
  tech$bb.kc[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$up.Bol[i] < tech$kc.upper[i]) &
        (tech$down.Bol[i] > tech$kc.lower[i])) {
      tech$bb.kc[i] <- 1
    }
    else {
      tech$bb.kc[i] <- 0
    }
  }
  
  
  #CCI Cross 100 Up#
  tech$CCI.cross.up[1] <- NA
  tech$CCI.cross.up[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] > 100) &
        (tech$CCI.20[i - 1] < 100)) {
      tech$CCI.cross.up[i] <- 1
    }
    else {
      tech$CCI.cross.up[i] <- 0
    }
  }
  
  #CCI Cross neg 100 Up#
  tech$CCI.cross.neg.up[1] <- NA
  tech$CCI.cross.neg.up[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] > -100) &
        (tech$CCI.20[i - 1] < -100)) {
      tech$CCI.cross.neg.up[i] <- 1
    }
    else {
      tech$CCI.cross.neg.up[i] <- 0
    }
  }
  
  #CCI Cross 100 Down#
  tech$CCI.cross.down[1] <- NA
  tech$CCI.cross.down[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] < 100) &
        (tech$CCI.20[i - 1] > 100)) {
      tech$CCI.cross.down[i] <- 1
    }
    else {
      tech$CCI.cross.down[i] <- 0
    }
  }
  
  #CCI Cross 0 Down#
  tech$CCI.cross.0.down[1] <- NA
  tech$CCI.cross.0.down[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] < 0) &
        (tech$CCI.20[i - 1] > 0)) {
      tech$CCI.cross.0.down[i] <- 1
    }
    else {
      tech$CCI.cross.0.down[i] <- 0
    }
  }
  
  #CCI Cross negative 100 Down#
  tech$CCI.cross.neg.100.down[1] <- NA
  tech$CCI.cross.neg.100.down[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if ((tech$CCI.20[i] < -100) &
        (tech$CCI.20[i - 1] > -100)) {
      tech$CCI.cross.neg.100.down[i] <- 1
    }
    else {
      tech$CCI.cross.neg.100.down[i] <- 0
    }
  }
  
  #CCI Above#
  tech$CCI.above[1] <- NA
  tech$CCI.above[2:20] <- NA
  for (i in 21:length(tech$Close)) {
    if (tech$CCI.20[i] > 100) {
      tech$CCI.above[i] <- 1
    }
    else {
      tech$CCI.above[i] <- 0
    }
  }

  #DI Cross#
  tech$DI.cross.Up[1] <- NA
  tech$DI.cross.Up[2:15] <- NA
  for (i in 16:length(tech$Close)) {
    if ((tech$p.DI.14[i] > tech$n.DI.14[i]) &
        (tech$p.DI.14[(i - 1)] < tech$n.DI.14[(i - 1)])) {
      tech$DI.cross.Up[i] <- 1
    }
    else {
      tech$DI.cross.Up[i] <- 0
    }
  }
  
  #DI Cross down#
  tech$DI.cross.down[1] <- NA
  tech$DI.cross.down[2:15] <- NA
  for (i in 16:length(tech$Close)) {
    if ((tech$p.DI.14[i] < tech$n.DI.14[i]) &
        (tech$p.DI.14[(i - 1)] > tech$n.DI.14[(i - 1)])) {
      tech$DI.cross.down[i] <- 1
    }
    else {
      tech$DI.cross.down[i] <- 0
    }
  }
  
  #DI Above#
  tech$DI.above[1] <- NA
  tech$DI.above[2:15] <- NA
  for (i in 16:length(tech$Close)) {
    if (tech$p.DI.14[i] > tech$n.DI.14[i]) {
      tech$DI.above[i] <- 1
    }
    else {
      tech$DI.above[i] <- 0
    }
  }

  #MACD 14 Calculation#
  tech$MACD[1] <- NA
  tech$MACD[2:25] <- NA
  for (i in 26:length(tech$Close)) {tech$MACD[i] <- tech$EMA.12[i] - tech$EMA.26[i]}
  
  
  #MACD 17 Calculation#
  tech$MACD.17[1] <- NA
  tech$MACD.17[2:25] <- NA
  for (i in 26:length(tech$Close)) {tech$MACD.17[i] <- tech$EMA.9[i] - tech$EMA.26[i]}
  
  #MACD 14 Signal#
  tech$MACD.signal[1] <- NA
  tech$MACD.signal[2:33] <- NA
  tech$MACD.signal[34] <- mean(tech$MACD[26:34])
  for (i in 35:length(tech$Close)) {
    tech$MACD.signal[i] <-
      (((2 / 27) * (tech$MACD[i] - tech$MACD.signal[i - 1])) + tech$MACD.signal[(i-1)])
  }
  
  #MACD 17 Signal#
  tech$MACD.17.signal[1] <- NA
  tech$MACD.17.signal[2:33] <- NA
  tech$MACD.17.signal[34] <- mean(tech$MACD.17[26:34])
  for (i in 35:length(tech$Close)) {
    tech$MACD.17.signal[i] <-
      (((2 / 27) * (tech$MACD.17[i] - tech$MACD.17.signal[i - 1])) + tech$MACD.17.signal[(i-1)])
  }
  
  #MA 14 Cross Down#
  tech$MA.cross.down[1] <- NA
  tech$MA.cross.down[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if ((tech$MACD[i] > tech$MACD.signal[i]) &
        (tech$MACD[(i - 1)] < tech$MACD.signal[(i - 1)])) {
      tech$MA.cross.down[i] <- 1
    }
    else {
      tech$MA.cross.down[i] <- 0
    }
  }
  
  #MA 14 Cross Up#
  tech$MA.cross.up[1] <- NA
  tech$MA.cross.up[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if ((tech$MACD[i] < tech$MACD.signal[i]) &
        (tech$MACD[(i - 1)] > tech$MACD.signal[(i - 1)])) {
      tech$MA.cross.up[i] <- 1
    }
    else {
      tech$MA.cross.up[i] <- 0
    }
  }
  
  #MA 17 Cross Down#
  tech$MA.17.cross.down[1] <- NA
  tech$MA.17.cross.down[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if ((tech$MACD.17[i] > tech$MACD.17.signal[i]) &
        (tech$MACD.17[(i - 1)] < tech$MACD.17.signal[(i - 1)])) {
      tech$MA.17.cross.down[i] <- 1
    }
    else {
      tech$MA.17.cross.down[i] <- 0
    }
  }
  
  #MA 17 Cross Up#
  tech$MA.17.cross.up[1] <- NA
  tech$MA.17.cross.up[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if ((tech$MACD.17[i] < tech$MACD.17.signal[i]) &
        (tech$MACD.17[(i - 1)] > tech$MACD.17.signal[(i - 1)])) {
      tech$MA.17.cross.up[i] <- 1
    }
    else {
      tech$MA.17.cross.up[i] <- 0
    }
  }
  
  #MA 14 Above#
  tech$MA.above[1] <- NA
  tech$MA.above[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if (tech$MACD[i] > tech$MACD.signal[i]) {
      tech$MA.above[i] <- 1
    }
    else {
      tech$MA.above[i] <- 0
    }
  }
  
  #MA 14 Below#
  tech$MA.below[1] <- NA
  tech$MA.below[2:34] <- NA
  for (i in 35:length(tech$Close)) {
    if (tech$MACD[i] < tech$MACD.signal[i]) {
      tech$MA.below[i] <- 1
    }
    else {
      tech$MA.below[i] <- 0
    }
  }
  
  
  #CCI, DI, MA Cross 3#
  tech$CCI.cross.3[1] <- NA
  tech$CCI.cross.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$CCI.cross.3[i] <- tech$CCI.cross.up[i] + tech$CCI.cross.up[i-1] + tech$CCI.cross.up[i-2]}
  
  tech$DI.cross.3[1] <- NA
  tech$DI.cross.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$DI.cross.3[i] <- tech$DI.cross.Up[i] + tech$DI.cross.Up[i-1] + tech$DI.cross.Up[i-2]}
  
  tech$DI.cross.down.3[1] <- NA
  tech$DI.cross.down.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$DI.cross.down.3[i] <- tech$DI.cross.down[i] + tech$DI.cross.down[i-1] + tech$DI.cross.down[i-2]}
  
  tech$MA.cross.3[1] <- NA
  tech$MA.cross.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$MA.cross.3[i] <- tech$MA.cross.up[i] + tech$MA.cross.up[i-1] + tech$MA.cross.up[i-2]}
  
  tech$CCI.cross.neg.3[1] <- NA
  tech$CCI.cross.neg.3[2:22] <- NA
  for ( i in 23:length(tech$Close)) {
    tech$CCI.cross.neg.3[i] <- tech$CCI.cross.neg.100.down[i] + tech$CCI.cross.neg.100.down[i-1] + tech$CCI.cross.neg.100.down[i-2]}
  
  #Price KC#
  tech$price.kc[1] <- NA
  tech$price.kc[2:19] <- NA
  for (i in 20:length(tech$Close)) {
    if (tech$Close[i] > tech$kc.upper[i]) {tech$price.kc[i] <- 1} 
    else {tech$price.kc[i] <- 0}
  }
  
  #Price KC Short#
  tech$price.kc.short[1] <- NA
  tech$price.kc.short[2:19] <- NA
  for (i in 20:length(tech$Close)) {
    if (tech$Close[i] < tech$kc.lower[i]) {tech$price.kc.short[i] <- 1} 
    else {tech$price.kc.short[i] <- 0}
  }
  
  #Buy Triple Cross 3#
  tech$buy.three.cross[1] <- NA
  tech$buy.three.cross[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$bb.kc[i] == 1) & (tech$CCI.cross.3[i] == 1) & (tech$DI.cross.3[i] == 1) & (tech$MA.cross.3[i] == 1)) {
      tech$buy.three.cross[i] <- 1}
    else { tech$buy.three.cross[i] <- 0}
  }
  
  #Buy Roll DI Cross 3 (DI Roll = 6 Days)#
  tech$buy.roll.DI[1] <- NA
  tech$buy.roll.DI[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$bb.kc[i] == 1) & (tech$CCI.cross.3[i] == 1) & (tech$MA.cross.3[i] == 1) & (tech$DI.cross.3[i] == 1)
       & (tech$DI.cross.3[i-1] == 1) & (tech$DI.cross.3[i-2] == 1) & (tech$DI.cross.3[i-3] == 1)
       & (tech$DI.cross.3[i-4] == 1) & (tech$DI.cross.3[i-5] == 1)) {
      tech$buy.roll.DI[i] <- 1}
    else { tech$buy.roll.DI[i] <- 0}
  }
  
  #Buy MA Above#
  tech$buy.MA.above[1] <- NA
  tech$buy.MA.above[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$price.kc[i] == 1) & (tech$CCI.cross.3[i] == 1) & (tech$MA.above[i] == 1) & (tech$DI.cross.3[i] == 1)) {
      tech$buy.MA.above[i] <- 1}
    else { tech$buy.MA.above[i] <- 0}
  }
  
  #Short MA Above inverse#
  tech$short.MA.inverse[1] <- NA
  tech$short.MA.inverse[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$price.kc.short[i] == 1) & (tech$CCI.cross.neg.3[i] == 1) & (tech$MA.below[i] == 1) & (tech$DI.cross.down.3[i] == 1)) {
      tech$short.MA.inverse[i] <- 1}
    else { tech$short.MA.inverse[i] <- 0}
  }
  
  
  #Buy Triple Above#
  tech$buy.triple.above[1] <- NA
  tech$buy.triple.above[2:36] <- NA
  for (i in 37:length(tech$Close)) {
    if((tech$bb.kc[i] == 1) & (tech$CCI.above[i] == 1) & (tech$MA.above[i] == 1) & (tech$DI.above[i] == 1)) {
      tech$buy.triple.above[i] <- 1}
    else { tech$buy.triple.above[i] <- 0}
  }
  
  #Buy Triple Above Modified#
  tech$buy.triple.mod[1] <- NA
  tech$buy.triple.mod[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if((tech$buy.triple.above[i] == 1) & (tech$CCI.above[i-1] == 1) & (tech$CCI.above[i-2] == 1) & (tech$CCI.above[i-3] == 1)) {
      tech$buy.triple.mod[i] <- 1}
    else { tech$buy.triple.mod[i] <- 0}
  }
  
  #Sell on Cross Down#
  tech$sell.cross.down[1] <- NA
  tech$sell.cross.down[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if(((tech$MA.17.cross.down[i] == 1) || (tech$MA.17.cross.down[i-1] == 1) || (tech$MA.17.cross.down[i-2] == 1) || (tech$MA.17.cross.down[i-3] == 1) || (tech$MA.17.cross.down[i-4] == 1))
       & ((tech$bb.kc[i] == 1 || tech$bb.kc[i-1] == 1 || tech$bb.kc[i-2] == 1 || tech$bb.kc[i-3] == 1 || tech$bb.kc[i-4] == 1))
       & ( (tech$CCI.cross.down[i] == 1) ||  (tech$CCI.cross.down[i-1] == 1)   ||  (tech$CCI.cross.down[i-2] == 1) ||  (tech$CCI.cross.down[i-3] == 1) ||  (tech$CCI.cross.down[i-4] == 1)
           ||  (tech$CCI.cross.0.down[i] == 1) || (tech$CCI.cross.0.down[i-1] == 1) || (tech$CCI.cross.0.down[i-2] == 1) || (tech$CCI.cross.0.down[i-3] == 1) || (tech$CCI.cross.0.down[i-4] == 1))) {
      tech$sell.cross.down[i] <- 1}
    else { tech$sell.cross.down[i] <- 0}
  }
  
  
  #Cover on Cross Up#
  tech$cover.cross.up[1] <- NA
  tech$cover.cross.up[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if(((tech$MA.17.cross.up[i] == 1) || (tech$MA.17.cross.up[i-1] == 1) || (tech$MA.17.cross.up[i-2] == 1) || (tech$MA.17.cross.up[i-3] == 1) || (tech$MA.17.cross.up[i-4] == 1))
       & ((tech$bb.kc[i] == 1 || tech$bb.kc[i-1] == 1 || tech$bb.kc[i-2] == 1 || tech$bb.kc[i-3] == 1 || tech$bb.kc[i-4] == 1))
       & ( (tech$CCI.cross.neg.up[i] == 1) ||  (tech$CCI.cross.neg.up[i-1] == 1)   ||  (tech$CCI.cross.neg.up[i-2] == 1) ||  (tech$CCI.cross.neg.up[i-3] == 1) ||  (tech$CCI.cross.neg.up[i-4] == 1)
           ||  (tech$CCI.cross.neg.up[i-5] == 1) || (tech$CCI.cross.neg.up[i-6] == 1) || (tech$CCI.cross.neg.up[i-7] == 1) || (tech$CCI.cross.neg.up[i-8] == 1) || (tech$CCI.cross.neg.up[i-9] == 1))) {
      tech$cover.cross.up[i] <- 1}
    else { tech$cover.cross.up[i] <- 0}
  }
  
  #Independent Sell Signals#
  tech$sell.MA.cross.down[1] <- NA
  tech$sell.MA.cross.down[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if((tech$MA.17.cross.down[i] == 1) || (tech$MA.17.cross.down[i-1] == 1) || (tech$MA.17.cross.down[i-2] == 1) || (tech$MA.17.cross.down[i-3] == 1) || (tech$MA.17.cross.down[i-4] == 1)) {
      tech$sell.MA.cross.down[i] <- 1}
    else { tech$sell.MA.cross.down[i] <- 0}
  }
  
  tech$sell.bb.kc[1] <- NA
  tech$sell.bb.kc[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if((tech$bb.kc[i] == 1 || tech$bb.kc[i-1] == 1 || tech$bb.kc[i-2] == 1 || tech$bb.kc[i-3] == 1 || tech$bb.kc[i-4] == 1)) {
      tech$sell.bb.kc[i] <- 1}
    else { tech$sell.bb.kc[i] <- 0}
  }
  
  tech$sell.CCI.cross.down[1] <- NA
  tech$sell.CCI.cross.down[2:39] <- NA
  for (i in 40:length(tech$Close)) {
    if((tech$CCI.cross.down[i] == 1) ||  (tech$CCI.cross.down[i-1] == 1)   ||  (tech$CCI.cross.down[i-2] == 1) ||  (tech$CCI.cross.down[i-3] == 1) ||  (tech$CCI.cross.down[i-4] == 1)
       ||  (tech$CCI.cross.0.down[i] == 1) || (tech$CCI.cross.0.down[i-1] == 1) || (tech$CCI.cross.0.down[i-2] == 1) || (tech$CCI.cross.0.down[i-3] == 1) || (tech$CCI.cross.0.down[i-4] == 1)) {
      tech$sell.CCI.cross.down[i] <- 1}
    else { tech$sell.CCI.cross.down[i] <- 0}
  }
  
  #Holding Gain on Next Sell Cross Down#
  tech$holding.gain.sell.1[1] <- NA
  tech$holding.gain.sell.1[2:39] <- NA
  tech$holding.gain.sell.per[1] <- NA
  tech$holding.gain.sell.per[2:39] <- NA
  tech$holding.latency[1] <- NA
  tech$holding.latency[2:39] <- NA
  for( i in 40:length(tech$Close)) {
    if((tech$buy.three.cross[i] == 0) & (tech$buy.roll.DI[i] == 0) & (tech$buy.MA.above[i] == 0) & (tech$buy.triple.above[i] == 0) & (tech$buy.triple.mod[i] == 0)) {
      tech$holding.gain.sell.1[i] <- 0
    }
    
    else if(1 %in% tech$sell.cross.down[i:length(tech$Close)]) {
      tech$holding.gain.sell.1[i] <- tech$Open[(min(which(tech$sell.cross.down[i:length(tech$Close)] == 1)) + i)] - tech$Open[i+1]
      tech$holding.gain.sell.per[i] <- tech$holding.gain.sell.1[i]/tech$Open[i+1]
      tech$holding.latency[i] <- (min(which(tech$sell.cross.down[i:length(tech$Close)] == 1)))+1
    }
    
    else {tech$holding.gain.sell.1[i] <- NA}
  }
  
  #Holding Gain on Next Sell Cross Down#
  tech$holding.gain.short[1] <- NA
  tech$holding.gain.short[2:39] <- NA
  tech$holding.gain.short.per[1] <- NA
  tech$holding.gain.short.per[2:39] <- NA
  tech$holding.latency.short[1] <- NA
  tech$holding.latency.short[2:39] <- NA
  for( i in 40:length(tech$Close)) {
    if((tech$short.MA.inverse[i] == 0)) {
      tech$holding.gain.sell.1[i] <- 0
    }
    
    else if(1 %in% tech$cover.cross.up[i:length(tech$Close)]) {
      tech$holding.gain.short[i] <- (tech$Open[(min(which(tech$cover.cross.up[i:length(tech$Close)] == 1)) + i)] - tech$Open[i+1])*(-1)
      tech$holding.gain.short.per[i] <- tech$holding.gain.short[i]/tech$Open[i+1]
      tech$holding.latency.short[i] <- (min(which(tech$cover.cross.up[i:length(tech$Close)] == 1)))+1
    }
    
    else {tech$holding.gain.short[i] <- NA}
  }
  
  #demeaned variables Volume
  tech$dlog.volume.1[1] <- NA
  tech$dlog.volume.1[2] <- NA
  for (i in 3:length(tech$Close)) {
    tech$dlog.volume.1[i] <- log(tech$Volume[i]/tech$Volume[i-1])
  }
  tech$dlog.volume.2[1] <- NA
  tech$dlog.volume.2[2] <- NA
  for (i in 3:length(tech$Close)) {
    tech$dlog.volume.2[i] <- log(tech$Volume[i]/tech$Volume[i-2])
  }
  
  # ATR
  tech$dlog.ATR.1[1] <- NA
  tech$dlog.ATR.1[2:11] <- NA
  for (i in 12:length(tech$Close)) {
    tech$dlog.ATR.1[i] <- log(tech$atr[i]/tech$atr[i-1])
  }
  tech$dlog.ATR.2[1] <- NA
  tech$dlog.ATR.2[2:12] <- NA
  for (i in 13:length(tech$Close)) {
    tech$dlog.ATR.2[i] <- log(tech$atr[i]/tech$atr[i-2])
  }
  print("here3")
  # ADX
  tech$dlog.ADX.1[1] <- NA
  tech$dlog.ADX.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    tech$dlog.ADX.1[i] <- log(tech$ADX[i]/tech$ADX[i-1])
  }
  tech$dlog.ADX.2[1] <- NA
  tech$dlog.ADX.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    tech$dlog.ADX.2[i] <- log(tech$ADX[i]/tech$ADX[i-2])
  }
  
  # Bollinger
  tech$dlog.Bollinger.1[1] <- NA
  tech$dlog.Bollinger.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    tech$dlog.Bollinger.1[i] <- log(tech$Bollinger[i]/tech$Bollinger[i-1])
  }
  tech$dlog.Bollinger.2[1] <- NA
  tech$dlog.Bollinger.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    tech$dlog.Bollinger.2[i] <- log(tech$Bollinger[i]/tech$Bollinger[i-2])
  }
  
  # Typical Price
  tech$dlog.Typical.Price.1[1] <- NA
  tech$dlog.Typical.Price.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    tech$dlog.Typical.Price.1[i] <- log(tech$Typical.Price[i]/tech$Typical.Price[i-1])
  }
  tech$dlog.Typical.Price.2[1] <- NA
  tech$dlog.Typical.Price.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    tech$dlog.Typical.Price.2[i] <- log(tech$Typical.Price[i]/tech$Typical.Price[i-2])
  }
  
  # CCI 20
  tech$dlog.CCI.20.1[1] <- NA
  tech$dlog.CCI.20.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    if(tech$CCI.20[i] > 0 && tech$CCI.20[i-1] < 0) {
      tech$dlog.CCI.20.1[i] <- log((tech$CCI.20[i]+(tech$CCI.20[i-1]*-1))/(tech$CCI.20[i-1]*-1))
    }
    else if(tech$CCI.20[i] < 0 && tech$CCI.20[i-1] > 0) {
      tech$dlog.CCI.20.1[i] <- (log(((tech$CCI.20[i]*-1)+tech$CCI.20[i-1])/tech$CCI.20[i-1])*-1)
    }
    else {
      tech$dlog.CCI.20.1[i] <- log(tech$CCI.20[i]/tech$CCI.20[i-1])
    }
  }
  tech$dlog.CCI.20.2[1] <- NA
  tech$dlog.CCI.20.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    if(tech$CCI.20[i] > 0 && tech$CCI.20[i-2] < 0) {
      tech$dlog.CCI.20.2[i] <- log((tech$CCI.20[i]+(tech$CCI.20[i-2]*-1))/(tech$CCI.20[i-2]*-1))
    }
    else if(tech$CCI.20[i] < 0 && tech$CCI.20[i-2] > 0) {
      tech$dlog.CCI.20.2[i] <- (log(((tech$CCI.20[i]*-1)+tech$CCI.20[i-2])/tech$CCI.20[i-2])*-1)
    }
    else {
      tech$dlog.CCI.20.2[i] <- log(tech$CCI.20[i]/tech$CCI.20[i-2])
    }
  }
  
  # WILL 14
  tech$dlog.Will.14.1[1] <- NA
  tech$dlog.Will.14.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    if(tech$Will.14[i] > 0 && tech$Will.14[i-1] < 0) {
      tech$dlog.Will.14.1[i] <- log((tech$Will.14[i]+(tech$Will.14[i-1]*-1))/(tech$Will.14[i-1]*-1))
    }
    else if(tech$Will.14[i] < 0 && tech$Will.14[i-1] > 0) {
      tech$dlog.Will.14.1[i] <- (log(((tech$Will.14[i]*-1)+tech$Will.14[i-1])/tech$Will.14[i-1])*-1)
    }
    else {
      tech$dlog.Will.14.1[i] <- log(tech$Will.14[i]/tech$Will.14[i-1])
    }
  }
  tech$dlog.Will.14.2[1] <- NA
  tech$dlog.Will.14.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    if(tech$Will.14[i] > 0 && tech$Will.14[i-2] < 0) {
      tech$dlog.Will.14.2[i] <- log((tech$Will.14[i]+(tech$Will.14[i-2]*-1))/(tech$Will.14[i-2]*-1))
    }
    else if(tech$Will.14[i] < 0 && tech$Will.14[i-2] > 0) {
      tech$dlog.Will.14.2[i] <- (log(((tech$Will.14[i]*-1)+tech$Will.14[i-2])/tech$Will.14[i-2])*-1)
    }
    else {
      tech$dlog.Will.14.2[i] <- log(tech$Will.14[i]/tech$Will.14[i-2])
    }
  }
  
  # CMF 20
  tech$dlog.CMF.20.1[1] <- NA
  tech$dlog.CMF.20.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    if(tech$CMF.20[i] > 0 && tech$CMF.20[i-1] < 0) {
      tech$dlog.CMF.20.1[i] <- log((tech$CMF.20[i]+(tech$CMF.20[i-1]*-1))/(tech$CMF.20[i-1]*-1))
    }
    else if(tech$CMF.20[i] < 0 && tech$CMF.20[i-1] > 0) {
      tech$dlog.CMF.20.1[i] <- (log(((tech$CMF.20[i]*-1)+tech$CMF.20[i-1])/tech$CMF.20[i-1])*-1)
    }
    else {
      tech$dlog.CMF.20.1[i] <- log(tech$CMF.20[i]/tech$CMF.20[i-1])
    }
  }
  tech$dlog.CMF.20.2[1] <- NA
  tech$dlog.CMF.20.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    if(tech$CMF.20[i] > 0 && tech$CMF.20[i-2] < 0) {
      tech$dlog.CMF.20.2[i] <- log((tech$CMF.20[i]+(tech$CMF.20[i-2]*-1))/(tech$CMF.20[i-2]*-1))
    }
    else if(tech$CMF.20[i] < 0 && tech$CMF.20[i-2] > 0) {
      tech$dlog.CMF.20.2[i] <- (log(((tech$CMF.20[i]*-1)+tech$CMF.20[i-2])/tech$CMF.20[i-2])*-1)
    }
    else {
      tech$dlog.CMF.20.2[i] <- log(tech$CMF.20[i]/tech$CMF.20[i-2])
    }
  }
  
  # MAD 20
  tech$dlog.MAD.20.1[1] <- NA
  tech$dlog.MAD.20.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    if(tech$MAD.20[i] > 0 && tech$MAD.20[i-1] < 0) {
      tech$dlog.MAD.20.1[i] <- log((tech$MAD.20[i]+(tech$MAD.20[i-1]*-1))/(tech$MAD.20[i-1]*-1))
    }
    else if(tech$MAD.20[i] < 0 && tech$MAD.20[i-1] > 0) {
      tech$dlog.MAD.20.1[i] <- (log(((tech$MAD.20[i]*-1)+tech$MAD.20[i-1])/tech$MAD.20[i-1])*-1)
    }
    else {
      tech$dlog.MAD.20.1[i] <- log(tech$MAD.20[i]/tech$MAD.20[i-1])
    }
  }
  tech$dlog.MAD.20.2[1] <- NA
  tech$dlog.MAD.20.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    if(tech$MAD.20[i] > 0 && tech$MAD.20[i-2] < 0) {
      tech$dlog.MAD.20.2[i] <- log((tech$MAD.20[i]+(tech$MAD.20[i-2]*-1))/(tech$MAD.20[i-2]*-1))
    }
    else if(tech$MAD.20[i] < 0 && tech$MAD.20[i-2] > 0) {
      tech$dlog.MAD.20.2[i] <- (log(((tech$MAD.20[i]*-1)+tech$MAD.20[i-2])/tech$MAD.20[i-2])*-1)
    }
    else {
      tech$dlog.MAD.20.2[i] <- log(tech$MAD.20[i]/tech$MAD.20[i-2])
    }
  }
  print("here4")
  # Index Flow 20
  tech$dlog.Index.Flow.20.1[1] <- NA
  tech$dlog.Index.Flow.20.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    tech$dlog.Index.Flow.20.1[i] <- log(tech$Index.Flow.20[i]/tech$Index.Flow.20[i-1])
  }
  tech$dlog.Index.Flow.20.2[1] <- NA
  tech$dlog.Index.Flow.20.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    tech$dlog.Index.Flow.20.2[i] <- log(tech$Index.Flow.20[i]/tech$Index.Flow.20[i-2])
  }
  
  # EMA 9
  tech$dlog.EMA.9.1[1] <- NA
  tech$dlog.EMA.9.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    tech$dlog.EMA.9.1[i] <- log(tech$EMA.9[i]/tech$EMA.9[i-1])
  }
  tech$dlog.EMA.9.2[1] <- NA
  tech$dlog.EMA.9.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    tech$dlog.EMA.9.2[i] <- log(tech$EMA.9[i]/tech$EMA.9[i-2])
  }
  # kc.width
  tech$dlog.kc.width.1[1] <- NA
  tech$dlog.kc.width.1[2:30] <- NA
  for (i in 31:length(tech$Close)) {
    tech$dlog.kc.width.1[i] <- log(tech$kc.width[i]/tech$kc.width[i-1])
  }
  tech$dlog.kc.width.2[1] <- NA
  tech$dlog.kc.width.2[2:31] <- NA
  for (i in 32:length(tech$Close)) {
    tech$dlog.kc.width.2[i] <- log(tech$kc.width[i]/tech$kc.width[i-2])
  }
  # Ticker State
  for (i in 1:(length(tech$Close) - 5)) {
    tech$one.day[i] <- ifelse(tech$Open[i] >= tech$Open[i+1],-1,1)
    tech$two.day[i] <- ifelse(tech$Open[i+1] >= tech$Open[i+2],-1,1)
    tech$three.day[i] <- ifelse(tech$Open[i+2] >= tech$Open[i+3],-1,1)
    tech$four.day[i] <- ifelse(tech$Open[i+3] >= tech$Open[i+4],-1,1)
    tech$five.day[i] <- ifelse(tech$Open[i+4] >= tech$Open[i+5],-1,1)
    tech$five.trend[i] <- ifelse(tech$Open[i] >= tech$Open[i+5],-1,1)
  }
  
  tech$trend.state <- abs(tech$one.day+tech$two.day+tech$three.day+tech$four.day+tech$five.day)*tech$five.trend
  
  for (i in 1:(length(tech$Close) - 1)) {
    tech$daily.return[i] <- (tech$Open[i+1] - tech$Open[i])/tech$Open[i]}
  
  output.file = paste("C:\\Users\\faskham\\Desktop\\Metrics", ticker, ".csv", sep ="")

  write.csv(tech, output.file, row.names = FALSE)
  
  #return(tech)  ##for flow without file writes
}

setup.data <- function(filepath, ticker) {
  data <- read.csv(paste(filepath, "Metrics", ticker, ".csv", sep =""))
  data.subset <- subset(data, select = c(Date,Open,ticker,daily.return,bb.kc,CCI.cross.up,CCI.cross.neg.up,CCI.cross.down,CCI.cross.0.down,CCI.cross.neg.100.down,CCI.above,DI.cross.Up,
                                         DI.cross.down,DI.above,MA.cross.down,MA.cross.up,MA.17.cross.down,MA.17.cross.up,MA.above,MA.below,CCI.cross.3,
                                         DI.cross.3,DI.cross.down.3,MA.cross.3,CCI.cross.neg.3,price.kc,price.kc.short,buy.three.cross,buy.roll.DI,
                                         buy.MA.above,short.MA.inverse,buy.triple.above,buy.triple.mod,sell.cross.down,cover.cross.up,sell.MA.cross.down,
                                         sell.bb.kc,sell.CCI.cross.down,trend.state)
                        )
  data.subset <- na.omit(data.subset)
  data.subset$Date <- as.Date(data.subset$Date, "%Y-%m-%d")
  data.subset <- data.subset[order(data.subset$Date),]
  data.clean <- data.subset[1:(length(data.subset$Open) - 5),]
  output.file = paste("C:\\Users\\faskham\\Desktop\\Clean", ticker, ".csv", sep ="")
  write.csv(data.clean, output.file, row.names = FALSE)
  
  return(data.clean) ##for flow without file writes
}

#for purely random weights enter NA for weight Center #Weight.SD should be some positive integer 
random.mutations.gen <- function(number.tests, factors.needed, weight.sd, weight.center) {
  i = 1
  species <- 0
  species.count <- number.tests
  genes <- factors.needed
  weights = rbind.data.frame(rep(NA, factors.needed))
  weight.names <- NA
  genome <- NA
  if (length(weight.center[1,])>1) {
    fittest <- TRUE
    classes <- nrow(weight.center)
    class.count <- 1
  }
  else {
    fittest <- FALSE
    classes <- 0
    class.count <- 1}
  for (item in i:genes) {weight.names[item] <- paste("factor",item)}
  print(classes)
  print(fittest)
    repeat {  
      if (fittest == TRUE && classes >= class.count) {
        repeat{
          if(fittest==TRUE) {
          data <- rnorm(1, weight.center[class.count,i], weight.sd)
          genome[i] <- data
          i = i + 1
          }
          if (i == genes+1 && species == 0) {
            colnames(weights) <- weight.names
            weights <- rbind.data.frame(weights, genome)
            i <- 1
            species <- species + 1
          }
          if (i==genes+1 && species >= 0){
            weights <- rbind.data.frame(weights, genome)
            i <- 1
            species <- species + 1
          }
          if (species == species.count) {
            i <- 1
            species <- 0
            class.count <- class.count+1
            print(paste("weight",class.count))
            break
          }
        }
      }
    else if (fittest == FALSE || classes + 1 == class.count) {
      repeat{
        data <- rnorm(1, 0, weight.sd)
        genome[i] <- data
        i = i + 1
        if (i == genes+1 && species == 0) {
          colnames(weights) <- weight.names
          weights <- rbind.data.frame(weights, genome)
          i <- 1
          species <- species + 1
        }
        if (i==genes+1 && species >= 0){
          weights <- rbind.data.frame(weights, genome)
          i <- 1
          species <- species + 1
        }
        if (species == species.count) {
          class.count <- class.count+1
          fittest <- TRUE
          print(paste("weight",class.count))
          break
          }
        }
      }
    else {
      break
    }
  } 
  if (classes > 0) {
    weights <- weights[complete.cases(weights), ]
    weights <- rbind.data.frame(weights, weight.center)
    return(weights) 
  }
  else {
    weights <- weights[complete.cases(weights), ]  
    return(weights)
  }
}

random.genome <- function(number.tests, factors.needed, progress.regress.weight, genome.selection, creatures.per) {
  i = 1
  species <- 0
  species.count <- number.tests
  genes <- factors.needed
  weights = rbind.data.frame(rep(NA, factors.needed))
  weight.names <- NA
  genome <- NA
  if (length(genome.selection[1,])>1) {
    fittest <- TRUE
    classes <- nrow(genome.selection)
    class.count <- 1
  }
  else {
    fittest <- FALSE
    classes <- 0
    class.count <- 1}
  for (item in i:genes) {weight.names[item] <- paste("genome",item)}
    repeat {  
      if (fittest == TRUE && classes >= class.count) {
        repeat{
          if(fittest==TRUE) {
            on.off <- genome.selection[class.count,i]
            data <- rbinom(1,genome.selection[class.count,i],(1-progress.regress.weight))
            genome[i] <- data
            i = i + 1
          }
          if (i == genes+1 && species == 0) {
            colnames(weights) <- weight.names
            weights <- rbind.data.frame(weights, genome)
            i <- 1
            species <- species + 1
          }
          if (i==genes+1 && species >= 0){
            weights <- rbind.data.frame(weights, genome)
            i <- 1
            species <- species + 1
          }
          if (species == species.count) {
            i <- 1
            species <- 0
            class.count <- class.count+1
            print(paste("genome",class.count))
            break
          }
        }
      }
      else if (fittest == FALSE || classes + 1 == class.count) {
        repeat{
          data <- rbinom(1,1,.5)
          genome[i] <- data
          i = i + 1
          if (i == genes+1 && species == 0) {
            colnames(weights) <- weight.names
            weights <- rbind.data.frame(weights, genome)
            i <- 1
            species <- species + 1
          }
          if (i==genes+1 && species >= 0){
            weights <- rbind.data.frame(weights, genome)
            i <- 1
            species <- species + 1
          }
          if (species == species.count) {
            class.count <- class.count+1
            fittest <- TRUE
            print(paste("genome",class.count))
            break
          }
        }
      }
      else {
        break
      }
    }  
  if (classes > 0) {
    weights <- weights[complete.cases(weights), ]
    weights <- rbind.data.frame(weights, genome.selection)
    return(weights) 
  }
  else {
    weights <- weights[complete.cases(weights), ]  
    return(weights)
  } 
}

exposure.calc <- function(input.data, input.weights, input.genomes){
  lifespan <- nrow(input.data)
  species <- nrow(input.weights)
  factors <- length(input.weights)
  species.counter <- 1
  factors.counter <- 1
  life.counter <- 1
  base <- t(as.data.frame(rep(NA,lifespan)))
  base <- na.omit(base)
  score.table <- t(as.data.frame(rep(NA,lifespan)))
  colnames(score.table) <- as.character(input.data[,1])
  score.table <- na.omit(base)
  agg.score <- NA
  repeat {
    if(factors.counter <= factors){
      score.vector <- input.data[, factors.counter+4]*input.weights[species.counter,factors.counter]*input.genomes[species.counter,factors.counter]
      base <- rbind.data.frame(base, score.vector)
      factors.counter <- factors.counter + 1
    }
    if(factors.counter > factors && life.counter <= lifespan){
      agg.score <- append(agg.score, sum(base[,life.counter]),after = life.counter)
      life.counter <- life.counter+1
      agg.score <- na.omit(agg.score)
    }
    if(factors.counter > factors && life.counter > lifespan && species.counter <= species){
      score.table <- rbind.data.frame(score.table, agg.score)
      species.counter <- species.counter + 1
      print(paste("Exposure calculated for species", species.counter, "of", species))
      factors.counter <- 1
      life.counter <- 1
      base <- t(as.data.frame(rep(NA,lifespan)))
      base <- na.omit(base)
      agg.score <- NA
    }
    if(species.counter > species){
      output <- subset.data.frame(input.data, select = c(Date,Open,ticker,daily.return))
      output <- cbind.data.frame(output, t(score.table))
      return(output)
      break
    }
  }
}

select.fittest <- function(scoring.data, catastrophe.prob, selection.pool) {
  library(dplyr)
  species <- length(scoring.data)-4
  species.counter <- 1
  species.frame <- t(as.data.frame(c(NA,NA,NA)))
  species.frame <- na.omit(species.frame)
  repeat{
    if(species.counter <= species) {
      data <- scoring.data %>% filter(scoring.data[,(species.counter+4)] > 1000)
      avg.daily <- mean(data$daily.return)
      days.held <- nrow(data)
      sd.of.return <- sd(data$daily.return)
      if(days.held != 0 && !is.na(sd.of.return)){
        score <- (avg.daily*days.held)/sd.of.return
      }
      else if (days.held == 0  || is.na(sd.of.return)){
        score <- -99999999999
      }
      data2 <- scoring.data %>% filter(scoring.data[,(species.counter+4)] < -1000)
      avg.daily2 <- mean(data2$daily.return)
      days.held2 <- nrow(data2)
      sd.of.return2 <- sd(data2$daily.return)
      if(days.held2 != 0  && !is.na(sd.of.return2)){
        score2 <- (avg.daily2*days.held2)/sd.of.return2
      }
      else if (days.held2 == 0  || is.na(sd.of.return2)) {
        score2 <- -99999999999
      }
      if (score <= score2) {
        inverse.flag <- 1
        score <- score2
        avg.daily <- avg.daily2
        days.held <- days.held2
        sd.of.return <- sd.of.return2
      }
      else {
        inverse.flag <- 0
      }
      species.frame <- rbind.data.frame(species.frame,c(species.counter,inverse.flag,score,avg.daily,days.held,sd.of.return))
      species.counter <- species.counter+ 1
      print(paste("score calculated for species",species.counter,"of",species))
      catastrophe <- rbinom(1,1,catastrophe.prob)
    }
    if(species.counter > species && catastrophe == 1) {
      colnames(species.frame) <- c("species","inverse","score","daily return","days held","sd of return")
      print("meteor strike!")
      successfuls <- head(species.frame[order(species.frame$score,decreasing = TRUE),],selection.pool)
      successfuls2 <- successfuls %>% filter(successfuls$score > 0)
      return(successfuls)
      break
    }
    else if (species.counter > species && catastrophe == 0) {
      colnames(species.frame) <- c("species","inverse","score","daily return","days held","sd of return")
      successfuls <- species.frame %>% filter(species.frame$score > 0)
      print("happy little fucks")
      return(successfuls)
      break
    }
  }
}

survivor.breed.weights <- function(input.weights, survivors.frame, factors) {
  survivors <- nrow(survivors.frame)
  survivor.count <- 1
  base <- t(as.data.frame(rep(NA,factors)))
  base <- na.omit(base)
  repeat {
    if (survivors == 0) {
      output.weights <- as.data.frame(NA)
      return(output.weights)
      break
    }
    else if (survivors >= survivor.count) {
      if (survivors.frame[survivor.count,2] == 1) {
        output.weights <- input.weights[survivors.frame[survivor.count,1],] * -1
        base <- rbind.data.frame(base, output.weights)
        survivor.count <- survivor.count + 1
      }
      else {
        output.weights <- input.weights[survivors.frame[survivor.count,1],]
        base <- rbind.data.frame(base, output.weights)
        survivor.count <- survivor.count + 1  
      }
    }
    else if (survivors < survivor.count) {
      return(base)
      break
    }
  }
}

survivor.breed.genome <- function(input.genome, survivors.frame, factors) {
  survivors <- nrow(survivors.frame)
  survivor.count <- 1
  base <- t(as.data.frame(rep(NA,factors)))
  base <- na.omit(base)
  repeat {
    if (survivors == 0) {
      output.weights <- as.data.frame(NA)
      return(output.weights)
      break
    }
    else if (survivors >= survivor.count) {
      output.genome <- input.genome[survivors.frame[survivor.count,1],]
      base <- rbind.data.frame(base, output.genome)
      survivor.count <- survivor.count + 1
    }
    else if (survivors < survivor.count) {
      return(base)
      break
    }
  }
}

run.world <- function(file.path, ticker, generations.spanned, creature.per, weight.devi.int, weight.devi, progress.regress, cat.prob, cat.survive) {
  generation <- 1
  filepath_1 <- file.path
  ticker_1 <- ticker
  plot1 <- as.data.frame(0)
  plot1 <- cbind.data.frame(plot1,as.data.frame(0))
  colnames(plot1) <- c("Generation", "Score")
  repeat {
    if (generation == 1) {
      tech.XIV(filepath = filepath_1, ticker = ticker_1)
      test.data <- setup.data(filepath = filepath_1, ticker = ticker_1)
      factors.considered <- length(test.data)-5 #for the four input columns minus the state column
      test.weight <- random.mutations.gen(number.tests = creature.per, factors.needed=factors.considered, weight.sd=weight.devi.int, weight.center=as.data.frame(NA))
      test.genome <- random.genome(number.tests=creature.per, factors.needed=factors.considered, progress.regress.weight=progress.regress, genome.selection=as.data.frame(NA))
      test.score <- exposure.calc(input.data=test.data,input.weights = test.weight,input.genomes = test.genome)
      test.fittest <- select.fittest(scoring.data = test.score, catastrophe.prob = cat.prob, selection.pool = cat.survive)
      fit.weight <- survivor.breed.weights(input.weights = test.weight, survivors.frame = test.fittest, factors = factors.considered)
      fit.genome <- survivor.breed.genome(input.genome = test.genome, survivors.frame = test.fittest, factors = factors.considered)
      plot2 <- as.data.frame(rep(generation,nrow(test.fittest)))
      plot2 <- cbind.data.frame(plot2, test.fittest$score)
      colnames(plot2) <- c("Generation", "Score")
      plot1 <- rbind.data.frame(plot1, plot2)
      generation <- generation + 1
      print(generation)
      }
    else if (generation <= generations.spanned) {
      print("cleared initial Generation")
      test.weight.2 <- random.mutations.gen(number.tests = creature.per, factors.needed=factors.considered, weight.sd=weight.devi, weight.center=fit.weight)
      test.genome.2 <- random.genome(number.tests=creature.per, factors.needed=factors.considered, progress.regress.weight=progress.regress, genome.selection=fit.genome)
      test.score <- exposure.calc(input.data=test.data,input.weights = test.weight.2, input.genomes = test.genome.2)
      test.fittest <- select.fittest(scoring.data = test.score, catastrophe.prob = cat.prob, selection.pool = cat.survive)
      fit.weight <- survivor.breed.weights(input.weights = test.weight.2, survivors.frame = test.fittest, factors = factors.considered)
      fit.genome <- survivor.breed.genome(input.genome = test.genome.2, survivors.frame = test.fittest, factors = factors.considered)
      plot2 <- as.data.frame(rep(generation,nrow(test.fittest)))
      plot2 <- cbind.data.frame(plot2, test.fittest$score)
      colnames(plot2) <- c("Generation", "Score")
      plot1 <- rbind.data.frame(plot1, plot2)
      generation <- generation + 1
      print(generation)
    }
    else if (generation > generations.spanned) {
      return(plot1)
      break
    }
  }
}


  
data2 <- run.world(file.path = "C:\\Users\\faskham\\Desktop\\", ticker = "ROKU", generations.spanned = 10, creature.per = 10, weight.devi.int = 800, weight.devi = 100, progress.regress = .1, cat.prob = .5, cat.survive = 10)
