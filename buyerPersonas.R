#install.packages('stats')
#install.packages('xlsx')
library(stats)
library(xlsx)

setwd("H:/Andy Mueller/!Projects/Buyer Persona")

###prepare data###
#segmentData <- read.csv('dataFrame.csv') ###very large file!###

#metalSegments
goldPurch <- subset(segmentData, percentGoldLines > 0 & percentGoldLines < 1)
silverPurch <- subset(segmentData, percentSilverLines > 0 & percentSilverLines < 1)
platPurch <- subset(segmentData, percentPlatLines > 0 & percentPlatLines < 1)
pallPurch <- subset(segmentData, percentPallLines > 0 & percentPallLines < 1)
#productSegments
bullionPurch <- subset(segmentData, percentBullionLines > 0 & percentBullionLines < 1)
semiPurch <- subset(segmentData, percentSemiLines> 0 & percentSemiLines < 1)
numiPurch <- subset(segmentData, percentNumiLines > 0 & percentNumiLines < 1)
#bullionSegments
coinPurch <- subset(segmentData, percentCoinLines > 0 & percentCoinLines < 1)
barPurch <- subset(segmentData, percentBarLines> 0 & percentBarLines < 1)
roundPurch <- subset(segmentData, percentRoundLines > 0 & percentRoundLines < 1)

###summary###
#metal
summary(goldPurch$percentGoldLines)
summary(silverPurch$percentSilverLines)
summary(platPurch$percentPlatLines)
summary(pallPurch$percentPallLines)

#product
summary(bullionPurch$percentBullionLines)
summary(semiPurch$percentSemiLines)
summary(numiPurch$percentNumiLines)

#bullion
summary(coinPurch$percentCoinLines)
summary(barPurch$percentBarLines)
summary(roundPurch$percentRoundLines)

###hist###
#metal
par(mfrow=c(4,1)) 
hist(goldPurch$percentGoldLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Gold Line Items Histogram',
     col = '#A58329')
hist(silverPurch$percentSilverLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Silver Line Items Histogram',
     col = '#8E9090')
hist(platPurch$percentPlatLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Platinum Line Items Histogram',
     col = '#008FBE')
hist(pallPurch$percentPallLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Pall Line Items Histogram',
     col = '#24A186')

#product
par(mfrow=c(3,1)) 
hist(bullionPurch$percentBullionLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Bullion Line Items Histogram',
     col = '#002539')
hist(semiPurch$percentSemiLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Semi Line Items Histogram',
     col = '#24A186')
hist(numiPurch$percentNumiLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Numi Line Items Histogram',
     col = '#A94442')

#bullion
par(mfrow=c(3,1)) 
hist(coinPurch$percentCoinLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Coin Line Items Histogram',
     col = '#F98D29')
hist(barPurch$percentBarLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Bar Line Items Histogram',
     col = '#6CC049')
hist(roundPurch$percentRoundLines,
     xlim = c(0,1),
     breaks = 5,
     main = '%Round Line Items Histogram',
     col = '#FCB424')

###boxplot###
#metal
par(mfrow=c(4,1))
boxplot(goldPurch$percentGoldLines, col = '#A58329', horizontal = TRUE, xaxt = 'n', 
        main = '%Gold Line Items Boxplot')
        axis(side = 1, at = fivenum(goldPurch$percentGoldLines), labels = TRUE, las = 2)
        text(fivenum(goldPurch$percentGoldLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))
boxplot(silverPurch$percentSilverLines, col = '#8E9090', horizontal = TRUE, xaxt = 'n', 
        main = '%Silver Line Items Boxplot')
        axis(side = 1, at = fivenum(silverPurch$percentSilverLines), labels = TRUE, las = 2)
        text(fivenum(silverPurch$percentSilverLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))
boxplot(platPurch$percentPlatLines, col = '#008FBE', horizontal = TRUE, xaxt = 'n', 
        main = '%Platinum Line Items Boxplot')
        axis(side = 1, at = fivenum(platPurch$percentPlatLines), labels = TRUE, las = 2)
        text(fivenum(platPurch$percentPlatLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))  
boxplot(pallPurch$percentPallLines, col = '#24A186', horizontal = TRUE, xaxt = 'n', 
        main = '%Palladium Line Items Boxplot')
        axis(side = 1, at = fivenum(pallPurch$percentPallLines), labels = TRUE, las = 2)
        text(fivenum(pallPurch$percentPallLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))

#product
par(mfrow=c(3,1))        
boxplot(bullionPurch$percentBullionLines, col = '#002539', horizontal = TRUE, xaxt = 'n', 
        main = '%Bullion Line Items Boxplot')
        axis(side = 1, at = fivenum(bullionPurch$percentBullionLines), labels = TRUE, las = 2)
        text(fivenum(bullionPurch$percentBullionLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))
boxplot(semiPurch$percentSemiLines, col = '#24A186', horizontal = TRUE, xaxt = 'n', 
        main = '%Semi Line Items Boxplot')
        axis(side = 1, at = fivenum(semiPurch$percentSemiLines), labels = TRUE, las = 2)
        text(fivenum(semiPurch$percentSemiLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))
boxplot(numiPurch$percentNumiLines, col = '#A94442', horizontal = TRUE, xaxt = 'n', 
        main = '%Numi Line Items Boxplot')
        axis(side = 1, at = fivenum(numiPurch$percentNumiLines), labels = TRUE, las = 2)
        text(fivenum(numiPurch$percentNumiLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))

#bullion        
par(mfrow=c(3,1))        
boxplot(coinPurch$percentCoinLines, col = '#F98D29', horizontal = TRUE, xaxt = 'n', 
        main = '%Coin Line Items Boxplot')
        axis(side = 1, at = fivenum(coinPurch$percentCoinLines), labels = TRUE, las = 2)
        text(fivenum(coinPurch$percentCoinLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))
boxplot(barPurch$percentBarLines, col = '#6CC049', horizontal = TRUE, xaxt = 'n', 
        main = '%Bar Line Items Boxplot')
        axis(side = 1, at = fivenum(barPurch$percentBarLines), labels = TRUE, las = 2)
        text(fivenum(semiPurch$percentBarLines), rep(1.2, 4), srt = 90, adj = 0,
        labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))
boxplot(roundPurch$percentRoundLines, col = '#FCB424', horizontal = TRUE, xaxt = 'n', 
         main = '%Round Line Items Boxplot')
        axis(side = 1, at = fivenum(roundPurch$percentRoundLines), labels = TRUE, las = 2)
        text(fivenum(roundPurch$percentRoundLines), rep(1.2, 4), srt = 90, adj = 0,
         labels = c('Min', 'Lower', 'Median', 'Upper', 'Max'))        
        
###cut offs###
goldCO <- .65
silverCO <- .84
platCO <- .40
pallCO <- .34
        
bullionCO <- .78 
semiCO <- .62
numiCO <- .53
        
coinCO <- .81
barCO <- .66
roundCO <- .55
        
#metal
goldMean <- mean(goldPurch$percentGoldLines)
silverMean <- mean(silverPurch$percentSilverLines)
platMean <- mean(platPurch$percentPlatLines)
pallMean <- mean(pallPurch$percentPallLines)
goldSD <- sd(goldPurch$percentGoldLines)
silverSD <- sd(silverPurch$percentSilverLines)
platSD<- sd(platPurch$percentPlatLines)
pallSD <- sd(pallPurch$percentPallLines)

pnorm(goldCO, goldMean, goldSD)
pnorm(silverCO, silverMean, silverSD)
pnorm(platCO, platMean, platSD)
pnorm(pallCO, pallMean, pallSD)
        
#product
bullionMean <- mean(bullionPurch$percentBullionLines)
semiMean <- mean(semiPurch$percentSemiLines)
numiMean <- mean(numiPurch$percentNumiLines)
bullionSD <- sd(bullionPurch$percentBullionLines)
semiSD <- sd(semiPurch$percentSemiLines)
numiSD <- sd(numiPurch$percentNumiLines)

pnorm(bullionCO, bullionMean, bullionSD)
pnorm(semiCO, semiMean, semiSD)
pnorm(numiCO, numiMean, numiSD)
        
#bullion
coinMean <- mean(coinPurch$percentCoinLines)
barMean <- mean(barPurch$percentBarLines)
roundMean <- mean(roundPurch$percentRoundLines)
coinSD <- sd(coinPurch$percentCoinLines)
barSD <- sd(barPurch$percentBarLines)
roundSD <- sd(roundPurch$percentRoundLines)

pnorm(coinCO, coinMean, coinSD)
pnorm(barCO, barMean, barSD)
pnorm(roundCO, roundMean, roundSD)
        
###dist###
x <- seq(.01,.99, by = .001)

#metal
par(mfrow=c(4,1))         
goldPDF <- dnorm(x, goldMean, goldSD)
plot(x, goldPDF,
     main = '%Gold Line Items PDF',
     xlab = '% Line Items Gold',
     type = 'l',
     col = '#A58329')
     abline(v = goldCO, col = 'black')
silverPDF <- dnorm(x, silverMean, silverSD)
plot(x, silverPDF,
     main = '%Silver Line Items PDF',
     xlab = '% Line Items Silver',
     type = 'l',
     col = '#8E9090')
     abline(v = silverCO, col = 'black')
platPDF <- dnorm(x, platMean, platSD)
plot(x, platPDF,
     main = '%Platinum Line Items PDF',
     xlab = '% Line Items Platinum',
     type = 'l',
     col = '#008FBE')
     abline(v = platCO, col = 'black')
pallPDF <- dnorm(x, pallMean, pallSD)
plot(x, pallPDF,
     main = '%Palladium Line Items PDF',
     xlab = '% Line Items Palladium',
     type = 'l',
     col = '#24A186')
     abline(v = pallCO, col = 'black')    
#product
par(mfrow=c(3,1))   
bullionPDF <- dnorm(x, bullionMean, bullionSD)
plot(x, bullionPDF,
     main = '%Bullion Line Items PDF',
     xlab = '% Line Items bullion',
     type = 'l',
     col = '#002539')
     abline(v = bullionCO, col = 'black')  
semiPDF <- dnorm(x, semiMean, semiSD)
plot(x, semiPDF,
     main = '%Semi Line Items PDF',
     xlab = '%Line Items Semi',
     type = 'l',
     col = '#24A186')
     abline(v = semiCO, col = 'black')
numiPDF <- dnorm(x, numiMean, numiSD)
plot(x, numiPDF,
     main = '%Numi Line Items PDF',
     xlab = '%Line Items Numi',
     type = 'l',
     col = '#A94442')
     abline(v = numiCO, col = 'black')
#bullion
par(mfrow=c(3,1))   
coinPDF <- dnorm(x, coinMean, coinSD)
plot(x, coinPDF,
     main = '%Coin Line Items PDF',
     xlab = '% Line Items Coin',
     type = 'l',
     col = '#F98D29')
     abline(v = coinCO, col = 'black')
barMean <- mean(barPurch$percentBarLines)
barSD <- sd(barPurch$percentBarLines)   
barPDF <- dnorm(x, barMean, barSD)
plot(x, barPDF,
     main = '%Bar Line Items PDF',
     xlab = '%Line Items Bar',
     type = 'l',
     col = '#6CC049')
     abline(v = barCO, col = 'black')
roundPDF <- dnorm(x, roundMean, roundSD)
plot(x, roundPDF,
     main = '%Round Line Items PDF',
     xlab = '%Line Items Round',
     type = 'l',
     col = 'black')
     abline(v = roundCO, col = 'black')

###apply to dataset###
segmentData$metalPersona <- ifelse(segmentData$percentGoldLines >= goldCO, 'goldBuyer',
                            ifelse(segmentData$percentSilverLines >= silverCO, 'silverBuyer',
                            ifelse(segmentData$percentPlatLines >= platCO, 'platBuyer',
                            ifelse(segmentData$percentPallLines >= pallCO, 'pallBuyer', "otherBuyer"))))

segmentData$productPersona <- ifelse(segmentData$percentBullionLines >= bullionCO, 'bullionBuyer',
                              ifelse(segmentData$percentSemiLines >= semiCO, 'semiBuyer',
                              ifelse(segmentData$percentNumiLines >= numiCO, 'numiBuyer', "otherBuyer")))

segmentData$bullionPersona <- ifelse(segmentData$percentCoinLines >= coinCO, 'coinBuyer',
                              ifelse(segmentData$percentBarLines >= barCO, 'barBuyer',
                              ifelse(segmentData$percentRoundLines >= roundCO, 'roundBuyer', "otherBuyer")))

buyerProfile <- segmentData[,c(1,23:25)]

###summary###
table(buyerProfile$metalPersona) / nrow(buyerProfile)
table(buyerProfile$productPersona) / nrow(buyerProfile)
table(buyerProfile$bullionPersona) / nrow(buyerProfile)

###write csv###
#write.csv(buyerProfile, file = 'buyerPersona.csv', row.names = FALSE)

###write xlsx###
#write.xlsx(buyerProfile, 'buyerPersona.xlsx')