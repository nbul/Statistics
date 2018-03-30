install.packages('car')
require('stats')
require('car')
setwd("/Volumes/DataGurdon/Natalia Bulgakova/Robustness paper/paired/csv")
MTSDdata <- read.csv('average.csv')
MTSDdata2 <- as.matrix(MTSDdata[,c(1,2,4)])
MTSDdata2 <- MTSDdata2[,-1]
MTSDdata2 <- apply(MTSDdata2,2,as.numeric) 
MTSDdata2 <- cbind2(as.matrix(MTSDdata[,1]),MTSDdata2)
MTSDdata2 <- as.data.frame(MTSDdata2)
colnames(MTSDdata2) <- c('genotype','Eccentricity','MTSD')
#MTSDdata2 <- MTSDdata2[!duplicated(MTSDdata2$MTSD),]

MTSDdata2$Eccentricity <- as.numeric(as.character(MTSDdata2$Eccentricity))
MTSDdata2$MTSD <- as.numeric(as.character(MTSDdata2$MTSD))
plot(MTSDdata2$Eccentricity, MTSDdata2$MTSD, col = MTSDdata2$genotype,
     xlab = "Eccentricity",
     ylab = "MTSD")
options(contrasts = c("contr.treatment", "contr.poly"))

model.1 = lm (MTSD ~ Eccentricity + genotype + Eccentricity:genotype,
              data = MTSDdata2)

library(car)

Anova(model.1, type="II")

model.2 = lm (MTSD ~ Eccentricity + genotype,
              data = MTSDdata2)

Anova(model.2, type="II")
summary(model.2)
hist(residuals(model.2), 
     col="darkgray")

plot(fitted(model.2), 
     residuals(model.2))

