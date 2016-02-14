#Ruggedness to performance
library(reshape2)
library(ggplot2)
require(rje)
library(directlabels)
library(ggrepel)

#from Barckozi, Analytis, & Wu (2016)
#hillClimbing <- c(0.1639468, 0.8280546, 0.2616505, 0.7067217, 0.6742425, 0.2740507, 0.6408546, 0.9148404, 0.849022, 0.7442067, 0.5362564, 0.07194061, 0.6887596, 0.6127429)
#random <- c(0.4890942, 0.9496214, 0.6319627, 0.8390647, 0.9679674, 0.6768137, 0.824881, 0.9876574, 0.9815164, 0.8827425, 0.6818602, 0.4323129, 0.8383287, 0.7272975)
Social <- c(0.7671833, 0.9920558, 0.8937955, 0.949713, 0.9912987, 0.9499782, 0.9599123, 0.9969064, 0.9954847, 0.9644195, 0.8904062, 0.8793531, 0.9579353, 0.9319516)
setwd("Walks")
setwd("100")
load("walkCodes.Rdata")
setwd("..")
setwd("..")
source("SMentropy.R")
source("Rf.R")

envNames <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert")

df <- data.frame(envNames)
df$Social <- Social
#df$hillClimbing <- hillClimbing
#df$random <- random

df$Malan <- Malanruggedness()
df$Quadratic <- ruggedness(2,2)
df$Hartley <- ruggedness(0,1)
df$Tsallis10 <- ruggedness(10,10)
df$NonConcave <- ruggedness(10,0)
df$PropRugged <- propRugged()

m1 <- melt(df, id.vars="envNames", measure.vars=c("Malan","Quadratic", "Hartley", "Tsallis10", "NonConcave", "PropRugged"))
colnames(m1) <- c("envNames", "Entropy", "Ruggedness")
m2 <- melt(df, id.vars="envNames", measure.vars=c("Social"))
colnames(m2) <- c("envNames", "Model", "Performance")

dm <- merge(m1,m2)

P<- ggplot(dm, aes(x=Ruggedness, y=Performance, col=Model, shape = Model)) + geom_point() + stat_smooth(method=lm)  + labs(x="Ruggedness (Rf)", y= "Average Payoff") + theme_bw() + facet_wrap(~ Entropy, ncol=3, scales="free")

ggsave("EntropyPlotMalan100.pdf", width=8, height=6)

#correlations

ents <- c("Malan","Quadratic", "Hartley", "Tsallis10", "NonConcave", "PropRugged")
#loop through entropy measures
for (ent in ents){
	print(ent)
	dm.sub <- subset(dm, Entropy==ent)
	L <- dm.sub$Model == "Social"
	print(cor(dm.sub[L,]$Ruggedness, dm.sub[L,]$Performance))
	fit <- lm(dm.sub[L,]$Ruggedness ~ dm.sub[L,]$Performance)
	rse<-summary(fit)$sigma
	print(rse)
}