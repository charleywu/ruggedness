#Ruggedness to performance
library(reshape2)
library(ggplot2)
require(rje)
library(directlabels)
library(ggrepel)

#from Barckozi, Analytis, & Wu (2016)
hillClimbing <- c(0.1639468, 0.8280546, 0.2616505, 0.7067217, 0.6742425, 0.2740507, 0.6408546, 0.9148404, 0.849022, 0.7442067, 0.5362564, 0.07194061, 0.6887596, 0.6127429)
random <- c(0.4890942, 0.9496214, 0.6319627, 0.8390647, 0.9679674, 0.6768137, 0.824881, 0.9876574, 0.9815164, 0.8827425, 0.6818602, 0.4323129, 0.8383287, 0.7272975)
Hybrid <- c(0.7671833, 0.9920558, 0.8937955, 0.949713, 0.9912987, 0.9499782, 0.9599123, 0.9969064, 0.9954847, 0.9644195, 0.8904062, 0.8793531, 0.9579353, 0.9319516)

source("SMentropy.R")
source("Rf.R")

envNames <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert")

df <- data.frame(envNames)
df$hillClimbing <- hillClimbing
df$random <- random
df$hybrid <- Hybrid

m1 <- melt(df, id.vars="envNames", measure.vars=c("hillClimbing","random", "hybrid"))
colnames(m1) <- c("envNames", "Model", "Performance")

#loop through different walk steps

dfList <- list()
walkSizes <- c("10", "100", "1000", "10000")
setwd("Walks")
for (i in 1:4){
	walksize <- walkSizes[i]
	setwd(walksize)
	#create temporary dataframe
	df <- data.frame(envNames)
	#load codes and compute entropy
	load("walkCodes.Rdata")
	df$Shannon <- ruggedness(1,1)
	df$Quadratic <- ruggedness(2,2)
	df$Hartley <- ruggedness(0,1)
	df$Tsallis <- ruggedness(10,10)
	#melt
	m2 <- melt(df, id.vars="envNames", measure.vars=c("Shannon","Quadratic", "Hartley", "Tsallis"))
	colnames(m2) <- c("envNames", "Entropy", "Ruggedness")
	m2$walkSize <- rep(walksize, nrow(m2))
	#add to list
	dfList[[i]] <- m2
	setwd("..")
}

m2 <- do.call("rbind", dfList)
dm <- merge(m1,m2)
dm$walkSize <- as.factor(dm$walkSize)
dm$Correlation <- rep(0,nrow(dm))
#for model, for walkSize, for entropy, find correlation between performance and ruggedness
for (model in levels(dm$Model)){
	for (walksize in levels(dm$walkSize)){
		for (entropy in levels(dm$Entropy)){
			dm.sub <-dm[dm$Model == model & dm$walkSize == walksize & dm$Entropy == entropy,]
			r <- cor(dm.sub$Performance, dm.sub$Ruggedness)
			dm[dm$Model == model & dm$walkSize == walksize & dm$Entropy == entropy,]$Correlation <- r
		}
	}
}

P<- ggplot(dm, aes(x=walkSize, y=Correlation, col=Model, shape=Model)) + geom_point()  + labs(x="Walk Size", y= "Correlation (Performance ~ Ruggedness)") + theme_bw() + facet_wrap(~ Entropy, ncol=2, scales="free")
ggsave("WalkSizePlot.pdf")


