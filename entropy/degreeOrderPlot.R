#which part of the sharma-mittal space provides the best correlation to performance
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
df$  <- Hybrid

m1 <- melt(df, id.vars="envNames", measure.vars=c("hillClimbing","random", "hybrid"))
colnames(m1) <- c("envNames", "Model", "Performance")

entDF <- data.frame(envNames)
gridValues <- c(2^-3, 2^-2, 2^-1, 2^0, 2^1, 2^2, 2^3, 2^4, 2^5)


#loop through different walk steps
dm <- matrix(0,nrow=0,ncol=5)
walkSizes <- c("10", "100", "1000", "10000")
setwd("Walks")
for (i in 1:4){
	walksize <- walkSizes[i]
	setwd(walksize)
	#load codes and compute entropy
	load("walkCodes.Rdata")
	#loop through different models
	for (model in levels(m1$Model)){
		perf <- m1[m1$Model == model,]$Performance
		#loop through order parameter
		for (r in gridValues){
			#loop through degree parameter
			for (t in gridValues){
				Rf <- ruggedness(r,t)
				correlation <- cor(perf, Rf)
				dm <- rbind(dm, c(model, walksize, r, t, correlation))
			}
		}
		
	}
	setwd("..")
}
setwd("..")

df <- as.data.frame(dm)
colnames(df) <- c("Model", "walkSize", "Order", "Degree", "Correlation")
df$Correlation <- as.numeric(as.character(df$Correlation))
df$Order <- ordered(df$Order, levels = c("0.125", "0.25", "0.5", "1", "2", "4", "8", "16", "32"))
df$Degree <- ordered(df$Degree, levels = c("0.125", "0.25", "0.5", "1", "2", "4", "8", "16", "32"))


P<- ggplot(df, aes(x=Order, y=Degree)) + geom_tile(aes(fill=Correlation), color="white") + scale_fill_gradient(high="white") + facet_wrap(walkSize ~ Model, ncol=3)
ggsave("Sharma-Mittal.pdf")
