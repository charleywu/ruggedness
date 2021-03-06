#Find the max and min fitness of the environment

source("envRange.R")
source("functions.R")

envList <- c(ackley, crossit, drop, egg, griewank, holder, langer, levy, levy13, rastr, schaffer2, schaffer4, schwef, shubert)
rangeList <- list(ackleyRange, crossitRange, dropRange, eggRange, griewankRange, holderRange, langerRange, levyRange, levy13Range, rastrRange, schaffer2Range, schaffer4Range, schwefRange, shubertRange )
envNames <- c("ackley", "crossit", "drop", "egg", "griewank", "holder", "langer", "levy", "levy13", "rastr", "schaffer2", "schaffer4", "schwef", "shubert")


for (i in 1:14){#loop through environments
	env<- envList[[i]]
	envRange <- rangeList[[i]]
	envName <- envNames[[i]]
	fitnessMatrix <- matrix(, ncol=1001, nrow =1001)
	#loop through 1:1000 for x and y
	for (x in 1:1001){
		for (y in 1:1001){
			xloc <- envRange[x]
			yloc <- envRange[y]
			fitnessMatrix[x,y] <- env(c(xloc,yloc))
		}
	}
	print(envName)
	print(c(max(fitnessMatrix),min(fitnessMatrix))) 
}