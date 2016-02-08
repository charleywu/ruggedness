#random walks
rm(list=ls())
ptm<-proc.time()
setwd("..") #move to parent folder
setwd("environments")

steps <- 10000
stepsize <- 1 #try 10
replications <- 100

load("environments.Rdata")

walkList <- list()
#loop through environments
for (i in 1:length(fitness)){
	env <- fitness[[i]]
	#output for each walk
	walks <- matrix(nrow=steps,ncol=replications)
	#loop through replications
	for (rep in 1:replications){

		#begin random walks
		#sample starting location
		startX <- sample(1:1001,1)
		startY <- sample(1:1001,1)
		#matrix storing fitness values for each step (x,y,fitness)
		path <- matrix(nrow=steps,ncol=3)
		path[1,] <- c(startX,startY,env[startX,startY])
		for (step in 2:steps){
			newX <- path[step-1,][1] + sample(0:stepsize,1)
			newY <- path[step-1,][2] + sample(0:stepsize,1)
				#check if beyond maximum bound
			if (newX>1001){
				newX <- newX - 1001
			}
			if (newY>1001){
				newY <- newY - 1001
			}
			path[step,] <- c(newX,newY,env[newX,newY])
		}
		#add finess value to walks
		walks[,rep] <- path[,3]
	}
	#add fitness values to walk
	walkList[[i]] <- walks
}

setwd("..")
setwd("entropy")
save(walkList, file="walks.Rdata")
print(proc.time() - ptm)
