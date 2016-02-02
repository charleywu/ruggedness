#provides an epsilon* value for each environment
start <-proc.time()
rm(list=ls())
setwd("..")

#parameters
steps <- 10000 #number of steps taken by each increasing random walk
stepsize <- 10 #size of each step, where 1 = range_env/1000
replications <- 100 #number of replications to perform, averaging over the max_delta_fit values


#load range data
load("range.Rdta")
#load environment function
source("optimization_functions2.R")
allEnvironmentFunctions <- c(ackley,crossit,drop,egg,unif,griewank,holder,langer,levy,levy13,rastr,schaffer2,schaffer4,schwef,shubert)
allEnvironmentNames <- c("ackley","crossit","drop","egg","unif","griewank","holder","langer","levy","levy13","rastr","schaffer2","schaffer4","schwef","shubert")


#begin simulation
epsilonVec <- c(rep(0,15)) #final output
#1. iterate through environments
for (env_num in 1:15){
	range_env <- range[[env_num]]
	envFunction <- allEnvironmentFunctions[[env_num]]

	epsilon_star <- c(rep(0,replications))
	#2. replications of random walk
	for (replication in 1:replications){
		walk <-matrix(nrow = steps, ncol = 3) #output of random walk; [x y fitness]
		count <- 1 #step counter for walk
		x <- sample(1:length(range_env),1) #x position
		y <- sample(1:length(range_env),1) #y position
		fitness <- envFunction(c(range_env[x],range_env[y])) #calculate fitness by transforming using range_env
		walk[1,] <- c(x,y,fitness) #randomly select xy position for first position

		#3. random walk
		while (count<steps){
			#new step positions
			stepx <- walk[count,][1] + sample(0:stepsize,1)
			stepy <- walk[count,][2] + sample(0:stepsize,1)
	
			#check if beyond maximum bound
			if (stepx>length(range_env)){
				stepx <- stepx - length(range_env)
			}
			if (stepy>length(range_env)){
				stepy <- stepy - length(range_env)
			}
			#increment count and save to walk matrix
			count <- count + 1
			fitness <- envFunction(c(range_env[stepx],range_env[stepy]))
			walk[count,] <- c(stepx, stepy, fitness)
		}

		#convert to data.frame
		df <- data.frame(walk)
		names(df) <- c("X", "Y", "fitness")
		#add time column
		df$step <- c(1:steps)

		#4. find the largest difference in fitness
		max_delta_fit <- 0 
		for (i in 2:steps - 1){
			step_i <- df$fitness[df$step==i]
			step_iplus1 <- df$fitness[df$step==i + 1]
			delta_fit <- abs(step_i - step_iplus1)
			if (delta_fit>max_delta_fit){
				max_delta_fit <- delta_fit
			}
		}
		#add max_delta_fit to epsilon_star
		epsilon_star[replication] <- max_delta_fit
	}
	#add average of epsilon_star values to epsilonVec
	epsilonVec[env_num] <- mean(epsilon_star)
}

print(epsilonVec)
time_complexity<-proc.time() - start
setwd("ruggednessEntropy")