#provides Rf for each environment (ruggedness entropy)
rm(list=ls())
ptm<-proc.time()
setwd("..") #move to parent folder in dropbox

#parameters
steps <- 10000 #number of steps taken by each increasing random walk
stepsize <- 10 #max size of each step, where 1 = range_env/1000
replications <- 5 #number of replications to perform, averaging over the max_delta_fit values

# solved from epsilonSearch.R
epsilonVec <- c(0.14215015, 0.81185975, 0.55446881, 0.50626783, 0.56549369, 0.03850807, 0.25362396,0.35231084, 0.28750825, 0.43181304, 0.16201408, 0.79754580, 0.67709166, 0.26071785, 0.43970627) 

#load range data
load("range.Rdta")
#load environment function
source("optimization_functions2.R")
allEnvironmentFunctions <- c(ackley,crossit,drop,egg,unif,griewank,holder,langer,levy,levy13,rastr,schaffer2,schaffer4,schwef,shubert)
allEnvironmentNames <- c("ackley","crossit","drop","egg","unif","griewank","holder","langer","levy","levy13","rastr","schaffer2","schaffer4","schwef","shubert")

#negative function because R is stupid (x less than negative y looks like assign value; x<-y)
neg <- function(x) -x
#function for coding each line of random walk (*-*)
line_code <- function(delta_fitness, e){
	code <- 0
	if (delta_fitness<neg(e)){
		code <- neg(1)
	}else if (abs(delta_fitness)<=e){
		code<-0
	}else if (delta_fitness>e){
		code<- 1
	}
	return(code)
}
#function for coding each pair of steps in random walk (*-*-*)
sequence_code <- function(line1, line2){
	sequence <- "0 0"
	if (line1==0){
		if (line2==1){
			sequence <- "0 1"
		} else if (line2==neg(1)){
			sequence<- "0 -1"
		}
	} else if (line1==1){
		if (line2==0){
			sequence <- "1 0"
		}else if (line2==neg(1)){
			sequence <- "1 -1"
		}
	} else if (line1==neg(1)){
		if (line2==0){
			sequence <- "-1 0"
		} else if (line2==1){
			sequence<- "-1 1"
		}
	}
	return(sequence)
}

#BEGIN SIMULATION
ruggednessVec <- c(rep(0,15)) #final output; n = numEnvironments
#1. iterate through environments
for (env_num in 1:15){
	range_env <- range[[env_num]] #range vector for env_num
	envFunction <- allEnvironmentFunctions[[env_num]] #relevant environment function
	epsilon_star <- epsilonVec[env_num] #relevent epsilon star value
	#create vector of epsilon values in range {0,epsilon_star}
	epsilon_values <- c(epsilon_star/128, epsilon_star/64, epsilon_star/32, epsilon_star/16, epsilon_star/8, epsilon_star/4, epsilon_star/2, epsilon_star)
	#output vector of looping through epsilon values; n = number of epsilon values to test
	H_epsilonVec <- c(rep(0,length(epsilon_values))) 
	#2. loop through all epsilon values
	for (epsilon_i in 1:length(epsilon_values)){
		epsilon <- epsilon_values[epsilon_i]
		entropyVec <- c(rep(0,replications)) #output vectors of H(epsilon) for each replication
		#3. replications of random walk
		for (replication in 1:replications){
			walk <-matrix(nrow = steps, ncol = 3) #output of random walk; [x y fitness]
			count <- 1 #step counter for walk
			x <- sample(1:length(range_env),1) #x position
			y <- sample(1:length(range_env),1) #y position
			fitness <- envFunction(c(range_env[x],range_env[y])) #calculate fitness by transforming using range_env
			walk[1,] <- c(x,y,fitness) #randomly select xy position for first position
			#4. random walk
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
			} #walk finished

			#convert to data.frame
			df <- data.frame(walk)
			names(df) <- c("X", "Y", "fitness")
			#add time column
			df$step <- c(1:steps)

			#create code vector
			code_vec <- c(rep(0,6))
			names(code_vec) <- c("0 1", "0 -1", "1 0", "1 -1", "-1 0", "-1 1") #0 refers to flat, 1 to up, -1 to down
			#5. Code each of the step sequences
			i <- 1 #counter for step number
			n_seq <- 0 #counter for number of sequences encountered
			while (i<steps-2){
				#fitness values of each step
				step_1 <- df$fitness[df$step==i]
				step_2 <- df$fitness[df$step==i + 1]
				step_3 <- df$fitness[df$step==i + 2]
				#change in fitness for each line (*-*)
				line_1 <- step_2 - step_1
				line_2 <- step_3 - step_2
				#codes for each line
				code1 <- line_code(line_1,epsilon)
				code2 <- line_code(line_2, epsilon)
				#code for sequence (*-*-*)
				seq_code <-sequence_code(code1,code2)

				#increment count in code_vec if one of the rugged codes
				if (seq_code %in% names(code_vec)){
					code_vec[seq_code] <- code_vec[seq_code] + 1
				}
				#increment counters
				n_seq <- n_seq + 1
				i <- i + 1
			} #walk is coded

			#calculate entropy
			P_pq <- code_vec/n_seq#number of each sequence divided by the total number of sequences encountered
			P <- P_pq * log(P_pq,6) #do this step first to remove NaN values
			P[is.nan(P)] <- 0
			ent <- neg(sum(P)) #entropy
			#add to entropyVec
			entropyVec[replication] <- ent
		} #end of replications

		#average entropy over replications and assign to H_epsilonVec, indexed by epsilon_i
		H_epsilonVec[epsilon_i]<- mean(entropyVec)

	} #end of calculations for this epsilon value
	#find max entropy, over all tested values of epsilon, and assign to ruggednessVec for environment
	ruggednessVec[env_num] <- max(H_epsilonVec)
}	
print(ruggednessVec)
setwd("ruggednessEntropy")
time_complexity<-proc.time() - ptm