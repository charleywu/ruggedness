#coding random walks
rm(list=ls())
ptm<-proc.time()
load("walks.Rdata")

#epsilon star values from fastEpsilon.R
epsilon_star <- c(0.016756621, 0.003327951, 0.005683390, 0.032014352, 0.003956948, 0.034378054, 0.002517841, 0.009171281, 0.013481127, 0.013129699, 0.001362400, 0.001296913, 0.012938648, 0.001057927)
#from 10 step random walks
#epsilon_star <- c(0.14215015, 0.81185975, 0.55446881, 0.50626783, 0.03850807, 0.25362396,0.35231084, 0.28750825, 0.43181304, 0.16201408, 0.79754580, 0.67709166, 0.26071785, 0.43970627) 

#negative function because R is stupid (x less than negative y looks like assign value; x<-y)
neg <- function(x) -x

#function for coding each line of random walk (*-*)
line_code <- function(delta_fitness, epsilon){
	code <- NA
	if (delta_fitness<neg(epsilon)){
		code <- neg(1)
	}else if (abs(delta_fitness)<=epsilon){
		code<-0
	}else if (delta_fitness>epsilon){
		code<- 1
	}
	return(code)
}

#function for coding each pair of steps in random walk (*-*-*)
sequence_code <- function(line1, line2){
	sequence <- "NA NA"
	if (line1==0){
		if (line2==1){
			sequence <- "0 1"
		} else if (line2==neg(1)){
			sequence<- "0 -1"
		} else if (line2==0){
			sequence <- "0 0"
		}
	} else if (line1==1){
		if (line2==0){
			sequence <- "1 0"
		}else if (line2==neg(1)){
			sequence <- "1 -1"
		}else if (line2==1){
			sequence <- "1 1"
		}
	} else if (line1==neg(1)){
		if (line2==0){
			sequence <- "-1 0"
		} else if (line2==1){
			sequence<- "-1 1"
		}else if (line2==neg(1)){
			sequence <- "-1 -1"
		}
	} 
	return(sequence)
}

walkCodes <- list()
#loop through environments
for (i in 1:length(walkList)){
	walks <- walkList[[i]]
	estar <- epsilon_star[i]
	#loop through epsilon values
	distributionList <- list() #distribution of sequences over all epsilon values (9 epsilon values: matrix(100 reps x 998 sequences))
	epsilonVec <- c(0, estar/128, estar/64, estar/32, estar/16, estar/8, estar/4, estar/2, estar)
	for (e in 1:9){
		epsilon<- epsilonVec[e]
		#loop through replications
		codeMatrix <- matrix(0,nrow=ncol(walks), ncol=9) #9 different sequences x num replications
		colnames(codeMatrix) <- c("0 1", "0 -1", "1 0", "1 -1", "-1 0", "-1 1", "1 1", "-1 -1", "0 0") #first 6 are rugged, 7-8 are smooth, and 9 is neutral
		for (rep in 1:ncol(walks)){
			#loop through sequences
			for(step in 1:(nrow(walks)-2)){
				step1 <- walks[step,rep]
				step2 <- walks[step+1,rep]
				step3 <- walks[step+2,rep]
				#code lines
				line1 <- line_code(step2-step1, epsilon)
				line2 <- line_code(step3-step2, epsilon)
				#code sequences
				seq_code <- sequence_code(line1,line2)
				#add sequence to codeMatrix
				codeMatrix[rep,][seq_code] <- codeMatrix[rep,][seq_code] + 1
			}
		}
		#aggregate over epsilon values
		distributionList[[e]] <- codeMatrix
	}
	walkCodes[[i]] <- distributionList
	print(i)
}
save(walkCodes, file="walkCodes.Rdata")
print(proc.time() - ptm)
