#function that returns a ruggedness vector, given an order and degree for sharma mittal
ruggedness <-function(order,degree){
	ruggednessVec <- c(rep(0,length(walkCodes)))
	#loop through environments
	for (i in 1:length(walkCodes)){
		env <- walkCodes[[i]]
		H_epsilonVec <- c(rep(0, length(env)))
		#loop through epsilon values
		for (j in 1:length(env)){
			replications <- env[[j]]
			avgEntropy <- 0 #used to store entropy values
			#loop through replications
			for (rep in 1:nrow(replications)){
				p <- replications[rep,1:6]#calculating only FEM
				if (sum(p)!=0){
					pdf <- p/sum(p)
					ent <- SMentropy(pdf,order,degree)
					#scale by n_rugged/n_total
					ent <- ent * (sum(replications[rep,1:6])/sum(replications[rep,]))
				}else{
					ent <- 0
				}
				#add to avgEntropy (averaged later)
				avgEntropy <- avgEntropy +  ent
			}
			#calculate average entropy
			avgEntropy <- avgEntropy / nrow(replications)
			H_epsilonVec[j] <- avgEntropy
		}
		#put max entropy over epsilon value into ruggednessVec
		ruggednessVec[i] <- max(H_epsilonVec)
	}
	return(ruggednessVec)
}
