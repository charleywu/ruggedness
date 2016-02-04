#fast epsilon
setwd("..")
setwd("environments")

load("environments.Rdata")

#create list of 1002001 unique x-y coordinates
addresses <- expand.grid(x=1:1001,y=1:1001)

epsilon_star <- c(rep(0,length(fitness)))
#loop through environments
for (envNum in 1:length(fitness)){
	#assign fitness matrix
	m<- fitness[[envNum]]
	#create expande matrix to include neighbors outside of grid
	m2 <- cbind(NA,rbind(NA,m,NA),NA)

	#build 8 x 1002001 matrix, where each column contains the address of each of the 1002001 x,y coordinates of the original matrix
	ret<-c()
	for(i in 1:-1)
	  for(j in 1:-1)
	    if(i!=0 || j !=0)
	      ret<-rbind(ret,m2[addresses$x+i+1+nrow(m2)*(addresses$y+j)]) 

	#maximum difference value, which will become epsilon*
	maxDiff <- 0

	for (i in 1:length(addresses)){
		xloc <- addresses$x[i]
		yloc <- addresses$y[i]
		fit <- m[xloc,yloc]
		bestDiff <- max(abs(ret[,i] - fit),na.rm=TRUE)
		if (bestDiff>maxDiff){
			maxDiff <- bestDiff
		}
	}

	#assign max diff to epsilon_star vec
	epsilon_star[envNum] <- maxDiff
}
#epsilon_star <- c(0.016756621, 0.003327951, 0.005683390, 0.032014352, 0.003956948, 0.034378054, 0.002517841, 0.009171281, 0.013481127, 0.013129699, 0.001362400, 0.001296913, 0.012938648, 0.001057927)
