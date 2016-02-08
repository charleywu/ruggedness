#Sharma-Mittal entropy function (c) Crupi, Nelson, Meder, Cevolani, & Trentori (in preparation)
SMentropy <- function(p, order, degree){
	ent <- NA
	if (!isTRUE(all.equal(sum(p),1))){
		print("Warning: not a proper probability distribution, does not sum to 1")
	}
	if (sum(p)==0){#if sum is zero
		ent<-0
		return(ent)	
	}
	#special cases
	else if (degree==1){
		if (order==1){#Shannon
			P<- p * log(1/p)
			P[is.nan(P)] <- 0
			ent <- sum(P)
		}else if (order>0){#Renyi
			ent<- (1/(1-order)) * log(sum(p^order))
		}else if (order==0){#Hartley
			ent<- log(sum(p[p>0]^0))
		}
	}else if (order==1){#Gaussian
		p<-p[p!=0]
		ent <- (1/(degree-1))*(1 - exp((1-degree) * sum(p * log(1/p))))

	}else {#General case
		ent <- (1/(degree-1)) * (1 - sum(p^order)^((degree-1)/(order-1)))
	}

	return(ent)
}