#Sharma-Mittal entropy function (c) Crupi, Nelson, Meder, Cevolani, & Trentori (in preparation)
SMentropy <- function(pdf, order, degree){
	ent <- NA
	if (sum(pdf)!= 1){#check that pdf is a proper probability distribution
		return(ent) 
}	else{
		#special cases
		if (degree==1){
			if (order==1){#Shannon
				P<- pdf * log(1/pdf)
				P[is.nan(P)] <- 0
				ent <- sum(P)
			}else if (order>0){#Renyi
				ent<- (1/(1-order)) * log(sum(pdf^order))
			}else if (order==0){#Hartley
				ent<- log(sum(pdf[pdf>0]^0))
			}
		}else if (order==1){#Gaussian
			pdf<-pdf[pdf!=0]
			ent <- (1/(degree-1))*(1 - exp((1-degree) * sum(pdf * log(1/pdf))))

		}else {#General case
			ent <- (1/(degree-1)) * (1 - sum(pdf^order)^((degree-1)/(order-1)))
		}
	}return(ent)
}