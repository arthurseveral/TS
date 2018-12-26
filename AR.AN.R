#R version 3.3.2 
  
sliding.window <- function(T, k) {

	A <- matrix(nrow = 0, ncol = k);
	
	for(i in 1:(length(T) - k + 1)) {
		A <- rbind(A, T[c(i:(i + (k - 1)))]);
	}

	return (A);
}
                          
                          
AR.AN <- function(T, strategy = c("DIVIDE", "SUBTRACT", "MIXED"), additive.smoothing = 0) {

	strategy <- match.arg(strategy);

	AR.AN.DIVIDE <- function(T) {
		output <- matrix(T, nrow=nrow(T), ncol=ncol(T));
        
		for(row in 1:nrow(T)) {
			for(col in 1:ncol(T)) {
				output[row, col] <- T[row, col] / mean(T[row,]);
			}
		}
        
		return(output);
	}

	AR.AN.SUBTRACT <- function(T) {
		output <- matrix(T, nrow=nrow(T), ncol=ncol(T));
        
		for(row in 1:nrow(T)) {
			for(col in 1:ncol(T)) {
				output[row, col] <- T[row, col] - mean(T[row,]);
			}
		}
        
		return(output);
	}

	AR.AN.MIXED <- function(T) {
		output <- matrix(T, nrow=nrow(T), ncol=ncol(T));
        
		for(row in 1:nrow(T)) {
			for(col in 1:ncol(T)) {
				T[row, col] <- (T[row, col] - mean(T[row,])) / mean(T[row,]);
			}
		}
        
		return(output);
	}
	
	res <- NULL;
	
	if(strategy == "DIVIDE") {
		res <- AR.AN.DIVIDE(T);
	}
	
	if(strategy == "SUBTRACT") {
		res <- AR.AN.SUBTRACT(T);
	}
	
	if(strategy == "MIXED") {
		res <- AR.AN.MIXED(T);
	}
	
	model <- lm(V1 ~ ., as.data.frame(res));
    
	return(model);
}
            
T <- sliding.window(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 3);

AR.AN(T, strategy = "SUBTRACT");
AR.AN(T, strategy = "DIVIDE", additive.smoothing = 0.01);
AR.AN(T, strategy = "MIXED", additive.smoothing = 0.01);
