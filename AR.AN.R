sliding.window <- function(T, k) {

	A <- matrix(nrow = 0, ncol = k);
	
	for(i in 1:(length(T) - k + 1)) {
		A <- rbind(A, T[c(i:(i + (k - 1)))]);
	}

	return (A);
}

AR.AN.predict <- function(obj, steps.ahead = 1) {
	for(i in steps.ahead) {
		newLine <- vector();
		for(col in 1:(ncol(obj.output) - 1)) {
			
		}
		
		obj.output <- rbind(obj.output, newLine);
	}
}

AR.AN <- function(T, strategy = c("DIVIDE", "SUBTRACT"), additive.smoothing = 0) {

	strategy <- match.arg(strategy);

	AR.AN.DIVIDE <- function(T) {
		output <- matrix(T, nrow=nrow(T), ncol=ncol(T));
        
		for(row in 1:nrow(T)) {
			for(col in 1:ncol(T)) {
				output[row, col] <- T[row, col] / (mean(T[row,] + additive.smoothing));
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

	T.output <- NULL;
	
	if(strategy == "DIVIDE") {
		T.output <- AR.AN.DIVIDE(T);
	}
	
	if(strategy == "SUBTRACT") {
		T.output <- AR.AN.SUBTRACT(T);
	}
	
	T.output <- as.data.frame(T.output);

	return (lm(T.output[[ncol(T.output)]] ~ ., as.data.frame(T.output)));
}
            
T <- sliding.window(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 0), 3);

model <- AR.AN(T, strategy = "SUBTRACT");
model <- AR.AN(T, strategy = "DIVIDE", additive.smoothing = 0.01);
