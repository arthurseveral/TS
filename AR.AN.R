#R version 3.3.2

sliding.window <- function(T, k) {
  
  A <- matrix(nrow = 0, ncol = k);
  
  for(i in 1:(length(T) - k + 1)) {
    A <- rbind(A, T[c(i:(i + (k - 1)))]);
  }
  
  return (A);
}

AR.AN.predict <- function(obj, steps.ahead = 1) {
  helper.matrix <- matrix(obj$matrix, nrow=nrow(obj$matrix), ncol=ncol(obj$matrix));
  helper.vector <- obj$helper.vector;
  
  response      <- matrix(nrow = 0, ncol = 3);
  
  for(i in 1:steps.ahead) {
    new.row <- vector();
    
    if(obj$strategy == "DIVIDE") {
      new.row   <- helper.matrix[nrow(helper.matrix), c(2:ncol(helper.matrix))] * (helper.vector[nrow(helper.matrix)] + obj$extensions$additive.smoothing); # Nova linha não normalizada

      helper.vector[length(helper.vector) + 1] <- obj$fn(new.row); # Aplicando a função fn - Usualmente uma média ou mediana - sobre a nova linha

      new.row   <- (new.row + obj$extensions$additive.smoothing) / (helper.vector[length(helper.vector)] + obj$extensions$additive.smoothing); # Nova linha normalizada

      predicted <- predict(obj$model, newdata=as.data.frame(t(new.row)), interval="prediction"); # predict requer um data.frame como argumento - as.data.frame(t(new.row))
      
      new.row[length(new.row) + 1] <- predicted[1][1]; # Adicionando o valor previsto à nova linha normalizada - obs: a função predict retorna uma matrix

      response      <- rbind(response, (predicted * helper.vector[length(helper.vector)]));
    }
    
    if(obj$strategy == "SUBTRACT") {
      new.row   <- helper.matrix[nrow(helper.matrix), c(2:ncol(helper.matrix))] + helper.vector[nrow(helper.matrix)]; # Nova linha não normalizada
      
      helper.vector[length(helper.vector) + 1] <- obj$fn(new.row); # Aplicando a função fn - Usualmente uma média ou mediana - sobre a nova linha
      
      new.row   <- new.row - helper.vector[length(helper.vector)]; # Nova linha normalizada
      
      predicted <- predict(obj$model, newdata=as.data.frame(t(new.row)), interval="prediction"); # predict requer um data.frame como argumento - as.data.frame(t(new.row))
      
      new.row[length(new.row) + 1] <- predicted[1][1]; # Adicionando o valor previsto à nova linha normalizada - obs: a função predict retorna uma matrix
      
      response      <- rbind(response, (predicted + helper.vector[length(helper.vector)]));
    }
    
    helper.matrix <- rbind(helper.matrix, new.row);
  }
  
  return(response);
}

AR.AN <- function(T, fn, strategy = c("DIVIDE", "SUBTRACT"), additive.smoothing = 0) {
  
  strategy <- match.arg(strategy);
  
  helper <- function(T, fn) {
    return(apply(T, 1, function(row) {
      fn(row[1:(length(row) - 1)]);
    }));
  }
  
  AR.AN.DIVIDE <- function(T, helper.vector) {
    output <- matrix(T, nrow=nrow(T), ncol=ncol(T));
    
    for(row in 1:nrow(T)) {
      for(col in 1:ncol(T)) {
        output[row, col] <- (T[row, col] + additive.smoothing) / (helper.vector[row] + additive.smoothing);
      }
    }
    
    return(output);
  }
  
  AR.AN.SUBTRACT <- function(T, helper.vector) {
    output <- matrix(T, nrow=nrow(T), ncol=ncol(T));
    
    for(row in 1:nrow(T)) {
      for(col in 1:ncol(T)) {
        output[row, col] <- T[row, col] - helper.vector[row];
      }
    }
    
    return(output);
  }
  
  T.matrix      <- NULL;
  helper.vector <- helper(T, fn);
  
  if(strategy == "DIVIDE") {
    T.matrix <- AR.AN.DIVIDE(T, helper.vector);
  }
  
  if(strategy == "SUBTRACT") {
    T.matrix <- AR.AN.SUBTRACT(T, helper.vector);
  }
  
  df      <- as.data.frame(T.matrix);
  columns <- colnames(df);
  
  formula.as.string <- paste(paste(columns[length(columns)], paste(columns[1:(length(columns) - 1)], collapse = " + "), sep = " ~ ")); # http://www.cookbook-r.com/Formulas/Creating_a_formula_from_a_string/   
  formula <- lm(as.formula(formula.as.string), df)
  
  return (list("matrix" = T.matrix, "model" = formula, "fn" = fn, "helper.vector" = helper.vector, "strategy" = strategy, "extensions" = list("additive.smoothing" = additive.smoothing)));
}
