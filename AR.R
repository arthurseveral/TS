AR.LSE <- function(z, order = 1) {

    res <- matrix(z[1:(length(z) - order)]);

    for(i in 1:order) {
        res <- cbind(res, z[(i + 1):length(z)]);
    }
    
    model <- lm(V1 ~ ., as.data.frame(res))
    
    return(model);
}

series <- w <- rnorm(100);

for(i in 2:100) {
    series[i] <- w[i] + 0.6 * w[i - 1];
}

AR.LSE(series, 1);

arima(series, order=c(1, 0, 0));

series <- w <- rnorm(100);

for(i in 3:100) {
    series[i] <- w[i] + 0.6 * w[i - 1] + 0.45 * w[i - 2];
}

AR.LSE(series, 2);

arima(series, order=c(2, 0, 0));

series <- w <- rnorm(100);

for(i in 4:100) {
    series[i] <- w[i] + 0.6 * w[i - 1] + 0.45 * w[i - 2] + 0.35 * w[i - 3];
}

AR.LSE(series, 3);

arima(series, order=c(3, 0, 0));
