source("complete.R")

corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  n <- 332
  
  data <- complete(directory,1:n)
  res <- c()
  for(i in 1:nrow(data))
  {
    if(data[i,2] > threshold) {
      dt <- getmonitor(data[i,1],directory)
      res <- c(res,cor(dt$sulfat[complete.cases(dt$sulfat,dt$nitrate)],
                       dt$nitrate[complete.cases(dt$sulfat,dt$nitrate)]))
      ##print(cor(dt$sulfate, dt$nitrate))
      #print(
      #cor(dt$sulfat[complete.cases(dt$sulfat,dt$nitrate)],
      #dt$nitrate[complete.cases(dt$sulfat,dt$nitrate)])
      #)
    }
  }
  res
}