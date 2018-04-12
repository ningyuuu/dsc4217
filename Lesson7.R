simulSpecialistDay <- function(n) {
  ntime <- 16
  time <- (0:(ntime-1))*30
  arrival.time.deviance <- sample(c(-15, -5, 0, 10, 15), 
                                  size = ntime, 
                                  replace = TRUE,
                                  prob = c(0.1, 0.25, 0.5, 0.1, 0.05)) 
  arrival.time <- time + arrival.time.deviance
  length.appt <- sample(c(24, 27, 30, 33, 36, 39), 
                        size = ntime,
                        replace = TRUE,
                        prob = c(0.1, 0.2, 0.4, 0.15, 0.1, 0.05))
  start.time <- rep(0, ntime)
  end.time <- rep(0, ntime)
  for (i in 1:ntime) {
    if (i == 1) {
      start.time[i] <- max(arrival.time[i],0)
    } else {
      start.time[i] <- max(arrival.time[i],end.time[i-1])
    }
    end.time[i] <- start.time[i] + length.appt[i]
  }
  rest.time <- sum(start.time - c(0,end.time[1:(length(end.time)-1)]))
  
  return (data.frame(arrival.time = arrival.time, 
                     start.time = start.time,
                     length.appt = length.appt, 
                     end.time = end.time,
                     rest.time = rest.time))
}

computeAverageDayLength <- function(n) {
  arrival.time.mat <- sapply(1:n, function (x) simulSpecialistDay(x)[["end.time"]])
  meanDayLength <- mean(arrival.time.mat[dim(arrival.time.mat)[1],])
  return (meanDayLength)
}

computeAverageRestTime <- function(n) {
  rest.time <- sapply(1:n, function (x) simulSpecialistDay(x)[["rest.time"]][1])
  return (mean(rest.time))
}

computeAccurateAverageDayLength <- function() {
  n <- 10
  print(n)
  err <- Inf
  while (err > 0.1) {
    n <- n*2
    print(n)
    result1 <- computeAverageDayLength(n)
    result2 <- computeAverageDayLength(n*2)
    err <- abs(result1-result2)
  }
  return(result1)
}

computeAccurateAverageRestTime <- function() {
  n <- 10
  print(n)
  err <- Inf
  while (err > 0.1) {
    n <- n*2
    print(n)
    result1 <- computeAverageRestTime(n)
    result2 <- computeAverageRestTime(n*2)
    err <- abs(result1-result2)
  }
  return(result1)
}

plotRestTimeDistribution <- function(n) {
  rest.time <- sapply(1:n, function (x) simulSpecialistDay(x)[["rest.time"]][1])
  df <- data.frame(rest.time = rest.time)
  require(ggplot2) 
  ggplot(data = df, aes(x = rest.time)) + geom_histogram(bins = 10)
}



### Question 2
simulSpecialistDay.2 <- function(n) {
  ntime <- 16
  time <- (0:(ntime-1))*30
  arrival.time.deviance <- sample(c(-15, -5, 0, 10, 15), 
                                  size = ntime, 
                                  replace = TRUE,
                                  prob = c(0.1, 0.25, 0.5, 0.1, 0.05)) 
  arrival.time <- time + arrival.time.deviance
  
  ## start change
  start.prelim.time <- rep(0, ntime)
  end.prelim.time <- rep(0, ntime)
  for (i in 1:ntime) {
    if (i == 1) {
      start.prelim.time[i] <- max(arrival.time[i],0)
    } else {
      start.prelim.time[i] <- max(arrival.time[i],end.prelim.time[i-1])
    }
    end.prelim.time[i] <- start.prelim.time[i] + 10
  }
  ## end change
  
  length.appt <- sample(c(24, 27, 30, 33, 36, 39), 
                        size = ntime,
                        replace = TRUE,
                        prob = c(0.1, 0.2, 0.4, 0.15, 0.1, 0.05)) - 5 #!
  start.time <- rep(0, ntime)
  end.time <- rep(0, ntime)
  for (i in 1:ntime) {
    if (i == 1) {
      start.time[i] <- max(end.prelim.time[i],0)
    } else {
      start.time[i] <- max(end.prelim.time[i],end.time[i-1])
    }
    end.time[i] <- start.time[i] + length.appt[i]
  }
  rest.time <- sum(start.time - c(0,end.time[1:(length(end.time)-1)]))
  
  return (data.frame(arrival.time = arrival.time, 
                     start.time = start.time,
                     length.appt = length.appt, 
                     end.time = end.time,
                     rest.time = rest.time))
}

computeAverageDayLength.2 <- function(n) {
  arrival.time.mat <- sapply(1:n, function (x) simulSpecialistDay.2(x)[["end.time"]])
  meanDayLength <- mean(arrival.time.mat[dim(arrival.time.mat)[1],])
  return (meanDayLength)
}

computeAverageRestTime.2 <- function(n) {
  rest.time <- sapply(1:n, function (x) simulSpecialistDay.2(x)[["rest.time"]][1])
  return (mean(rest.time))
}

computeAccurateAverageDayLength.2 <- function() {
  n <- 10
  print(n)
  err <- Inf
  while (err > 0.1) {
    n <- n*2
    print(n)
    result1 <- computeAverageDayLength.2(n)
    result2 <- computeAverageDayLength.2(n*2)
    err <- abs(result1-result2)
  }
  return(result1)
}

computeAccurateAverageRestTime.2 <- function() {
  n <- 10
  print(n)
  err <- Inf
  while (err > 0.1) {
    n <- n*2
    print(n)
    result1 <- computeAverageRestTime.2(n)
    result2 <- computeAverageRestTime.2(n*2)
    err <- abs(result1-result2)
  }
  return(result1)
}

## Question 3

simulSpecialistDay.3 <- function(n) {
  ntime <- 16
  time <- (0:(ntime-1))*30
  arrival.time.deviance <- sample(c(-15, -5, 0, 10, 15), 
                                  size = ntime, 
                                  replace = TRUE,
                                  prob = c(0.1, 0.25, 0.5, 0.1, 0.05)) 
  arrival.time <- time + arrival.time.deviance
  
  ## start change
  start.prelim.time <- rep(0, ntime)
  end.prelim.time <- rep(0, ntime)
  for (i in 1:ntime) {
    if (i == 1) {
      start.prelim.time[i] <- max(arrival.time[i],0)
    } else {
      start.prelim.time[i] <- max(arrival.time[i],end.prelim.time[i-1])
    }
    end.prelim.time[i] <- start.prelim.time[i] + rnorm(1, mean = 10.5, sd = 1.5) ## change
  }
  ## end change
  
  length.appt <- sample(c(24, 27, 30, 33, 36, 39), 
                        size = ntime,
                        replace = TRUE,
                        prob = c(0.1, 0.2, 0.4, 0.15, 0.1, 0.05)) - 5 #!
  start.time <- rep(0, ntime)
  end.time <- rep(0, ntime)
  for (i in 1:ntime) {
    if (i == 1) {
      start.time[i] <- max(end.prelim.time[i],0)
    } else {
      start.time[i] <- max(end.prelim.time[i],end.time[i-1])
    }
    end.time[i] <- start.time[i] + length.appt[i]
  }
  rest.time <- sum(start.time - c(0,end.time[1:(length(end.time)-1)]))
  
  return (data.frame(arrival.time = arrival.time, 
                     start.time = start.time,
                     length.appt = length.appt, 
                     end.time = end.time,
                     rest.time = rest.time))
}

computeAverageDayLength.3 <- function(n) {
  arrival.time.mat <- sapply(1:n, function (x) simulSpecialistDay.3(x)[["end.time"]])
  meanDayLength <- mean(arrival.time.mat[dim(arrival.time.mat)[1],])
  return (meanDayLength)
}

computeAverageRestTime.3 <- function(n) {
  rest.time <- sapply(1:n, function (x) simulSpecialistDay.3(x)[["rest.time"]][1])
  return (mean(rest.time))
}

computeAccurateAverageDayLength.3 <- function() {
  n <- 10
  print(n)
  err <- Inf
  while (err > 0.1) {
    n <- n*2
    print(n)
    result1 <- computeAverageDayLength.3(n)
    result2 <- computeAverageDayLength.3(n*2)
    err <- abs(result1-result2)
  }
  return(result1)
}

computeAccurateAverageRestTime.3 <- function() {
  n <- 10
  print(n)
  err <- Inf
  while (err > 0.1) {
    n <- n*2
    print(n)
    result1 <- computeAverageRestTime.3(n)
    result2 <- computeAverageRestTime.3(n*2)
    err <- abs(result1-result2)
  }
  return(result1)
}