#This script is the simulation model to handle Birthday problem V3
iteration <- function(n,num.cust){
    #We use 1 to 365 to represent the 365 days in a year. Now generate a series of 30 random number between 1 and 365 to represent the 
    #birthdays of the 30 people
    birthdays <- sample(365, size=num.cust,replace = TRUE)
    
    #Use table function to get the frequencies of each birthday, which is used to calculate the number of winners
    freq <- table(birthdays)
    
    num.winners <- sum(freq[freq>1])
    
    return(num.winners)
}



#We create a function to calculate probability with number of iterations as input
simulate <- function(iters, num.cust) {
    results <- sapply(1:iters,iteration, num.cust)
    expected.num.winners <- sum(results) / iters
    return (expected.num.winners)
}


#Create a function to calculate the error of the simulation model with the given number of iterations
SimError <- function(iters, num.cust){
    winners1 <- simulate(iters,num.cust)
    winners2 <- simulate(iters,num.cust)
    
    return (abs(winners1-winners2))
}

GetExpectedNumWinners <- function(error.tolerance, num.cust){
    #Now try to find out the desired probability
    num.iterations <- 1000
    
    #Use while loop to run the simulation. If the error is less than desireable threshold, we stop, otherwise,
    #increase the number of iterations.
    while(TRUE) {
        error <- SimError(num.iterations,num.cust)
        
        cat("Number of iterations: ", num.iterations, "\t Error: ", error,"num.cust", "\n")
        if(error < error.tolerance | num.iterations>1000000)
            break
        
        num.iterations <- num.iterations *10
    }
    
    expected.num.winners <-simulate(num.iterations,num.cust)
    return(expected.num.winners)
}

PrizeforGame<-function(num.cust, error.tolerance) {
    expect.num.winners <- GetExpectedNumWinners(error.tolerance,num.cust)
    max.prize <- 10*num.cust / expect.num.winners
    cat("Max prize to pay per customer: ", max.prize)
}