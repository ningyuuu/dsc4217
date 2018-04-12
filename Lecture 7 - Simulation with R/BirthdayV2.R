#This script is the simulation model to handle Birthday problem V2
iteration <- function(n,num.cust){
    #We use 1 to 365 to represent the 365 days in a year. Now generate a series of 30 random number between 1 and 365 to represent the 
    #birthdays of the 30 people
    birthdays <- sample(365, size=num.cust,replace = TRUE)
    
    #Use rank function to rank the birthdays. If all birthdays are different, then rank will return 30 numbers from 1 to 30. Otherwise, it will return different 
    #sequence of numbers. We use this property to tell if there is repeated birthdays.
    rank <- rank(birthdays, ties.method = "min")
    
    hasDuplication <- ifelse(sum(rank) == num.cust*(num.cust+1)/2, 0, 1)
    
    return(hasDuplication)
}



#We create a function to calculate probability with number of iterations as input
simulate <- function(iters, num.cust) {
    results <- sapply(1:iters,iteration, num.cust)
    prob <- sum(results) / iters
    return (prob)
}



#Create a function to calculate the error of the simulation model with the given number of iterations
SimError <- function(iters, num.cust){
    prob1 <- simulate(iters,num.cust)
    prob2 <- simulate(iters,num.cust)
    
    return (abs(prob1-prob2))
}

GetDuplicateBirthDayProb <- function(num.cust,error.tolerance){
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
    
    prob <- simulate(num.iterations,num.cust)
    
    return(prob)
}

#Plot the probability of having duplicated birthdays against number of customers
PlotDuplicatedBirthdayProb <- function(min.num.cust, max.num.cust, error.tolerance){
    library(ggplot2)
    num.custs <- min.num.cust : max.num.cust
    probs <- sapply(num.custs,GetDuplicateBirthDayProb,error.tolerance)
    
    df <- data.frame(NumOfCusts = num.custs, Probabilities = probs)
    ggplot(df, aes(x=NumOfCusts, y=Probabilities)) + geom_line(color="blue") 
        + labs(x="Number of Customers", y="Probability of having duplicated birthdays") 
        + geom_hline(yintercept=0.5,lty=2,color="red")
}