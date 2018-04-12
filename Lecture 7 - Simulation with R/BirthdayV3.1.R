#This script is the simulation model to handle Birthday problem V1
iteration <- function(n, numofpeople){
    #Get a simulation of whether the people were born in leap year first
    total.leap.year <- sum(sample(c(TRUE,FALSE), size = numofpeople, replace = TRUE, prob=c(0.25,0.75)))
    
    #We use 1 to 365 to represent the 365 days in a normal year, and use number 366 to represent 29 Feb in a leap year
    #Now generate a series of 30 random number between 1 and 365 to represent the 
    #birthdays of the people
    birthdays1 <- sample(366, size=total.leap.year,replace = TRUE)
    birthdays2 <- sample(365, size=(numofpeople-total.leap.year),replace = TRUE)
    
    birthdays <- c(birthdays1,birthdays2)
    
    #Use rank function to rank the birthdays. If all birthdays are different, then rank will return 30 numbers from 1 to 30. Otherwise, it will return different 
    #sequence of numbers. We use this property to tell if there is repeated birthdays.
    rank <- rank(birthdays, ties.method = "min")
    
    hasDuplication <- ifelse(sum(rank) == numofpeople*(numofpeople+1)/2, 0, 1)
    
    return(hasDuplication)
}

#We create a function to calculate probability with number of iterations as input?
simulate <- function(iters,numofpeople) {
    results <- sapply(1:iters,iteration,numofpeople)
    prob <- sum(results) / iters
    return (prob)
}

#Create a function to calculate the error of the simulation model with the given number of iterations
SimError <- function(iters, numofpeople){
    prob1 <- simulate(iters,numofpeople)
    prob2 <- simulate(iters,numofpeople)
    
    return (abs(prob1-prob2))
}

GetDuplicateBirthDayProb <- function(numofpeople, threshold){
    #Now try to find out the desired probability
    num.iterations <- 1000
    
    while(num.iterations < 10000000) {
        error <- SimError(num.iterations,numofpeople)
        
        if(error < threshold)
            break
        
        num.iterations <- num.iterations *10
    }
    
    simulate(num.iterations,numofpeople)
}