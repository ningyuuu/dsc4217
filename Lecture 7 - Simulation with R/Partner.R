#Marriage problem
iteration <- function(iter,N,M) {
    #create a sequence of numbers 1 to N in random order
    a <- sample(N, size=N, replace = FALSE)
    
    #Find the best rank from the first M-1 numbers
    best.of.first.pool <- 0
    if(M>1) #When M=1, the best.of.first.pool will just be 0
        best.of.first.pool <- max(a[1:(M-1)])
    #From now on, the first partner that has rank higher than best.of.first.pool will be chosen
    partner.selected <- 0
    #for(i in M:N){
    #    if(a[i] > best.of.first.pool){
    #        partner.selected <- a[i]
    #        break
    #    }
    #}
    remaining.candidates <- a[M:N]
    b<-which(remaining.candidates > best.of.first.pool)
    
    if(length(b) > 0)  #sometimes, we don't find one better than best.of.first.pool
    {
        partner.selected <- remaining.candidates[b[1]]   #Return the ranking of the first such candidate 
    }
    found <- ifelse(partner.selected == N, 1, 0)   #N is the highest rank
    return(found)
}

#Simulation for a given N and a given M for iters number of iterations
FindBestPartberProb <- function(M,N,iters)
{
    FoundBestPartner <- sapply(1:iters, iteration, N, M)
    return (sum(FoundBestPartner)/iters)
}

#Going through all the possible stopping criteria and decide the one that gives the highest
#possibility of finding the best candidate
FindOptimalStopping <- function (N, iters)
{
    probs <- sapply(1:N, FindBestPartberProb,N,iters)
    maxProb = max(probs)
    optimalStopping = which.max(probs)
    
    cat ("Optimal stopping at ", optimalStopping, " with probability of ", maxProb)
}