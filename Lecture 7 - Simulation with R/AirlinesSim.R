#This script is to simulate airline ticket sales

#One Iteration of the simulation
iteration <- function(iter, discounted.seats, total.seats = 100, mean.demand = 110, sd.deman=30,  prob.convert=0.6, full.fare=590, discounted.fare=450)
{
    demand <- max(0,floor(rnorm(1,mean.demand,sd.deman)))
    discounted.sales <- ifelse(demand > discounted.seats, discounted.seats, demand)
    full.fare.demand <- ifelse(demand > discounted.sales,demand - discounted.sales,0)
    full.fare.sales <- min(rbinom(1,full.fare.demand,prob.convert),total.seats-discounted.seats)
    
 #   cat("demand: ", demand, " dicounted sales: ", discounted.sales, " full fare demand: ", full.fare.demand, " Full fare Sales: ", full.fare.sales)
    return(discounted.sales*discounted.fare + full.fare.sales*full.fare)
}

#Simulate for a given number of discounted seats
simulate <- function(discounted.seats,iters, total.seats = 100, mean.demand = 110, sd.deman=30,  prob.convert=0.6, full.fare=590, discounted.fare=450)
{
    revs <- sapply(1:iters,iteration, discounted.seats,total.seats, mean.demand, sd.deman,  prob.convert, full.fare, discounted.fare)
    
    return(sum(revs)/iters)
}

#Find the optimal number of discouted seats
FindOptimalDiscSeats<-function(mean.demand = 110, iters, total.seats = 100, sd.deman=30,  prob.convert=0.6, full.fare=590, discounted.fare=450)
{
    seats <- 1:total.seats
    expected.revs <- sapply(seats, simulate, iters,total.seats, mean.demand, sd.deman,  prob.convert, full.fare, discounted.fare)
    df <- data.frame(No.Discounted.Seat = seats, Expected.Rev = expected.revs)
    
    optimal.disc.seat <- seats[which.max(expected.revs)]
    return(total.seats - optimal.disc.seat)
}

#this function is used for assessing impact of discount rate on seat allocation
FindOptimalDiscSeats1<-function(discounted.fare=450, mean.demand = 110, iters, total.seats = 100, sd.deman=30,  prob.convert=0.6, full.fare=590)
{
    cat(discounted.fare)
    seats <- 1:total.seats
    expected.revs <- sapply(seats, simulate, iters,total.seats, mean.demand, sd.deman,  prob.convert, full.fare, discounted.fare)
    df <- data.frame(No.Discounted.Seat = seats, Expected.Rev = expected.revs)
    
    optimal.disc.seat <- seats[which.max(expected.revs)]
    return(total.seats - optimal.disc.seat)
}

#this function is used for assessing impact of discount rate on seat allocation and return
#optimal revenue
FindOptimalDiscSeats2<-function(discounted.fare=450, mean.demand = 110, iters, total.seats = 100, sd.deman=30,  prob.convert=0.6, full.fare=590)
{
    seats <- 1:total.seats
    expected.revs <- sapply(seats, simulate, iters,total.seats, mean.demand, sd.deman,  prob.convert, full.fare, discounted.fare)
    df <- data.frame(No.Discounted.Seat = seats, Expected.Rev = expected.revs)
    
    optimal.disc.seat <- seats[which.max(expected.revs)]
    combo <- c(optimal.disc.seat, max(expected.revs))
    return(combo)
}

FindSeatsAllocationByDemand <- function(iters,total.seats = 100, sd.deman=30,  prob.convert=0.6, full.fare=590, discounted.fare=450){
    mean.demands <- seq(50,200,by=5)
    seats <- sapply(mean.demands,FindOptimalDiscSeats,iters,total.seats, sd.deman,  prob.convert, full.fare, discounted.fare)
    
    require(ggplot2)
    df <- data.frame(Mean.Demand<-mean.demands, Seat<-seats)
    ggplot(df,aes(x=Mean.Demand,y=Seat)) + geom_line()
}

FindSeatsAllocationByDiscRate <- function(iters,total.seats = 100, mean.demand = 110, sd.deman=30,  prob.convert=0.6, full.fare=590){
    discount.rates <- seq(0.3,1,by=0.05)
    disCount.fares <- floor(discount.rates*full.fare)
    seats <- sapply(disCount.fares,FindOptimalDiscSeats1,mean.demand,iters,total.seats, sd.deman,  prob.convert, full.fare)
    
    require(ggplot2)
    df <- data.frame(Discount.Rate<-discount.rates, Seat<-seats)
    ggplot(df,aes(x=Discount.Rate,y=Seat)) + geom_line()
}

FindBestCombination <- function(iters,total.seats = 100, sd.deman=30,  prob.convert=0.6, full.fare=590)
{
    mean.demand <- 110
    discount.rates <- seq(0.3,1,by=0.05)
    disCount.fares <- floor(discount.rates*full.fare)
    combos <- lapply(disCount.fares,FindOptimalDiscSeats2,mean.demand,iters,total.seats, sd.deman,  prob.convert, full.fare)
    combos
}