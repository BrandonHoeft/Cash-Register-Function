# Cash Register Function
# Author: Brandon Hoeft
# Date: August 23, 2015
# Objective: Write a function that given the inputs of price and payment amount
# returns the exact change needed in terms of Dollar bills and coins.


cash.register <- function (pmt = as.numeric() , price = as.numeric()) {
    change <- pmt - price
    remaining.change <- change
    one.hundred.bill <- 0 # initialize the count of currency denomination vectors
    fifty.bill <- 0
    twenty.bill <- 0
    ten.bill <- 0
    five.bill <- 0
    one.dollar <- 0
    quarter <- 0
    dime <- 0
    nickel <- 0
    penny <- 0
    
    # While() loops will execute a statement based on a condition, like an if() stmt. Difference is
    # that a while() will run the statement over and over again as long as the condition in the 
    # While() stmt is TRUE. 
    
    # while() loop will run as many iterations as the condition is TRUE
    while(remaining.change >= 100) {    
        one.hundred.bill <- one.hundred.bill + 1    # the body that will be run during each loop
        remaining.change <- remaining.change - 100 
    
        # once this loop evaluates to FALSE, the while() loop is abandoned and the cash.register  
        # function will move on to evaluate next lines of code below
    } 
    
    while(remaining.change >= 50) { 
        fifty.bill <- fifty.bill + 1  
        remaining.change <- remaining.change - 50
    }
    
    while(remaining.change >= 20) {
        twenty.bill <- twenty.bill + 1
        remaining.change <- remaining.change - 20
    }
        
    while(remaining.change >= 10) {
        ten.bill <- ten.bill + 1
        remaining.change <- remaining.change - 10
    }
    
    while(remaining.change >= 5) {
        five.bill <- five.bill + 1
        remaining.change <- remaining.change - 5
    }
    
    while(remaining.change >= 1) {
        one.dollar <- one.dollar + 1
        remaining.change <- remaining.change -1
    }
    
    while(remaining.change >= 0.25) {
        quarter <- quarter + 1
        remaining.change <- remaining.change - 0.25
    }
    
    while(remaining.change >= 0.10) {
        dime <- dime + 1
        remaining.change <- remaining.change - 0.10
    }
        
    while(remaining.change >= 0.05) {
        nickel <- nickel + 1
        remaining.change <- remaining.change - 0.05
    }
    
    while (remaining.change >= 0.01) {
        penny <- penny + 1
        remaining.change <- remaining.change - 0.01
    }
    
    change.counts <- c(one.hundred.bill, fifty.bill, twenty.bill, ten.bill, five.bill, one.dollar,
                       quarter, dime, nickel, penny)
    names(change.counts) <- c("$100", "$50", "$20", "$10", "$5", "$1", "Qtrs", "Dimes", "Nickels", 
                              "Pennies")
    
    # generates a diagnostic warning that will generate in the function output even though 
    # it is not the last expression evaluated 
    message("Give the customer the following amounts in change:") 
    
    change.counts # final expression is printed
}

# alternative way to print the message and counts - store a message and the change.counts vector 
# as a list and print the 2 list elements as the final expression to be evaluated
    # message.to.clerk <- "Give the customer the following amounts in change: "
    # temp <- list(message.to.clerk = message.to.clerk, change.counts = change.counts)
    # print(temp[1:2])


# Test it out 
cash.register(500, 150)
cash.register(5, 2.19)
cash.register(10, 5.18)

