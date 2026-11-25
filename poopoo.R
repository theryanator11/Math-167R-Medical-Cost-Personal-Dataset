
#Assignment 5 -- Math 167R Ryan Stone

#' 1.) The data set beavers (package datasets) describes a small part of a study of
#' the long-term temperature dynamics of beaver Castor canadensis in 
#' north-central Wisconsin. Body temperature was measured by telemetry 
#' every 10 minutes for two females.

    library(datasets) 
    data(beavers)
    head(beaver1)
    head(beaver2)

    #' part A)  Test if the difference between the average body temperature is 
    #' significantly different from 0, specify if the test is paired or not.
      temp1 <- beaver1$temp
      temp1
      temp2 <- beaver2$temp
      temp2
      # we are using an independent 2-sample t test
      t.test(temp1,temp2, alternative = "two.sided")

    #' part B) Test if there is a difference between the variances 
    #' of the two groups.
      var(temp1)
      var(temp2)
      # we are using an F test for this because we are comparing variances.
      var.test(temp1, temp2, alternative = "two.sided")

      
#'  2.) Dr. Tortora thinks that the number of chocolate chips in a cookie 
#'  follows a Poisson distribution; she doesn’t know the mean. The file 
#'  chocolatechips.rdata contains the number of chocolate chips per cookie
#'  on a sample of 30 cookies. Use the χ2 test to test the hypothesis
#'  that the number of chocolate chips in a cookie 
#'  follows a Poisson distribution.
 
      cc <- chocolatechips
      cc
      lambda <- mean(cc)
      
      #categorize the observations using trick from the midterm
      obs <- c(sum(cc<=3), sum(cc==4), sum(cc==5), sum(cc>=6))
      obs
    
      p1 <- ppois(3, lambda)
      p2 <- dpois(4, lambda)
      p3 <- dpois(5, lambda)
      p4 <- 1 - ppois(5, lambda)
      
      probs <- c(p1, p2, p3, p4)
      sum(probs)
      
      Ex <- 30 * probs
      Ex
      chi2 <- sum((obs - Ex)^2 / Ex)
      chi2
      
      degreesFreedom <- length(probs) - 2
      pval <- 1 - pchisq(chi2, degreesFreedom)
      pval
      
      #' since the p value obtained from this process gets me a p value of 
      #' 0.4728337, I fail to reject the null hypothesis (at alpha = 0.05)
      #' that the cookie chip numbers are not distributed like a poisson.

    
#' 3.) Let X1, . . . , Xn be an independent random sample from a normal 
#' distribution with mean 20 and variance 64. Use several values of n and 
#' answer the following questions.
    # A.) Calculate the theoretical probability
    probNorm22 <- function(n){
      1 - pnorm(22, 20, sqrt(64/n), lower.tail = TRUE, log.p = FALSE)      
    }
    
    probNorm22(30)
    probNorm22(40)
    probNorm22(100)
    probNorm22(200)

    #' B.) Write R code to estimate this probability through simulation.
    #' For each simulation repetition:

    test_normals <- function(n, B){
      repeat.vector <- NULL
      for(i in 1:B){
        vector <- rnorm(n, mean = 20, sd = 8)
        avg <- mean(vector)
        #here is the mean of the vector
        repeat.vector<- c(repeat.vector, avg)
      }
      over22 <- repeat.vector[repeat.vector>22]
  
      print("proportion of values of values over 22:")
      prop22 <- length(over22)/length(repeat.vector)
      print(prop22)
    }

    #' C.) Perform the simulation using B = 1,000, B = 5,000, and B = 10,000 
    #' repetitions. For each value of B and n, report:
    #' • the estimated probability;
    #' • the absolute difference between the estimated and theoretical 
    #' probabilities.

    test_normals(30, 1000)
    test_normals(30, 5000)
    test_normals(30, 10000)
    # abs difference of predicted-calculated
    abs(test_normals(30, 10000) - probNorm22(30))
    
    test_normals(40, 1000)
    test_normals(40, 5000)
    test_normals(40, 10000)
    # abs difference of predicted-calculated
    abs(test_normals(40, 10000) - probNorm22(40))

    test_normals(100, 1000)
    test_normals(100, 5000)
    test_normals(100, 10000)
    # abs difference of predicted-calculated
    abs(test_normals(100, 10000) - probNorm22(100))

    test_normals(200, 1000)
    test_normals(200, 5000)
    test_normals(200, 10000)
    # abs difference of predicted-calculated
    abs(test_normals(200, 10000) - probNorm22(200))
    
      #' the estimated probability with 30 individuals is 0.0915
      #' (10000 rounds or repeat, estimate) and the
      #' calculated probability is 0.08545176
      #' |estimated - calculated probability|: 0.00604824
    
      #' the estimated probability with 40 individuals is 0.0591 
      #' (10000 rounds or repeat, estimate) and the
      #' calculated probability is 0.05692315
      #' |estimated - calculated probability|: 0.00217685
    
      #' the estimated probability with 100 individuals is 0.0068 
      #' (10000 rounds or repeat, estimate) and the
      #' calculated probability is 0.006209665
      #' |estimated - calculated probability|: 0.000590335
    
      #' the estimated probability with 200 individuals is 2e-04 
      #' (10000 rounds or repeat, estimate) and the
      #' calculated probability is 0.000203476
      #' |estimated - calculated probability|: 0.000003476
    
    #' D.) Briefly discuss what happens to the accuracy of the estimate
    #' as the number of repetitions and sample size increase.
    
      #' when you increase B, the increased number of trials makes the prediction
      #' more and more accurate and it appears to converge at a number that is 
      #' close to the calculated number that I got from part A. Additionally,
      #' when you increase n, the number of vectors that have an average of above
      #' 22 gets smaller and smaller. The sample mean has a very low probability
      #' of exceeding 22 as n rises.
    
    
