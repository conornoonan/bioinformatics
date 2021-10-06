#Creating a randomly generated numeric vector of length 100 with values between 0 and 50

random_vec <- runif(100, min = 0, max = 50)

#Sorting the vector by order of their value from largest to smallest

random_vec_sorted <- sort(random_vec, decreasing = TRUE)

#Write a function which calculates the logarithm (base 10) of a vector it is given, subtracts this from the original vector, and returns the new vector of values

log_dif <- function(x){
  
  #generating the log10 of each values in vector x and saving as a new vector
  log_vec <- log10(x)
  
  #subtract log_vec from input vector
  vec_diff <- x - log_vec
  
  return(vec_diff)
}

log_dif_value <- log_dif(random_vec)

#create individual objects which contain the mean, SD, and SE of the vector log_diff_value

log_dif_value_mean <- mean(log_dif_value)
log_dif_value_SD <- sd(log_dif_value)
log_dif_value_SE <- log_dif_value_SD/sqrt(length(log_dif_value))

#Create a vector which contains these three values

log_dif_value_info <- c("Mean" = log_dif_value_mean, "Standard Deviation" = log_dif_value_SD, "Standard Error" = log_dif_value_SE)
log_dif_value_info

##ADVANCED SECTION

#create a sequence of numbers from 15 to 100

test_vec <- c(15:100)

#find the mean of the numbers in this vector which are greater than 20 and less than 60

#creating a vector which contains these values and getting the mean

mean_vec <- mean(range_vec <- test_vec[test_vec > 20 & test_vec < 60])

#find the sum of the numbers in test_vec which are greater than 48.

sum_vec <- sum(range_vec1 <- test_vec[test_vec > 48])

#Write a function which returns the maximum and minimum values of a vector, without using the max(), min(), or range() functions

maxmin_1 <- function(x){
  #sorting vector by descending order and picking the first value
  x_max <- sort(x, decreasing = TRUE)
  max_value <- x_max[1]
  #sorting the vector by ascending order abd picking the first value
  x_min <- sort(x, decreasing = FALSE)
  min_value <- x_min[1]
  max_min <- c("Maximum Value" = max_value, "Minimum Value" = min_value)
  return(max_min)
  
}



maxmin_1(random_vec)
