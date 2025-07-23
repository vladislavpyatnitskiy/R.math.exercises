is_leap_year <- function(x){ # Find out whether the year is leap
  
  l <- NULL
  
  for (n in 1:length(x)){ y = x[n]
    
    if (y == 0){ M = "Does not exist" } # Year 0 does not exist
    
    else if (y %% 100 == 0 & (y / 100) %% 4 != 0){ M = "No" } # If year / 100
    
    else if (y %% 4 == 0){ M = "Yes" } # If year / 4 or 400
    
    else { M = "No" } # If no previous conditions are satisfied
  
    l <- rbind.data.frame(l, cbind(y, M)) } # Add to data frame
  
  colnames(l) <- c("Year", "Leap Year") # Column names
  
  l # display
}
is_leap_year(c(-1200, -1000, -100, -4, 0, 100, 1000, 1900, 1964, 2000))
