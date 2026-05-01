letter_calculator <- function(x){ # Calculate number of each letter in word
  
  W <- NULL
  
  for (j in 1:length(x)){ # Calculate letters for each word
    
    y <- tolower(x[j]) # De-capitalise all letters
    
    l <- NULL # Split letters
    
    for (m in 1:nchar(y)) l <- c(l, strsplit(y, split = "")[[1]][m])
    
    d <- unique(l) # Assign new variable to contain unique letters
    
    k <- NULL # Calculate frequency of each letter in the word
    
    for (m in 1:length(d)){ # For each unique letter calculate its number 
      
      f <- 0 # Calculate number of certain letters in word
      
      for (i in 1:length(l)){ # Calculate number of certain letter in a word
        
        f <- c(
          sum(f, ifelse((d[m] %in% l[i]) == T, 1, 0))
          ) 
        
      } 
      
      k <- c(k, f) # Add number for each letter
    }
    
    names(k) <- d # Assign letter
    
    if (is.null(W)) W <- list(k) else W[[j]] <- k # Add set of number to list
  }
  names(W) <- x # Assign words for each list name
  
  W # Display
}
letter_calculator(c("Avangard", "Atlanta", "Bible", "Coconut")) # test
