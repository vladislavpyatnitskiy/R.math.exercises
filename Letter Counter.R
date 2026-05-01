letter_calculator <- function(x){ # Calculate number of each letter in word
  
  W <- NULL # Calculate letters for each word
  
  for (j in 1:length(x)){ k <- table(strsplit(tolower(x[j]), "")[[1]])
    
    if (is.null(W)) W <- list(k) else W[[j]] <- k # Add set of number to list
  }
  names(W) <- x # Assign words for each list name
  
  W # Display
}
letter_calculator(c("Avangard", "Atlanta", "Bible", "Coconut")) # test
