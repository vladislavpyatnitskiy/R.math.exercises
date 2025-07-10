unique.digits <- function(x, y){ L <- NULL # Show numbers with unique digits

  for (n in x:y){ l <- NULL # Separate digits and check 
    
    N <- as.character(n) # Make numeric data a character and separate digits
  
    for (m in 1:nchar(N)){ l <- c(l, strsplit(N, split = "")[[1]][m]) }
    
    # Join numbers with unique digits
    if (l[1] != l[2] && l[2] != l[3] && l[3] != l[1]){ L <- c(L, n) } } 
    
  length(L) # Show number of 3 digit numbers where digits differ
}
unique.digits(100, 999) # Test
