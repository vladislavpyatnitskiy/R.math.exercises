unique.digits <- function(x){ L <- NULL # Show numbers with unique digits
  
  for (n in x:999){ l <- NULL # Separate digits and check 
  
    for (m in 1:nchar(as.character(n))){ l <- c(l, strsplit(as.character(n),
                                              split = "")[[1]][m]) }
    
    if (isFALSE(l[1]==l[2]) && isFALSE(l[2]==l[3]) && isFALSE(l[3]==l[1])){ 
      
      L <- c(L,n) } } # Join numbers with unique digits
    
  length(L) # Show number of 3 digit numbers where digits differ
}
unique.digits(100) # Test
