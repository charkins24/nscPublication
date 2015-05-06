#### simpleCap ####
simpleCap <-  # capitalize first letter of words in a string
  function(x) {
    
    if(length(x) != 1) {
      stop('input must be a vector of length 1')
    }
    
    if(inherits(x, 'numeric')) {
      s <- x
    } else {
      s <- 
        strsplit(x, " ")[[1]]
      
      s <- 
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }
    
    return(s)
    
  }
