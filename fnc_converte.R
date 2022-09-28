
# Função converte decimais

fnc_converte <- 
  function(x, numeric=T) {
    
    x <- gsub("\\ ", "", x)
    x <- gsub("\\.", "", x)
    x <- gsub("\\,", ".", x )
    x <- trimws(x)
    
    if(numeric==T) {
      
      x <- as.numeric(x)
      
    }

  }