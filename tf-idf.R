dom_tfidf <- function(df){
  
  nr <- nrow(df)
  
  tf <- apply(df, 2 , function(x){
    x/sum(x)
  })
  
  idf <- apply(df, 2, function(x){
    
    p <- sum(ifelse(x>0, 1L, 0L))
    
    log(nr/p)
  })
  
  o <- vapply(1:ncol(df), function(x){
    
    tf[, x] * idf[x]
    
  }, FUN.VALUE = rep(NA_real_, nrow(df)))
  
  colnames(o) <- colnames(df)
  
  return(o)
}



