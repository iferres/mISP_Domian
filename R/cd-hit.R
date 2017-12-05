runCdhit <- function(file, out){
  
  if (Sys.which('cd-hit')==''){
    stop("cd-hit is not in your $PATH.")
  }
  
  if (missing(out)){
    
    out <- rev(strsplit(file, '/')[[1]])[1]
    
    out <- sub('[.]\\w+','.cdhit', out)
    
  }
  
  
  cdhit <- paste0('cd-hit -c 0.97 -A 0.9 -i ', file, ' -o ', out)
  
  system(cdhit)
  
  return(out)  
}




