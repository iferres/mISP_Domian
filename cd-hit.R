runCdhit <- function(file){
  
  if (Sys.which('cd-hit')==''){
    stop("cd-hit is not in your $PATH.")
  }
  
  ofile <- sub('[.]\\w+','.cdhit', file)
  
  cdhit <- paste0('cd-hit -c 0.97 -i ', file, ' -o ', ofile)
  
  system(cdhit)
  
  return(ofile)  
}




