overlap <- function(a=c(1L, 1L), b=c(1L, 1L)){
  if(a[1]>a[2]){
    a <- rev(a)
  }
  if(b[1]>b[2]){
    b <- rev(b)
  }
  return(a[1]<=b[2] & a[2]>=b[1])
}


#' m is a logical matrix. If we have A overlapping with B, and C don't
#' overlaping with thhe last 2, the matrix would look like:
#' m =
#'   A  B  C
#'A  T  T  F
#'B  T  T  F
#'C  F  F  T

determineOverlap<-function(m){
  st<-c(1,1)
  li<-list()
  #start from m[1,1]
  li[[1]]<-colnames(m)[st[2]]
  #while 'st' doesn't reach the right edge of the matrix...
  while(st[2]!=ncol(m)){
    #If next (right) cell is TRUE..
    if(m[st[1],st[2]+1]==T){
      #..then move to next cell.
      st[2]<-st[2]+1
      li[[length(li)]][length(li[[length(li)]])+1]<-colnames(m)[st[2]]
      #If next cell is FALSE but bottom cell is TRUE...
    }else if(m[st[1]+1,st[2]]==T){
      #..then move to bottom cell.
      st[1]<-st[1]+1
      #If both next and bottom cells are FALSE...
    }else{
      #..then move to the bottom-right (diagonal) cell.
      st[1]<-st[1]+1
      st[2]<-st[2]+1
      li[length(li)+1]<-colnames(m)[st[2]]
    }
  }
  return(li)
}


rm_overlaping_clans<-function(){}
