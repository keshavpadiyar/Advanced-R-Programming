#' Implementation of Dynamic Method method to solve knapsack problem
#'
#' @param x : Input Data.Frame having Columns v (values) and their w (weights)
#' @param W : Input Numeric - maximum capacity of the knapsack
#'
#' @return The function returns a list having a> maximum values b> elements that can be filled into the knapsack
#' @export
#'
#' @details This algorithm can be used to solve the knapsack problem - Given a set of items,
#'          each with a weight and a value, determine the number of each item to include in a collection so that
#'          total weight is less than or equal to a given limit and the total value is as large as possible.
#'          The basic idea is to use a matrix to store the values of the elements and then select the max value.
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
#'
#' @examples
#'
#' \dontrun{
#'  brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)
#'  $value
#'  [1] 24644
#' $element
#'  [1]  4  7  9 13 15
#'}


knapsack_dynamic<-function(x,W){

  #validation
  stopifnot(is.data.frame(x))
  stopifnot(c("v","w")%in%names(x))
  stopifnot(is.numeric(x$w)&&is.numeric(x$v))
  stopifnot(any(x$w<=W)||any(x$v>0))
  stopifnot(W>0)

  #Elimination

  discard<-which(x$w > W)
  if(length(discard>0)){

    x<-x[-1, ]
    x<-x[order(x$w), ]
  }



  set_matrix<-matrix(0,nrow=nrow(x)+1, ncol=W+1)
  w<-x$w
  v<-x$v
  elements<-c()

  for(i in 2:(nrow(x)+1)){

    for(j in 1:(W+1)){

      if(w[i-1]>j){

        set_matrix[i,j]<-set_matrix[i-1,j]

      }
      else{
        set_matrix[i,j]<-max((v[i-1]+set_matrix[i-1,j-w[i-1]]),(set_matrix[i-1,j]))
      }
    }

  }
  i<-nrow(set_matrix)
  j<-ncol(set_matrix)
  value<-set_matrix[i,j]

  while(i>1 & j>1){
    if(set_matrix[i,j] !=set_matrix[i-1,j]){

      elements<-c(elements, rownames(x)[i-1])
      j<-j - x$w[i-1]

    }
    i<-i-1
  }
  return(list(value = round(set_matrix[nrow(set_matrix),ncol(set_matrix)]),elements = as.numeric(sort(elements))))

}
