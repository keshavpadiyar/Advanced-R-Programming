#' Implementation of Greedy Method method to solve knapsack problem
#'
#' @param x : Input Data.Frame having Columns v (values) and their w (weights)
#' @param W : Input Numeric - maximum capacity of the knapsack
#'
#' @return The function returns a list having a> maximum values b> elements that can be filled into the knapsack
#' @export
#'
#' @details This algorithm can be used to solve the knapsack problem - Given a set of items,
#'          each with a weight and a value, determine the number of each item to include in a collection so that
#'          total weight is less than or equal to a given limit and the total value is as large as possible. The Greedy
#'          algorithm sorts the items in decreasing order of value per unit of weight and then proceeds to insert them into the sack.
#'          This algorithm will not give an exact result (but it can be shown that it will return at least 50% of the true maximum value),
#'          but it will reduce the computational complexity considerably. For more details refer
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#' @examples
#'
#' \dontrun{
#'  greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
#'  $value
#'  [1] 190944
#'  $elements
#'  [1] 256 530 701 559  89  75 759 626 219 244  63 672 455 764 329  77 705 320 110 509 762 729 691 283 553 620 341 187  83 707 511
#'}

greedy_knapsack <- function (x, W){

  stopifnot(class(x)=="data.frame" &&
            class(W)=="numeric"    &&
            colnames(x)[1]=="w"    &&
            colnames(x)[2]=="v"    &&
            W > 0
            )

  v = 0
  w = c()

  #  Get the value per unit weight of the element
  x$vperW <- x$v/x$w

  #  Sort the elements with decreasing order of its value per weight value
  x <- x[order(x$vperW, decreasing = TRUE),]

  #  Considering the row names as the elements
  rowName <- row.names(x)

  # Iterating over all elements
  for(i in 1:nrow(x)){

    #  Start filling the knapsack, if the weight of the element is less than the
    #  Total weight/ (remaining weight after filling).
    if ((W >= 0) & (x$w[i]<=W)){

        W = W - x$w[i]
        #  Sum the total values of all the elements filled into the knapsack
        v = v + x$v[i]

        w[i] = as.numeric(rowName[i])

    }else{

      break

    }

  }

  return(list(value = round(v), elements = w))
}
