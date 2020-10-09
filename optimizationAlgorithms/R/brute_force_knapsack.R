#' Implementation of Brute-force method to solve knapsack problem
#'
#' @param x : Input Data.Frame having Columns v (values) and their w (weights)
#' @param W : Input Numeric - maximum capacity of the knapsack
#' @param parallel : Input Flag, by default the value is FALSE.
#'                   FALSE: The algorithm follows normal execution routine.
#'                   TRUE: Parallelism is activated and the execution routine follows execution on multiple workers
#'
#' @return The function returns a list having a> maximum values b> elements that can be filled into the knapsack
#' @export
#'
#' @details This algorithm can be used to solve the knapsack problem - Given a set of items,
#'          each with a weight and a value, determine the number of each item to include in a collection so that
#'          total weight is less than or equal to a given limit and the total value is as large as possible. This algorithm
#'          enumerates all n-combinations of n objects. to find the max value and all the elements that can be fitted into the
#'          knapsack. But When number of  item (n) is increased, execution time starts to increase rapidly. For more details
#'          refer
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#'
#' @examples
#'
#' \dontrun{
#'  brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500)
#'  $value
#'  [1] 24644
#' $element
#'  [1]  4  7  9 13 15
#'  }
#'


brute_force_knapsack <- function (x, W, parallel = FALSE){

  stopifnot(class(x)=="data.frame"   &&
              class(W)=="numeric"    &&
              colnames(x)[1]=="w"    &&
              colnames(x)[2]=="v"    &&
              W > 0
            )


  if (parallel == FALSE){

    maxValue = 0

    #  Total combinations of the elements that can be put into knapsack forms 2^(no of rows)
    n_combinations = 2^nrow(x)

    #  Loop over each row and form the combinations with other rows
    for (i in 1: (n_combinations-1)){

      #  Since there will be  2^n combinations, converting the row numbers to binary
      #  value will give us the same no of combination for each row.
      n_bits = intToBits(i)

      value = 0

      weight = 0

      elements = NULL

        for (j in 1: length(n_bits))

          {

          if (n_bits[j] == TRUE)

            {
              value = value + x$v[j]

              weight = weight + x$w[j]

              elements = c(elements, j)

             }#  End if (n_bits(j) == TRUE)

          }# End for (j in 1: intToBits(n_bits))

          if (weight<=W && value > maxValue){

              maxValue = value

              finalElements = elements

          }

        }# End for (i in 1: (n_combinations-1))

       return (list(value = round(maxValue), elements = finalElements))

  }# end if parallel == False

  else{

    #  Get the number of available cores (both logical and Physical)
    cores <- parallel::detectCores()

    #  Initiate cluster
    cluster <- parallel::makePSOCKcluster(cores)

    #  Export the data.frame variable x from current environment to all worker
    parallel::clusterExport(cluster, c("x", "W"), envir = environment())

    #  Get possible combinations of elements
    elements <- unlist( parallel::parLapply(cluster, 1:nrow(x), fun = function(ele_comb){utils::combn(rownames(x), ele_comb, paste, collapse = ";")}))

    #  Get all possible weights combinations
    weight <- unlist( parallel::parLapply(cluster, 1:nrow(x), fun = function(ele_comb){utils::combn(x$w, ele_comb, sum)}))


    #  Get possible values combinations
    values <- unlist(parallel:: parLapply(cluster, 1:nrow(x), fun = function(ele_comb){utils::combn(x$v, ele_comb, sum)}))


    #  Terminate the workers by stopping the cluster
    parallel::stopCluster(cluster)

    #  Get the weights that were less than the knapsack capacity
    posWeights <- which(weight <W)

    #  Get the maximum value that could be gained
    maxValue <- round(max(values[posWeights]))

    #  Identify the position of the elements which bags maximum values
    posMaxValue <- which (round(values) == maxValue)

    #  Separate the elements from their combinations
    posElements <- unique(as.numeric(unlist(strsplit(elements[posMaxValue],";"))))

    return(list(value = maxValue, elements = posElements))

  }

}
