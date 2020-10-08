brute_force_knapsack <- function (x, W, parallel = FALSE){



  stopifnot(class(x)=="data.frame"   &&
              class(W)=="numeric"    &&
              colnames(x)[1]=="w"    &&
              colnames(x)[2]=="v"    &&
              W > 0
            )


  if (parallel == FALSE){

    maxValue = 0

    n_combinations = 2^nrow(x)

    for (i in 1: (n_combinations-1)){

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

       return (list(value = round(maxValue), element = finalElements))

  }# end if parallel == False

  else{

    library(parallel)

    #  Get the number of available cores (both logical and Physical)
    cores <- detectCores()

    #  Initiate cluster
    cluster <- makeCluster(cores)

    #  Export the data.frame variable x from current environment to all worker
    clusterExport(cluster, c("x"), envir = environment())

    #  Export the library parallel to all worker
    clusterEvalQ(cluster, {library(parallel)})

    #  Get possible combinations of elements
    elements <- unlist( parLapply(cluster, 1:nrow(x), fun = function(ele_comb){utils::combn(rownames(x), ele_comb, paste, collapse = ";")}))

    #  Get all possible wights combinations
    wights <- unlist( parLapply(cluster, 1:nrow(x), fun = function(ele_comb){utils::combn(x$w, ele_comb, sum)}))

    #  Get possible values combinations
    values <- unlist( parLapply(cluster, 1:nrow(x), fun = function(ele_comb){utils::combn(x$v, ele_comb, sum)}))

    #  Terminate the workers by stopping the cluster
    stopCluster(cluster)

    posWeights <- which(wights <W)

    maxValue <- round(max(values[posWeights]))

    posMaxValue <- which (round(values) == maxValue)

    posElements <- unique(as.numeric(unlist(strsplit(elements[posMaxValue],";"))))

    return(list(value = maxValue, element = posElements))

  }

}

brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)

brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)

brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)

brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)


system.time(brute_force_knapsack(x = knapsack_objects[1:32,], W = 3500, parallel = TRUE))



#' Checking Time taken to Execute for n = 16
#' \dontrun{
#' system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))
#' Output
#' user  system elapsed
#' 2.84    2.23    5.75
#' }

