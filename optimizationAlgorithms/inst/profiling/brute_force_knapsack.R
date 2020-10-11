profvis::profvis({


  set.seed(42)
  n <- 2000
  knapsack_objects <-
    data.frame( w=sample(1:4000, size = n, replace = TRUE),
                v=runif(n = n, 0, 10000) )

  x <- knapsack_objects[1:20,]

  W <- 3500

  parallel = TRUE

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

       print (list(value = round(maxValue), elements = finalElements))

  }# end if parallel == False

  else{

    #  Get the number of available cores (both logical and Physical)
    cores <- parallel::detectCores()

    #  Initiate cluster
    cluster <- parallel::makePSOCKcluster(cores)

    #  Export the data.frame variable x from current environment to all worker
    parallel::clusterExport(cluster, c("x"), envir = environment())

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
    posMaxValue <- min(which (round(values) == maxValue))

    #  Separate the elements from their combinations
    posElements <- unique(as.numeric(unlist(strsplit(elements[posMaxValue],";"))))

    print(list(value = maxValue, elements = posElements))

  }
}
)
