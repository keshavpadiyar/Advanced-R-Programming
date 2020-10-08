

##  Greedy Knapsack
greedy_knapsack <- function (x, W){

  stopifnot(class(x)=="data.frame" &&
            class(W)=="numeric"    &&
            colnames(x)[1]=="w"    &&
            colnames(x)[2]=="v"
            )

  if (W==0){

    return(list(value = 0, element = 0))

  }

  v = 0
  w = c()


  x$vperW <- x$v/x$w

  x <- x[order(x$vperW, decreasing = TRUE),]

  rowName <- row.names(x)

  for(i in 1:nrow(x)){

    if ((W >= 0) & (x$w[i]<=W)){

        W = W - x$w[i]

        v = v + x$v[i]

        w[i] = as.numeric(rowName[i])

    }else{

      break

    }

  }

  return(list(value = round(v), element = w))
}

greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

#' Checking Time taken to Execute for n = 1000000
#' \dontrun{
#' system.time(greedy_knapsack(x = knapsack_objects[1:1000000,], W = 3500))
#' Output
#' user  system elapsed
#' 1.47    0.08    1.56
#' }


