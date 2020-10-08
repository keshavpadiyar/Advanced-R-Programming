knapsack_dynamic <- function (x, W, parallel = FALSE){



  stopifnot(class(x)=="data.frame"   &&
              class(W)=="numeric"    &&
              colnames(x)[1]=="w"    &&
              colnames(x)[2]=="v"
             )

  num_row = nrow(x)

  mat_dynamic <- matrix(0,nrow = num_row+1, ncol = W+1)

  for (i in 1:num_row+1){

    for (j in 1:W+1){

      if (i==1 || j==1){

        mat_dynamic[i,j] = 0

        #print(mat_dynamic)

      }else if (x$w[i-1] <= j){

        mat_dynamic[i,j] = max(x$v[i-1] + mat_dynamic[i-1, j-x$w[i-1]], mat_dynamic[i-1, j])
        #print(mat_dynamic)

      }else{

        mat_dynamic[i,j] = mat_dynamic[i-1,j]
        #print(mat_dynamic)

      }


    }# End for 1:W

  }# End for 1:num_row

  i = num_row

  j = W+1

  elements = c()

  value = 0

  k = 1

  while (i>1 && j>1){

    if (mat_dynamic[i,j] == mat_dynamic[i-1,j]){

        i <- i-1

    }else{

      elements[k] <- i

      k <- k+1

      value <- value + x[i,2]

      j <- j - x[i,1]

      i <- i - 1

    }

  }

  return(list( value = round(mat_dynamic[length(x[[1]]), (W+1)]), elements = sort(elements)) )
}

x = knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)

