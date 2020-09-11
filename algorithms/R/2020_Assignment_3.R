#' Euclidean algorithm to find the greatest common divisor of two numbers
#'
#' @param int1 Input 1 - Scalar Integer value
#' @param int2 Input 2 - Scalar Integer value
#'
#' @return Returns the greatest common divisor of \code{int1} and \code{int2}
#' @export
#' 
#' @details {This function finds greatest common divisor of the given 2 numbers. If inputs are not numeric scalar,
#' the program returns error
#'
#'For additional information click on the below URL
#
#'\url{https://en.wikipedia.org/wiki/Euclidean_algorithm}
#'}
#'
#' @examples
#' \dontrun{
#' euclidean(123612, 13892347912)
#' 4}
#' 
euclidean <- function (int1,int2){
  stopifnot((is.atomic(int1))&&(is.atomic(int2)&&(is.numeric(int1))&&(is.numeric(int2))))
#' If the user inputs decimal values, the program rounds it to the integer.
  int1 = round(int1,0)
  int2 = round(int2,0)
  if (int2 == 0){
    return(int1)
  }else{
    return(euclidean(int2, int1%%int2))
  }
}


#' Find the shortest path Between vertices using Dijkstra's algorithm
#'
#' @param data Data frame consisting vertices and distance between them 
#' @param node Initial Node/vertex from which the distance is to be calculated 
#'
#' @return Shortest distance from the initial node/Vertex to all other vertices
#' @export
#'
#' @details {This function finds the shortest distance from the input vertex to all other vertices.
#'    
#'For additional information click on the below URL
#'
#'\url{https://en.wikipedia.org/wiki/Graph_(mathematics)}
#'}
#'
#' @examples
#'\dontrun{ 
#'wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), 
#'                          v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'                          w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#'dijkstra(wiki_graph,1)
#' 0 7 9 20 20 11}
#'
dijkstra <- function(data, node) {
#' validate the input data is a data frame of 3 columns
  stopifnot((is.data.frame(data)) && (ncol(data)==3))
#' Convert the given data frame into Matrix, to traverse through all possible path and get the shortest one
  vl_vertices <- unlist(unique(data[1]))
  N <- length(vl_vertices)
  mat_graph <- matrix(0, nrow = N, ncol = N, byrow = FALSE)
  for (i in 1:N) {
    vl_temp <- subset(data, data[1] == i)
    for (j in 1:nrow(vl_temp)) {
      x <- unlist((vl_temp[j, ]))
      mat_graph[x[1], x[2]] <- x[3]
    }
  }
#' Assign the unknown distances as Infinity.
  mat_graph[mat_graph == 0] <- Inf
  vl_weight <- replicate(N, Inf)
  vl_traversed <- replicate(N, 0)
#' Initialize the distance from the required node to itself as Zero
  vl_weight[node] <- 0
#' Run the loop till all the vertices/Nodes are not traversed
  while (sum(vl_traversed) < N) {
    vl_edges <- replicate(N, Inf)
    for (i in 1:N) {
      if (vl_traversed[i] == 0) {
        vl_edges[i] <- vl_weight [i]
      }
    }
    vl_currentWeight <- min(vl_edges)
    vl_currentPoint <- which.min(vl_edges)
    
    for (i in 1:N) {
      vl_newweight <- vl_currentWeight + mat_graph [vl_currentPoint, i]
      if (vl_newweight < vl_weight[i]) {
        vl_weight[i] <- vl_newweight
      }
    }
    vl_traversed[vl_currentPoint] <- 1
  }
  return(vl_weight)
}