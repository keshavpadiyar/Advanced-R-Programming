create_fridge <- function(number_users){

  stopifnot(inherits(number_users,"numeric"))

  food_list <- 1:10

  list_fridge <- list()

  for (i in 1: number_users){

    list_fridge[[i]] <-  data.frame(

                                  food_item_id =  1:5,

                                  food_item_quantity = sample.int(10, 5),

                                  food_prohibited_combo = sample(1:5)


                                    )



  }

  class(list_fridge) <- c("refrigerator")

  return(list_fridge)
}

build_diet_list <- function (object){

  for (i in 1:length(object)){

    total <- sum(object[[i]]$food_item_quantity)

    diet <- round((object[[i]]$food_item_quantity*total)/100)

    #revised_stock <- object[[i]]$food_item_quantity - diet

    object[[i]]$diet <- rep(sum(diet),nrow(object[[i]]))
  }

  return(object)

}


update_food_list <- function (object, user_id, food_id, update_max=-1, update_conflicting_food = 0){

  stopifnot(
              inherits(object, "refrigerator") &&
              inherits(user_id, "numeric") &&
              inherits(food_id, "numeric") &&
              inherits(update_max, "numeric")
            )


  if (!length(unlist(dplyr::filter(object[[user_id]], food_item_id  %in%  food_id)))){

        cat("For the User: ",user_id,"Item -",food_id," not found in the refrigerator!\n")

        #flag <- readline(" To add new item enter: N")

        #if (flag == "N"){

          #print(flag)

       # }

  }else if(update_max > -1 ){


    object[[user_id]][which(object[[user_id]][,1] %in% food_id),2]<-update_max

    cat("Item updated for :",user_id,"\n")


  }else if (update_max < -1){

    print("No item quantity is updated !")

  } else if ( any(as.vector(update_conflicting_food) != 0)){

    for (i in update_conflicting_food){

      if (length(unlist(dplyr::filter(object[[user_id]],abs(i) %in% food_item_id)))){

           if (i < 0){

             object[[user_id]] <-  subset(object[[user_id]], !food_item_id %in% abs(i))

             cat("Removing the Item",abs(i),"\n")

           }else{

             print("Item already exists\n")

           }

      }else if (i<0){

            print("Item not found in the fridge\n")

      }else{

        object[[user_id]]<- rbind(object[[user_id]], c(i,update_max, NULL))

        }

    }

  }


  return(object)


}

request_food <- function(object, user_id, food_ids){

  stopifnot(
    inherits(object, "refrigerator") &&
      inherits(user_id, "numeric") &&
      inherits(food_id, "integer")
  )

  if (!length(unlist(dplyr::filter(object[[user_id]], food_item_id %in% food_id)))){

    cat("For the User: ",user_id,"Item -",food_id," not found in the refrigerator!\n")

  }else if (sum(unlist(dplyr::filter(object[[user_id]],food_item_id %in% food_id))) > object[[user_id]]$diet[1] ){

    print("Requested Food Excceding the total daily limit!!")

  }else if (any(unlist(dplyr::filter(object[[user_id]],food_item_id %in% food_id))$food_prohibited_combo == food_id )){

    stop("Requested Food cannot be dispensed as its restricted to have these combinations!!")

  }


  else{

  current_count <- object[[user_id]][which(object[[user_id]][,1] %in% food_id),2] -1

  if (any(current_count<0)){

    stop("Requested Item Unavailable!!")


  }else{

  object[[user_id]][which(object[[user_id]][,1] %in% food_id),2] <- current_count

  object[[user_id]]$diet <- object[[user_id]]$diet - length(food_id)

  }
}

  return(object)

}

