create_toy_store <- function(max_number_toys){

    if (class(max_number_toys) != "numeric" ||
        length(max_number_toys)>1 ||
        max_number_toys <=0){

          message("Error: Invalid Input!")
          stop()

    }## End Input validator If

    df_toyStore = data.frame()

    v_toys = c("Action Figure" = 250, "Cars" = 100, "Planes" = 500, "Cards" = 50, "Puzzeles" = 150)

    age_group = c("7-14", "3-14", "12-25", "3-9", "6-12")

    for (i in 1:max_number_toys){

      k = sample(5,1)

      df_toyStore <- rbind(df_toyStore, c(i,
                                          paste(names(v_toys[k]),"-",i),
                                          v_toys[k],
                                          as.numeric(unlist(strsplit(age_group[k],"-"))[1]),
                                          as.numeric(unlist(strsplit(age_group[k],"-"))[2])
                                          )
                          )
    }

    colnames(df_toyStore) <- c("item_id", "item_name", "item_price","min_age","max_age")

    toystore_object <- list(

                           toy_store <- df_toyStore,

                           toy_store_size <- max_number_toys

                           )
    class(toystore_object) <- c("toystore")

    return(toystore_object)

}##  End of  create_toy_store

## Add method add_to_toy_store to toy_Store class

add_to_toy_store <- function (object, item_name, max_item_cost, min_age, max_age, ...)UseMethod("add_to_toy_store")
add_to_toy_store.toystore <- function (object, item_name, max_item_cost, min_age, max_age){

  if (class(object)!= "toystore" ||
      (class(item_name)!="character" || length(item_name)>1) ||
      (class(max_item_cost)!="numeric" || length(max_item_cost)>1) ||
      (class(min_age)!="numeric" || length(min_age)>1)||
      (class(max_age)!="numeric" || length(max_age)>1)
      ){

      message("Error: Invalid Input!")
      stop()

  }## End input validation If

  if (nrow(object[[1]])<object[2]){

    object[[1]] <- rbind(object[[1]], c(as.numeric(max(row.names(object[[1]])))+1,

                                        item_name,

                                        min_age,

                                        max_age

                                        )
                        )# End Rbind


  }## End if validate toystore size
  else{

    message("Error: No space to add additional toys!")
    stop()

  }

}

## Adding method recommend_toy to the toystore class

recommend_toy <- function(object, age, max_item_cost, ...)UseMethod("recommend_toy")
recommend_toy.toystore <- function (object, age, max_item_cost){

  if (class(object)!= "toystore" ||
      (class(max_item_cost)!="numeric" || length(max_item_cost)>1) ||
      (class(age)!="numeric" || length(age)>1)
  ){

    message("Error: Invalid Input!")
    stop()

  }## End input validation If
   else{

     object[[1]] = transform(object[[1]], item_price = as.numeric(item_price ),
                                          min_age = as.numeric(min_age),
                                          max_age = as.numeric(max_age)
                             )

     df <- dplyr::filter(object[[1]], ((item_price <= max_item_cost) & ((min_age <= age)&(max_age >=age))))

     item_count <- nrow(df)

     if (item_count>1){

           cat("Recommended Toys: \n")

           base::print(df[,2:3], row.names = FALSE)

     }else if (item_count == 1){

           cat("Item is sold! \n")

           object[[1]] <- subset(object[[1]], !item_id %in% unlist(df[1]))

     }else{

       message("Item not available in the store!")
       stop()

     }

     return(object)

   }## end else
}##  End method recommend_toy

##  Override print method for toystore
print.toystore <- function(object){

  if (class(object)!= "toystore"
  ){

    message("Error: Invalid Input!")
    stop()

  }## End input validation If


  cat("Total Capacity of the toy store:", unlist(object[2])," toys\n")

  cat("\n current stock in the store: \n")

  base::print(object[[1]][,2:5], row.names = FALSE)

}## End print Method
