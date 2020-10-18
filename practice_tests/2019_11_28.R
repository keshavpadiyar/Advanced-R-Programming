##  Creating a S3 class
create_wardrobe <- function(max_number_clothes){

  if (!(class(max_number_clothes)=="numeric") || length(max_number_clothes)>1){

    message("Error: Incorrect value entered. Please enter atomic integer")
    stop()
  }

  wardrobe_item <- data.frame()

  weather_list <- c("spring","summer", "fall", "winter")

  clothes_list <- c("T shirt", "vest", "sweatshirt", "jacket")

  for (i in 1:max_number_clothes){

    wardrobe_item <- rbind(wardrobe_item, c(item_id = i,
                             item_name = clothes_list [sample.int(4,1)],
                             weather_name = weather_list [sample.int(4,1)]
                             )
                           )


  }

  colnames(wardrobe_item) <- c("item_id", "item_name", "weather_name")

  class_object <- list(wardrobe <- wardrobe_item,
                       wardrobe_Size <- max_number_clothes )

  class(class_object) <- c("wardrobe")

  return(class_object)

}

## Creating a method for wardrobe class
add_to_wardrobe <- function(object, item, weather, ...)UseMethod("add_to_wardrobe")

add_to_wardrobe.wardrobe <- function(object, item, weather){

  if (class(object)!= "wardrobe" ||

       (class(item) != "character" && length(item)>1) ||

       (class(weather) != "character" && length(weather)>1)

  ){

    message("Error: Invalid Input!")

    stop()

  }

    if (nrow(object[[1]]) < object[2]) {

      object[[1]] <- rbind(object[[1]], c(as.numeric(row.names(object[[1]])[nrow(object[[1]])])+1,

                                          item,

                                          weather)

                                          )

    }else{

      message("Wardrobe is full, cannot add additional item!")
      stop()
    }

  return (object)
}

## adding obtain_clothes method to wardrobe class
obtain_clothes <- function(object, weather, quantity, ...)UseMethod("obtain_clothes")

obtain_clothes.wardrobe <- function(object, weather, quantity){


  if (class(object)!= "wardrobe" ||

      (class(weather) != "character" && length(item)>1) ||

      (class(quantity) != "numeric" && length(weather)>1)

  ){

    message("Error: Invalid Input!")

    stop()

  }

  df = dplyr::filter(object[[1]], weather_name %in% weather)

  if (nrow(df)==0){

    message("No item found for this weather!")
    stop()

  }else if(nrow(df)<quantity){

    message("Requested quantity not available in the wardrobe!")
    stop()

  }else{

    df = head(df,quantity)

    object[[1]] = subset(object[[1]], !(object[[1]]$item_id %in% df$item_id))

  }

  return (object)
}

## adding print method to wardrobe

print.wardrobe <- function(object){

  cat("Total Capacity of the Wardrobe: ",unlist(object[2]),"items\n")

  cat("Weather wise Items in the Wardrobe:\n")

  df = object[[1]][order(object[[1]]$weather_name),]

  base::print(df[,3:2], row.names=FALSE)

}

#future_wardrobe <- create_wardrobe(max_number_clothes=5)
#print(future_wardrobe)
#future_wardrobe <-add_to_wardrobe(future_wardrobe,"raincoat","rain")
#future_wardrobe <-obtain_clothes(future_wardrobe,"rain",5)
#future_wardrobe <-obtain_clothes(future_wardrobe,"winter",2)
#print(future_wardrobe)

## Question 3:

input_vector <- c(1,2,3,4,5,6,7,8,9)
row_dim <- 3
col_dim <- 3
by_row <- TRUE

mat <- matrix()

temp = c()

k = 1

for (i in 1:row_dim){

     for (j in 1: col_dim){

        temp[j] = (as.vector(input_vector[k]))

       k = k+1
     }

    #print(paste(temp, collapse = " "))
    print(temp)

  }

