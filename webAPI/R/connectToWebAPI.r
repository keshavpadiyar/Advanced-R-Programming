#' url Constructor
#'
#' @param l_kpi : Selected KPI codes
#' @param l_m  : Selected Municipality Codes
#' @param l_year : Required Year
#' @param url  : Required URL to retrieve the data from
#'
#' @return : returns the complete constructed URL
#'
#' @details : This function will take the user input to construct an URL.
#'            Features: 1. User must input kpi code, municipality code and the year to fetch the data related to KPI vs Municipality for mentioned years.
#'                      2. Or, the User can give URL to fetch the data directly from the given URL.
#'            Note: if user inputs all the parameters then the data will be retrieved only from from the URL the user proveds.
#'
#' @examples
#'
#' \dontrun{
#'
#' urlConstructor("","","", url = "http://api.kolada.se/v2/municipality?all")
#' http://api.kolada.se/v2/municipality?all
#'
#' urlConstructor(l_m = "0180,0581,0580",l_kpi = "N01951,N61714",l_year = "2018,2019,2020", url = "")
#' http://api.kolada.se/v2/data/kpi/N01951,N61714/municipality/0180,0581,0580/year/2018,2019,2020
#'
#' #' urlConstructor(l_m = "0180,0581,0580",l_kpi = "N01951,N61714",l_year = "2018,2019,2020", url = "http://api.kolada.se/v2/municipality?all")
#' http://api.kolada.se/v2/municipality?all
#' }
urlConstructor <- function (l_kpi,l_m, l_year, url){

  base_url = "http://api.kolada.se/v2"

  if (!identical(url,"")){

    return(url)

  } else if (identical(l_kpi, "")) {

    warning(" Please select KPI")

  }else if (identical(l_m, "")){

    warning(" Please select the area Municipality")

  }else if (identical(l_year, "")){

    warning(" Please select the Year")

  } else{

    url = paste(base_url,"data/kpi",l_kpi,"municipality",l_m,"year",l_year, sep = "/")

    return(url)
  }

}

#' Get API data
#'
#' @param url : The URL to the API to fetch the data
#'
#' @return : Returns the object with details pertaining to accessing the API, contains the data and metadata
#'
#' @details :  This function tries to access the API from the input URL using the GET command httr package, and prints the status for the user.
#'             If the presented URL is invalid, the function throws the warning message and halts the execution
#'
#' @import httr
#'
#' @examples
#'
#' \dontrun{
#' url = "http://api.kolada.se/v2/data/kpi/N01951,N61714/municipality/0180,0581,0580/year/2018,2019,2020"
#'
#' response = getAPI(url)
#'
#' GET URL STATUS:
#' Response http://api.kolada.se/v2/data/kpi/N01951,N61714/municipality/0180,0581,0580/year/2018,2019,2020
#' Date: 2020-09-29 12:27
#' Status: 200
#' Content-Type: application/json; charset=utf-8
#' Size: 1.56 kB
#'
#' }
getAPI <- function(url){

  out <- GET(url = url)

  cat("GET URL STATUS: \n")

  print(out)

  warn_for_status(out)

  stop_for_status(out)

  return(out)
}


#' Verify the Content
#'
#' @param response : Response from the getAPI function, this will contain the object of the API.
#'
#' @return : Json data level 1 flattened to Data.Frame
#'
#' @import jsonlite
#'
#' @details : This function takes the response from the get API function, validates if the response has data content or not, finally parses the input json content and flatten the level 1 content.
#'
verifContent <- function(response){

  if (content(response)$count ==0) warning("No Content to Parse")

  vl_content <- content(response, "text", encoding = "UTF-8")

  fromJSON(vl_content, flatten = TRUE)
}


#' Flatten the JSON File
#'
#' @param df : level 1 flattened data from the verifContent function
#'
#' @param df_data : Empty data frame for shuffling the data
#'
#' @return : Completely flattened json data as data.frame
#'
#' @details : This function takes the data.frame with the list type and  flattens it into rows and column format
#'
flattenJSON <- function(df, df_data){

  temp = sapply(df, class)

  lnested = structure( which(temp=="list"), names = c( names(which(temp=="list"))))

  if(length(lnested)==0) return(df)

  for(i in 1:length(lnested)){

    for (j in 1:nrow(df)){

      df_temp = cbind (df[j,-lnested[i]],data.frame(Reduce(rbind, df[j,names(lnested[i])])),  row.names = NULL)

      df_data = rbind(df_temp,df_data)
    }

  }

  flattenJSON(df_data, df_data)
}


#' User function to get the data from API
#'
#' @param l_kpi : Required KPI Code
#'
#' @param l_m : Required Municipality Code
#'
#' @param l_year : Required Year
#'
#' @param url : If data to be fetched from a specific URL
#'
#' @return : data.frame having the requested data from the API
#'
#' @export
#'
#' @details : This function to be called from the package to access the API, parse and download the data. The requested data will be downloaded as CSV file
#'
#' @examples
#' \dontrun{
#'
#' final_data = getData(l_m = "0180,0581,0580",l_kpi = "N01951,N61714",l_year = "2018,2019,2020", url = "")
#' GET URL STATUS:
#' Response http://api.kolada.se/v2/data/kpi/N01951,N61714/municipality/0180,0581,0580/year/2018,2019,2020
#' Date: 2020-09-29 12:27
#' Status: 200
#' Content-Type: application/json; charset=utf-8
#' Size: 1.56 kB
#'
#'
#' }
#'
getData <- function (l_kpi,l_m,l_year, url){

  url = urlConstructor(l_kpi,l_m, l_year, url)

  response = getAPI(url)

  vl_content  = as.data.frame(verifContent(response))

  df_data = vl_content[FALSE,]

  df_data = flattenJSON(vl_content,df_data)

  return(df_data)

}

#m_data = getData("","","", url = "http://api.kolada.se/v2/municipality?all")

#kpi_data = getData("","","", url = "http://api.kolada.se/v2/kpi_groups")

#final_data = getData(l_m = "0180,0581,0580",l_kpi = "N01951,N61714",l_year = "2018,2019,2020", url = "")

#write.csv(df_data,"data.csv", row.names = FALSE,col.names = TRUE, sep = ',')
