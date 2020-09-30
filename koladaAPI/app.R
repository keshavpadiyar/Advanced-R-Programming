library(shiny)
library(webAPI)
library(dplyr)


municipality_data = getData("","","", url = "http://api.kolada.se/v2/municipality?all")

kpi_data = getData("","","", url = "http://api.kolada.se/v2/kpi?all")

server <- function (input, output, session){

    m_area <- renderPrint ({

        if(length(input$m_names)==0 && length(input$radio)==0)

        {

            return(paste(as.character(municipality_data[[2]])
                         ,collapse =",", sep = "" )
            )
        }else if (length(input$m_names)==0 && length(input$radio)!=0)

        {

            return(paste(as.character(filter(municipality_data,
                                             values.type %in% input$radio) [[2]]),
                         collapse =",", sep = "" )
            )
        } else if (length(input$m_names)!=0 && length(input$radio)==0)

        {

            return(paste(as.character(filter(municipality_data,
                                             values.title %in% input$m_names) [[2]]),
                         collapse =",", sep = "" )
            )
        } else{

            paste(as.character(filter (municipality_data,
                                       (values.title %in% input$m_names)
                                       &
                                           (values.type %in% input$radio) )[[2]]),
                  collapse=", ",sep="")
        }

    })


    l_kpi <- renderPrint ({

        if(length(input$k_names)==0)

        {

            return(paste(as.character(unique(kpi_data[[5]]))
                         ,collapse =",", sep = "" )
            )
        }else{

            as.vector(paste(as.character(unique(filter (kpi_data,
                                                        values.title %in% input$k_names)[[5]])),
                            collapse=", ",sep=""))
        }

    })


    output$value <- renderPrint({ input$radio })


    l_year <- renderPrint(paste(as.character(input$year),collapse = ",", sep = ""))


    url <- reactive({gsub(" ",'',gsub('"','',gsub( "[[^]]*]",'',paste('"http://api.kolada.se/v2/data/kpi"',
                                                            sub("[1]",'',l_kpi()), "municipality",
                                                            sub("[1]",'',m_area()), '"year"',
                                                            sub("[1]",'',l_year()),
                                                            collapse = "/", sep = "/"))))})

    output$url <- renderTable({getData("","","", url = url())[,-1]})


    #output$text <- url



}

ui <- fluidPage(

    h2("Kolada API : KPI And Municipality Data Visualization"),

    hr(),

    sidebarPanel(

        selectInput("m_names","Municipality Area",
                    as.vector(municipality_data$values.title), multiple = TRUE)
        ,

        selectInput("k_names","KPI", as.vector(kpi_data$values.title), multiple = TRUE)

        ,

        selectInput('year', 'year',seq(2000,2050,1), multiple = TRUE)

        ,

        radioButtons("radio","Select Landsting or Kommun",
                     choices = unique(as.vector(municipality_data$values.type)),
                     selected = 1)
    ),

    column(4,tableOutput("url"))
)


shinyApp(ui, server)
