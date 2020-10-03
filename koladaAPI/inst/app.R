  library(shiny)
  library(webAPI)
  library(dplyr)
  library(ggplot2)

  dim_municipality_data = getData("","","", url = "http://api.kolada.se/v2/municipality/0180,0001,0580,0581,1480,1280,2480,0380,1281")[,-1]

  dim_kpi_data = getData("","","", url = "http://api.kolada.se/v2/kpi/N00002,N00005Y,N00011,N00014,N00019,N00022,N00024,N00026,N00042,N00053")[,-1]

  fact_data = getData("","",url = "http://api.kolada.se/v2/data/kpi/N00002,N00005Y,N00011,N00014,N00019,N00022,N00024,N00026,N00042,N00053/municipality/0180,0001,0580,0581,1480,1280,2480,0380,1281/year/2010,2011,2012,2013,2014,2015,2016,2017,2018,2019")[,-1]

  join_fact_dim_temp = merge(fact_data,dim_kpi_data,by.x = c ("values.kpi"), by.y = c("values.id"), all = TRUE)

  join_fact_dim = merge(join_fact_dim_temp, dim_municipality_data, by.x = c("values.municipality"), by.y = c("values.id"))

  server <- function (input, output){

     df <- reactive({ if (length(input$m_names) == 0 )

                                      {

                                             vl_m = unique(join_fact_dim$values.title.y)


                                      }else

                                        {
                                              vl_m = input$m_names

                                        }

                              if (length(input$k_names)==0)

                                        {

                                              vl_k = unique(join_fact_dim$values.title.x)

                                        }else

                                          {

                                              vl_k = input$k_names

                                           }

                              if (length(input$year) == 0)

                                          {

                                               vl_y = unique(join_fact_dim$values.period)

                                          }else

                                            {
                                                  vl_y = input$year

                                            }
                             if (length(input$gender) == 0)

                                           {

                                                vl_g = unique(join_fact_dim$gender)

                                           }else

                                           {
                                                vl_g = input$gender

                                           }

                           return(

                                      distinct(select(filter(join_fact_dim, (values.title.y %in% vl_m)

                                                      &
                                                        (values.title.x %in% vl_k)

                                                      &
                                                         (values.period %in% vl_y)

                                                      &
                                                        (gender %in% vl_g)

                                         ), values.title.y,

                                            values.title.x,

                                            values.kpi,

                                            values.period,

                                            gender,

                                            value
                                         )
                                         )
                                    )
                           }
                           )
     output$bar1 <- renderPlot({

       ggplot(df(),
              aes(x = format(values.period,0), y = value, color = format(values.period,0),

                        fill = format(values.period,0), label = value)) +
         geom_col() +

         ggtitle("Yearly Distributionm") +

         xlab("Year") + ylab("Values") + labs(fill = "Year") + labs(color = "Year") +

         theme_bw() + theme(legend.position = "right") +

         theme(plot.title = element_text(hjust = 0.5))



       })


     output$bar2 <- renderPlot({


       ggplot(df(),
              aes(x = values.kpi, y = value, color = values.title.x,

                  fill = values.title.x, label = value)) +

         geom_col() +

         ggtitle("Distribution of KPI") +

         xlab("KPI") + ylab("Values") + labs(fill='KPI') +  labs(fill='KPI') + labs(color='KPI') +

         theme_bw() + theme(legend.position = "right") +

         theme(plot.title = element_text(hjust = 0.5))


     })

     output$bar3 <- renderPlot({


       ggplot(df(),
              aes(x = values.title.y, y = value, color = values.title.y,

                  fill = values.title.y, label = value)) +
         geom_col() +

         ggtitle("Distribution of Municipality") +

         xlab("Municipality") + ylab("Values") + labs(fill = "Municipality") + labs (color = "Municipality") +

         theme_bw() + theme(legend.position = "right") +

         theme(plot.title = element_text(hjust = 0.5))


     })

output$bar4 <- renderPlot({

    ggplot(data = df(), aes(x="", y=value, fill=gender))+
  
     geom_bar(width = 1, stat = "identity") + theme_bw()+
  
  coord_polar("y", start=0)+ggtitle("Gender Distribution") +

 theme(plot.title = element_text(hjust = 0.5))

 })
  }

  ui <- fluidPage(

     titlePanel( h2("Kolada API : KPI And Municipality Data Visualization", align = "center")),

      hr(),

      sidebarPanel(

          selectInput("m_names","Municipality Area",
                      as.vector(join_fact_dim$values.title.y), multiple = TRUE)
          ,

          selectInput("k_names","KPI", as.vector(join_fact_dim$values.title.x), multiple = TRUE)

          ,

          selectInput("year","Year", sort(as.vector(join_fact_dim$values.period)), multiple = TRUE)

          ,

          selectInput("gender","Gender", as.vector(join_fact_dim$gender), multiple = TRUE)

          ,

      ),


            mainPanel(

            plotOutput("bar2",height = 400)

            ),

            fluidRow(

                column(6,plotOutput("bar1",height = 250)),

                column(6,plotOutput("bar3",height = 250) )),

            plotOutput("bar4",height = 400),


)

  shinyApp(ui, server)
