  library(shiny)
  library(SwedishKoladaAPI)
  library(dplyr)
  library(ggplot2)

  dim_municipality_data = getKoladaAPIData(url = "http://api.kolada.se/v2/municipality?all")

  dim_kpi_data = getKoladaAPIData(url = "http://api.kolada.se/v2/kpi?all")

  fact_data = getKoladaAPIData(l_kpi ="N00002,N00005Y,N00011,N00014,N00019,N00022,N00024,N00026,N00042,N00053",
                                  l_m = "0180,0001,0580,0581,1480,1280,2480,0380,1281",
                                   l_year = "2010,2011,2012,2013,2014,2015,2016,2017,2018,2019")

  join_fact_dim_temp = merge(fact_data,dim_kpi_data,by.x = c ("KPI_ID"), by.y = c("id"), all = TRUE)

  join_fact_dim = merge(join_fact_dim_temp, dim_municipality_data, by.x = c("Municipality_ID"), by.y = c("id"))

  #join_fact_dim$Gender = sapply(join_fact_dim$Gender, switch, K = "Female", M = "Male", T = "Total")

  server <- function (input, output){

     df <- reactive({ if (length(input$m_names) == 0 )

                                      {

                                             vl_m = unique(join_fact_dim$title.y)


                                      }else

                                        {
                                              vl_m = input$m_names

                                        }

                              if (length(input$k_names)==0)

                                        {

                                              vl_k = unique(join_fact_dim$title.x)

                                        }else

                                          {

                                              vl_k = input$k_names

                                           }

                              if (length(input$year) == 0)

                                          {

                                               vl_y = unique(join_fact_dim$Year)

                                          }else

                                            {
                                                  vl_y = input$year

                                            }
                             if (length(input$Gender) == 0)

                                           {

                                                vl_g = unique(join_fact_dim$Gender)

                                           }else

                                           {
                                                vl_g = input$Gender

                                           }

                           return(

                                      distinct(select(filter(join_fact_dim, (title.y %in% vl_m)

                                                      &
                                                        (title.x %in% vl_k)

                                                      &
                                                         (Year %in% vl_y)

                                                      &
                                                        (Gender %in% vl_g)

                                         ), title.y,

                                            title.x,

                                            KPI_ID,

                                            Year,

                                            Gender,

                                            value
                                         )
                                         )
                                    )
                           }
                           )

     output$bar1 <- renderPlot({

       ggplot(df(),
              aes(x = format(Year,0), y = value, color = format(Year,0),

                        fill = format(Year,0), label = value)) +
         geom_col() +

         ggtitle("Yearly Distributionm") +

         xlab("Year") + ylab("Values") + labs(fill = "Year") + labs(color = "Year") +

         theme_bw() + theme(legend.position = "right") +

         theme(plot.title = element_text(hjust = 0.5))



       })


     output$bar2 <- renderPlot({


       ggplot(df(),
              aes(x = KPI_ID, y = value, color = title.x,

                  fill = title.x, label = value)) +

         geom_col() +

         ggtitle("Distribution of KPI") +

         xlab("KPI") + ylab("Values") + labs(fill='KPI') +  labs(fill='KPI') + labs(color='KPI') +

         theme_bw() + theme(legend.position = "right") +

         theme(plot.title = element_text(hjust = 0.5))


     })

     output$bar3 <- renderPlot({


       ggplot(df(),
              aes(x = title.y, y = value, color = title.y,

                  fill = title.y, label = value)) +
         geom_col() +

         ggtitle("Distribution of Municipality") +

         xlab("Municipality") + ylab("Values") + labs(fill = "Municipality") + labs (color = "Municipality") +

         theme_bw() + theme(legend.position = "right") +

         theme(plot.title = element_text(hjust = 0.5))


     })

    output$bar4 <- renderPlot({

        if (nrow(df())==0){

          return("No Values To Display")

        }else{

              ggplot(data = df(), aes(x="", y=value, fill=Gender))+

              geom_bar(width = 1, stat = "identity") + theme_bw()+

              coord_polar("y", start=0)+ggtitle("Gender Distribution") +

              theme(plot.title = element_text(hjust = 0.5))

        }

     })
  }

  ui <- fluidPage(

     titlePanel( h2("Kolada API : KPI And Municipality Data Visualization", align = "center")),

      hr(),

      sidebarPanel(

          selectInput("m_names","Municipality Area",
                      as.vector(join_fact_dim$title.y), multiple = TRUE)
          ,

          selectInput("k_names","KPI", as.vector(join_fact_dim$title.x), multiple = TRUE)

          ,

          selectInput("year","Year", sort(as.vector(join_fact_dim$Year)), multiple = TRUE)

          ,

          selectInput("Gender","Gender", sort(as.vector(join_fact_dim$Gender)), multiple = TRUE)

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
