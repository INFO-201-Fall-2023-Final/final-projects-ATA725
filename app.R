library(dplyr)
library(stringr)
library(ggplot2)
library(shiny)
library(plotly)

source("final_project.R")

# SHINY UI
ui <- navbarPage(inverse = TRUE, "Navigating the Electric Future",
                 
                 # First Page - Intro        
                 tabPanel("Intro",
                          fluidPage(h1("Navigating the Electric Future"),
                                    br(),
                                    p("Welcome to \"Navigating the Electric Future,\" a study of the relationship between electric vehicles (EVs) 
                                      and greenhouse gas (GHG) emissions in Washington State. 
                                      Understanding the impact of EV adoption is critical as climate change becomes an increasingly important issue. 
                                      We investigate two important datasets: the Electric Vehicle Population Data and the Greenhouse Gas Emissions Data. 
                                      Refer to our data documentation (label) and our analysis below, this Shiny program provides useful information for 
                                      policymakers and the general public by shedding light on the possible environmental benefits of EVs. "),
                                    br(),
                                    p("Expectations: We are expecting our research will investigate captivating visuals, in-depth analysis, and thought-provoking discoveries. 
                                      We give a complex story of the electric transition in transportation using a combination of Ben Jones' Seven Data Story Types."),
                                    br(),
                                    br(),
                                    fluidRow(
                                      column(12,offset = 2,
                                             img(src = "image.jpg",
                                                 height = 400,
                                                 width = 800))),
                                    br(),
                                    br(),
                                    br(),
                                    div(p(strong("Built by using the power of Rstudio and Shiny.")), 
                                        p(strong("R Packages:"), "ggplot2, shiny, wordcloud2, plotly, dplyr."),
                                        p(strong("Sources:"), a("data.gov.com", href = "https://data.gov/"), "for data,"),
                                        style="text-align: right;")
                          )
                 ),
                 
                 # Second Page       
                 tabPanel("The First Days of Washington's Electric Vehicles",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                               sliderInput("year",
                                                                           "Select year",
                                                                           2012, 2023,value = c(2012, 2023),
                                                                           sep = " "),
                                                               wellPanel(style = "background: white",
                                                                         h3("Info:"),
                                                                         textOutput("ev_growth_yr")
                                                                     
                                                               )
                                                               )
                                                  ),
                                                  
                                                  mainPanel(
                                                    p(strong(em("\"The First Days of Washington's Electric Vehicles\""), "- The Green Wave: Washington's Journey Towards Electric Vehicles")),
                                                    br(),
                                                    p("Let's analyze annual data to find important growth trends and areas where EV adoption is highest."),
                                                    br(),
                                                    plotOutput("EV_Growth_yr_Plot", width = "100%")
                                                  )
                          )
                          )
                 ),
                 
                 # Third Page        
                 tabPanel("A Comparison of Electric Vehicles and Greenhouse Gases",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                              selectInput("exact_year",
                                                                          "Select year",
                                                                          choices = 2012:2023,
                                                                          selected = 1),
                                                              wellPanel(style = "background: white",
                                                                        h3("Comparison:"),
                                                                        textOutput("ev_vs_ghg")
                                                                            
                                                                  )
                                                               )
                                                               ),
                                                  
                                                  mainPanel( 
                                                    p(strong(em("\"A Comparison of Electric Vehicles and Greenhouse Gases\""), "- The Balancing Act: EVs and Their Environmental Footprint")),
                                                    br(),
                                                    p("Let's find intriguing connections between EV prevalence and environmental damage."),
                                                    br(),
                                                    plotOutput("p", width = "100%")
                                                  )
                          )
                          )
                 ),
                 
                 # Fourth Page  
                 tabPanel("The Integration of Policy and Progress ",
                          fluidPage(sidebarLayout(position = "right",
                                                  sidebarPanel(style = "background: black",
                                                               wellPanel(style = "background: white",
                                                                         selectInput("exact_yr",
                                                                                     "Select year",
                                                                                     choices = 2013:2023,
                                                                                     selected = 1)),
                                                               wellPanel(style = "background: white",
                                                                         selectizeInput("city",
                                                                                     "Select city",
                                                                                     choices = unique_df$City,
                                                                                     selected = 1)),
                                                               wellPanel(style = "background: white",
                                                                         selectizeInput("county",
                                                                                     "Select county",
                                                                                     choices = unique_df$County,
                                                                                     selected = 1)),
                                                               wellPanel(style = "background: white",
                                                                         h4("Info:"),
                                                                         textOutput("ev_trends")),
                                                               wellPanel(style = "background: white",
                                                                         h4("Notes:"),
                                                                         p("The scatter plot suggests a rising trajectory in the number of electric vehicles. 
                                                                           The spread of points across the years indicates variability in adoption rates, potentially influenced by policy changes. Notably, there's an increase in the concentration of data points in later years, 
                                                                           hinting at a positive policy impact on EV adoption. 
                                                                           The `geom_point()` function in the code visually represents each year's total EV count, offering insights into policy effectiveness over time. 
                                                                           It's clear that policy initiatives may have acted as catalysts, accelerating the adoption of electric vehicles, as evidenced by the denser clusters of points post-policy introduction years."),
                                                                         )),
                                                  
                                                  mainPanel( 
                                                    p(strong(em("\"The Integration of Policy and Progress\""), "- Crossroads of Change: Policy Influence on EV Adoption and Emissions")),
                                                    br(),
                                                    p("Let's probe the relationships between public initiatives and policy shifts, 
                                                      as well as trends in EV uptake and greenhouse gas emissions."),
                                                    br(),
                                                    plotOutput("EV_Adoption_Plot", width = "100%")
                                                  )
                          )
                          )
                 ),
                 
                 # Fifth Page
                 tabPanel("Summary takeaways & About Page",
                          fluidPage(
                                                  br(),
                                                  p("Electric vehicles play an important role in environmental sustainability, and our Shiny app has revealed an important insight: 
                                                    lower greenhouse gas emissions are associated with more EV adoption in Washington. 
                                                    Important for environmental policymaking is this discovery, which came from looking at data from Washington State's EV Population Data 
                                                    and GHG Reporting Program. The combination of Shiny's interactive features with R code for data processing and visualization has proven 
                                                    the great utility of dynamic data presentation. It provides a captivating and easy way to explain complicated information. "),
                                                  br(),
                                                  #p("References: "),
                                                  #br(),
                                                  #br(),
                          )
                     )
                 
)


# SHINY SERVER

server <- function(input, output) {
  
##Part 1
  filtered_data <- reactive({
    req(input$year)
    filtered_data <- subset(EV_pop_df_yr, Model.Year >= input$year[1] & Model.Year <= input$year[2])
    return(filtered_data)
  })

  # Plot of EV Growth Over Years
  output$EV_Growth_yr_Plot <- renderPlot({
    ggplot(filtered_data(), aes(x = Model.Year, y = total_cars)) +
      geom_line() +
      labs(title = "EV Growth Over Years")
  }, height = 500, width = 800)
  
  # Description of plot of EV Growth Over Years 
  output$ev_growth_yr <- renderText({ 
    paste("From", input$year[1], "to", input$year[2], "the total number of electric cars is ", total_cars_yr(filtered_data(), input$year), ".")
  })

  
##Part 2
  # Plot of EV vs GHG
  output$p <- renderPlot({
    ggplot() +
    geom_line(data=filtered_data(), aes(x=Model.Year, y=total_cars), color="blue") +
    geom_line(data=GHG_em_yr, aes(x=Year, y=total_emissions / ratio), color="red", linetype="dashed") +
    scale_y_continuous(name="Total EVs",
                       sec.axis=sec_axis(~ . * ratio, name="Total Emissions (MTCO2e)")) +
    labs(title="EV Adoption vs GHG Emissions Over Time", x="Year")
  })
  
  # Description of Comparison
  output$ev_vs_ghg <- renderText({ 
    paste("In", input$exact_year, "the total number of electric cars is ", total_cars_in_yr(EV_pop_df_yr, input$exact_year), 
          "and the total green house gas emissions is ", total_em_in_yr(GHG_em_yr, input$exact_year),".")
  })
  

##Part 3
  output$ev_trends <- renderText({ 
    paste("For", input$exact_yr, "the total number of electric cars comparing to the year before is ", 
          ev_trend_fun(EV_pop_df_region, as.numeric(input$exact_yr), input$city, input$county), ".")
  })
  
  output$EV_Adoption_Plot <- renderPlot({
  ggplot(EV_pop_df_region, aes(x=Model.Year, y=total_cars)) +
    geom_point() +
    labs(title="EV Adoption Trends Over Time")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

