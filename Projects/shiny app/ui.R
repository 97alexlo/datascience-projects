#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)
################################################################################################################
################## shiny url: https://shiny.rcg.sfu.ca/u/ala148/shinyapp/ #########################################
################################################################################################################
shinyUI(fluidPage(

  titlePanel("BC Climate Data from 1979 - 2017"),
    mainPanel(width=10,
      tabsetPanel(
        tabPanel("About",htmlOutput("about")),
                  tabPanel("BC climate data analysis", 
                           sidebarLayout(
                           sidebarPanel(
                             selectInput("dataSource", "Please a select a plot:",
                                         c("Temperature over time" = "temp",
                                           "Snowfall over time" = "snow",
                                           "CO2 levels over time" = "co2")),
                             checkboxInput(inputId="templin",c("Add a linear regression line to the temperature plot"),FALSE),
                             checkboxInput(inputId="temploess",c("Add a loess regression curve to the temperature plot"),FALSE),
                             checkboxInput(inputId="tempavg",c("Add a long term average line to the temperature plot"),FALSE),
                             checkboxInput(inputId="snowloess",c("Add a loess regression curve to snowfall plot"),FALSE),
                             checkboxInput(inputId="snowlin",c("Add a linear regression line to snowfall plot"),FALSE),
                             checkboxInput(inputId="co2lin",c("Add a linear regression line to C02 plot"),FALSE), width=4
                           ),
                           mainPanel(plotOutput("tempPlot"))
                           )
                           ),
                    tabPanel("Results", 
                             sidebarLayout(
                               sidebarPanel(checkboxInput(inputId="resultco2",c("Add a linear regression line to CO2 plot"),FALSE),
                                            checkboxInput(inputId="resultloess",c("Add a loess regression curve to temperature plot"),FALSE),
                                            checkboxInput(inputId="resulttemp",c("Add a linear regression line to temperature plot"),FALSE)
                               ),
                        mainPanel(
                                 plotOutput("results"),htmlOutput("text"))
                             )
                    ),
                  tabPanel("References",htmlOutput("references"))
      )
  )
))

