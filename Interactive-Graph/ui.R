## APMA E4990 Modeling Social Data
## Professor Wiggins and Hofman
## Xu Huang

library(ggvis)
library(shiny)

# For dropdown menu
actionLink <- function(inputId, ...){
  tags$a(href = 'javascript:void', 
         id = inputId, 
         class = 'action-button',
         ...)
}

shinyUI(fluidPage(
  titlePanel("Yelp Restaurant Explorer"),
  fluidRow(
    column(3, 
      wellPanel(
        #h4("Filter"),
        sliderInput("Reviews", "Minimum number of Reviews on Yelp", 10, 4578, value = c(10, 4578), step = 50),
        sliderInput("Stars", "Rating Stars", min = 1, max = 5, value = c(1,5), step = 0.5),
        sliderInput("Price", "Price in terms of $ $$ $$$, etc", 1, 4, value = c(2,4), step = 1),
        selectInput("States", "States where restaurants reside", c("All", "Arizona", "Baden-Wurttemberg",
                    "Edinburgh", "Illinois", "North Carolina", "Nevada", "Ontario", "Pennsylvania",
                    "Quebec", "Rhineland-Palatinate", "South Carolina", "Wisconsin")),
        selectInput("Noise", "Noise Level", c("All", "Quiet", "Average", "Loud", "Very Loud"))
        ),
      
      wellPanel(
        selectInput("xvar", "X-axis variable", axis_vars, selected = "review_count"),
        selectInput("yvar", "Y-axis variable", axis_vars, selected = "stars")
        )
      ),
    column(9, 
           ggvisOutput("plot1"),
           wellPanel(
             span("Number 0f restaurants selected:", 
                  textOutput("n_restaurants")
               )
             )
           )
    )
  )
)