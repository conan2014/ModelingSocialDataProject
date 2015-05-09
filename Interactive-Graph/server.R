## APMA E4990 Modeling Social Data
## Professor Wiggins and Hofman
## Xu Huang

library(ggvis)
library(shiny)

library(dplyr)
#install.packages("RSQLite")
if (FALSE) library(RSQLite)

#install.packages("data.table")
library("data.table")

library(readr)

#install.packages("xlsx")
library(xlsx)

setwd("C:/Users/Conan/Desktop/shiny-examples/Yelp-example")

# Read in business csv file
business <- read_csv("yelp_business.csv")
business <- as.data.frame(business)
business <- mutate(business, id = seq(1, nrow(business)))

# Filter out those business with <10 reviews and without price range, and select specified columns
all_business <- business[business$review_count >= 10 & business$`attributes.Price Range` %in% c(1,2,3,4), ] 
categories.logic = grepl("u'Restaurants", all_business$categories)
all_business <- cbind(all_business, categories.logic)
all_business <- all_business[categories.logic==TRUE, ]
all_business <- select(all_business, id, name, review_count, state, city, stars, `attributes.Price Range`, business_id, `attributes.Noise Level`)
names(all_business) <- c("id", "name", "review_count", "state", "city", "stars", "price_range", "business_id", "noise")
# 13,966 restaurants selected in total at this point
# write.xlsx(all_business, file = "C:/Users/Conan/Desktop/business.xlsx", sheetName = "Sheet1", col.names = TRUE)

shinyServer(function(input, output, session){
  
  # Filter the restaurants, returning a data frame
  # Note: first letter of all variables contained in all_business df are lowercase
  #       first letter of all variables from user inputs or considered temporary are uppercase 
  restaurants <- reactive({
    # Create temp variables for input values
    Minreviews <- input$Reviews[1]
    Maxreviews <- input$Reviews[2]
    Minstars <- input$Stars[1]
    Maxstars <- input$Stars[2]
    Minprice <- input$Price[1]
    Maxprice <- input$Price[2]
    
    # Apply filters
    m <- all_business %>% 
      filter(
        review_count >= Minreviews,
        review_count <= Maxreviews,
        stars >= Minstars,
        stars <= Maxstars,
        price_range >= Minprice,
        price_range <= Maxprice
        ) %>%
      arrange(stars)
    
    # Optional: filter by cities
    if (input$States != "All"){
      if (input$States == "Arizona"){
        m <- m %>% filter(state %like% "AZ" )
      }
      else if(input$States == "Baden-Wurttemberg"){
        m <- m %>% filter(state %like% "BW" )
      }
      else if(input$States == "Edinburgh"){
        m <- m %>% filter(city %like% "Edinburgh" )
      }
      else if(input$States == "Illinois"){
        m <- m %>% filter(state %like% "IL" )
      }
      else if(input$States == "North Carolina"){
        m <- m %>% filter(state %like% "NC" )
      }
      else if(input$States == "Nevada"){
        m <- m %>% filter(state %like% "NV" )
      }
      else if(input$States == "Ontario"){
        m <- m %>% filter(state %like% "ON" )
      }
      else if(input$States == "Pennsylvania"){
        m <- m %>% filter(state %like% "PA" )
      }
      else if(input$States == "Quebec"){
        m <- m %>% filter(state %like% "QC" )
      }
      else if(input$States == "Rhineland-Palatinate"){
        m <- m %>% filter(state %like% "RP" )
      }
      else if(input$States == "South Carolina"){
        m <- m %>% filter(state %like% "SC" )
      }
      else if(input$States == "Wisconsin"){
        m <- m %>% filter(state %like% "WI" )
      }
    }
    
    # Optional: filter by noise level
    if (input$Noise != "All"){
      if (input$Noise == "Quiet"){
        m <- m %>% filter(noise %like% "quiet" )
      }
      else if(input$Noise == "Average"){
        m <- m %>% filter(noise %like% "average" )
      }
      else if(input$Noise == "Loud"){
        m <- m %>% filter(noise %like% "loud" )
      }
      else if(input$Noise == "Very Loud"){
        m <- m %>% filter(noise %like% "very_loud" )
      }
    }
    
    m <- as.data.frame(m)
    
  })
  
  # Functions for generating tooltip text
  restaurant_tooltip <- function(x) {
    if (is.null(x)) return (NULL)
    if (is.null(x$id)) return (NULL)
    
    # Pick out the restaurant with this ID
    all_business <- isolate(restaurants())  # Explain why we need to isolate !!!!
    restaurant <- all_business[all_business$id == x$id, ]
    
    paste0("<b>", restaurant$name, "</b><br>",
           restaurant$city, "<br>", restaurant$state, "<br>", 
           "Price Level: ", restaurant$price_range, "<br>", 
           "Noise: ", restaurant$noise, "<br>",
           "Number of reviews: ", restaurant$review_count)
  }
  
  # A reactive expression with the ggvis plot
  vis <- reactive({
    # Labels for axes
    xvar_name <- names(axis_vars)[axis_vars == input$xvar]
    yvar_name <- names(axis_vars)[axis_vars == input$yvar]
    
    # Normally we could do something like props(x = ~BoxOffice, y = ~Reviews),
    # but since the inputs are strings, we need to do a little more work.
    xvar <- prop("x", as.symbol(input$xvar))
    yvar <- prop("y", as.symbol(input$yvar))
    
    restaurants %>%
      ggvis(x = xvar, y = yvar, key := ~id) %>%
      layer_points(size := 50, size.hover := 200, 
                   fillOpacity := 0.2, fillOpacity.hover := 0.5) %>%
      add_tooltip(restaurant_tooltip, "hover") %>%
      add_axis("x", title = xvar_name) %>%
      add_axis("y", title = yvar_name) %>%
      set_options(width = 600, height = 520)
  })
  
  vis %>% bind_shiny("plot1")
  
  output$n_restaurants <- renderText({nrow(restaurants()) })
})

