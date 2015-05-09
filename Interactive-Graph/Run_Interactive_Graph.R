# You need server.R, ui.R, and global.R to generate the interactive graph successfully
# Change this to the path to your local directory  
setwd("C:/Users/Conan/Desktop/shiny-examples")

library(shiny)

# Change this to the file name that contains server.R, ui.R, and global.R
shiny::runApp("Yelp-example")

