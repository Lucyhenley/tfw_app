library(shiny)
library(grid)
library(gridExtra)
library(reshape2)
library(png)
library(tools)
library(pracma)
library(latex2exp)



#setwd("C:/Users/lucy_/covid-recovery/app")


seat_locations <- read.csv(file="seat_locations.csv")
shield_locations <- read.csv(file="shield_locations.csv")

domain_x <- 20.4
domain_y <- 2.82
x_box <- c(0,0,       domain_x,domain_x,0,domain_x)
y_box <- c(0,domain_y,domain_y,0,       0,0)

source("ui.R")
source("server.R")
source("src.R")

shinyApp(ui = ui, server = server)
