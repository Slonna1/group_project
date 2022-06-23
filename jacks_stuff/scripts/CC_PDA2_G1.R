library(shiny)
library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)

data <- read_csv(here("data/beds_by_nhs_board_of_treatment_and_specialty.csv"))
summary(data)

data = filter(data, Location == "S92000003")

ggplot(data) + aes(x = Quarter, y = AllStaffedBeds) + geom_line()
ggplot(data) + aes(x = Quarter, y =  TotalOccupiedBeds) + geom_line()

data