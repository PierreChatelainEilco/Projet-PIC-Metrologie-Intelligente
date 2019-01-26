#initialize
library(shiny)
library(ggplot2)
library(purrr)
library(dplyr)



#plotting theme for ggplot2
.theme<- theme(
  axis.line = element_line(colour = 'gray', size = .75),
  panel.background = element_blank(),
  plot.background = element_blank()
)