library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(igraph)
# install.packages("colourpicker")
library('colourpicker')
library(shinythemes)
library(rsconnect)
options(shiny.sanitize.errors = FALSE)
Sys.setlocale("LC_ALL", "ko_KR.UTF-8")

g = make_empty_graph()

whichGraph = 0
