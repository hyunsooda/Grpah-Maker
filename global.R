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
# g = add_vertices(g, 1, name='hello')
# g = add_vertices(g, 1, name='world')
# g = add_vertices(g, 1, name='hello2')
# g = add_vertices(g, 1, name='world2')
# g = add_edges(g, c('hello', 'world', 'world', 'hello', 'world2','hello2', 'hello2', 'world2'))
# plot(g, family='AppleMyungjo')
# gg = g
whichGraph = 0