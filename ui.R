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
Sys.setlocale("LC_ALL", "ko_KR.UTF-8")



shinyUI(
  fluidPage(theme = shinytheme("cerulean"),
            # App title
            titlePanel(title='', windowTitle = "Graph Maker"),
            # h2('Graph Maker', img(src = 'https://2.bp.blogspot.com/--F3rQKNMc04/WkUD-_s4N2I/AAAAAAAAAJg/q-mLKFm8xIcVEdzonP0QiqOCiGe9m5viQCLcBGAs/s1600/data-structures-icon.png', height = "30px")),
            
            # Sidebar layout with a input and output definitions
            fluidRow(
              column(4,
                     fluidRow(
                       column(6,
                              wellPanel(
                                h3("User input form"),hr(),  
                                
                                textInput(inputId = "vertex", 
                                          label = "Vertex:",
                                          placeholder = "Vertex name(required & only English)"),
                                
                                actionButton(inputId = "add_vertex_btn", 
                                             label = "Add vertex"),hr(),
                                
                                selectInput(inputId = "edge_from", 
                                            label = "edge from:",
                                            width = '70%',
                                            choices = V(g)$name),
                                selectInput(inputId = "edge_to", 
                                            label = "edge to:",
                                            width = '70%',
                                            choices = V(g)$name),
                                textInput(inputId = "edge_label", 
                                          label = "edge name:",
                                          value = '',
                                          placeholder = "Edge name(not required & only English)"),
                                
                                actionButton(inputId = "add_edge_btn", 
                                             label = "Add edge(directed)"), hr(),
                                
                                actionButton(inputId = "add_edge_un_btn", 
                                             label = "Add edge(undirected)"),hr(),
                                
                                actionButton(inputId = "network_plotting", 
                                             label = "draw graph"), hr(),
                                actionButton(inputId = "matrix_plotting", 
                                             label = "draw matrix"), hr(),
                                actionButton(inputId = "degree_plotting", 
                                             label = "draw degree distribution"), hr()
                                
                              ),
                              
                              wellPanel(
                                h3('Shortest Path'),hr(),
                                
                                selectInput(inputId = "shortest_vertex_from", 
                                            label = "vertex from:",
                                            choices = V(g)$name),
                                selectInput(inputId = "shortest_vertex_to", 
                                            label = "vertex to:",
                                            choices = V(g)$name),
                                colourInput(
                                  "shortest_vertex_color", NULL, "#b2b2a4",
                                  allowTransparent = TRUE),
                                sliderInput(inputId = 'shortest_path_width', 
                                            label = 'shortest path width',
                                            min = 0,
                                            max = 15,
                                            value = 1,
                                            step = .1,),
                                actionButton(inputId = "draw_shortest_path", 
                                             label = "Draw shortest path")
                              ),
                              
                              wellPanel(
                                h3('Sub graph'),hr(),
                                
                                actionButton(inputId = "draw_sub_graph", 
                                             label = "Draw sub graph")
                              )
                       ),
                       column(6,
                              wellPanel(
                                h3('Utility'),hr(),
                                textInput(inputId = "graph_title", 
                                          label = "graph title:",
                                          placeholder = "Enter text to be used as graph title"),
                                
                                h4('vertex color'),
                                colourInput(
                                  "vertex_color", NULL, "#b2b2a4",
                                  allowTransparent = TRUE),
                                h4('vertex label color'),
                                colourInput(
                                  "vertex_label_color", NULL, "#0085a4",
                                  allowTransparent = TRUE),
                                h4('edge color'),
                                colourInput(
                                  "edge_color", NULL, "#ffb798",
                                  allowTransparent = TRUE),
                                h4('vertex frame color'),
                                colourInput(
                                  "vertex_frame_color", NULL, "#00FF0080",
                                  allowTransparent = TRUE),
                                
                                sliderInput(inputId = 'vertex_size', 
                                            label = 'vertex size',
                                            min = 1,
                                            max = 70,
                                            value = 20,
                                            step = 1,),
                                sliderInput(inputId = 'vertex_label_size', 
                                            label = 'vertex label size',
                                            min = 1,
                                            max = 5,
                                            value = .5,
                                            step = .1,),
                                sliderInput(inputId = 'edge_width', 
                                            label = 'edge width',
                                            min = 0,
                                            max = 15,
                                            value = 1,
                                            step = .1,),
                                sliderInput(inputId = 'edge_curved', 
                                            label = 'edge curve',
                                            min = 0,
                                            max = 1,
                                            value = 0,
                                            step = .1,),
                                sliderInput(inputId = 'arrow_size', 
                                            label = 'arrowsize',
                                            min = 0,
                                            max = 3,
                                            value = 1,
                                            step = .1,),
                                
                                selectInput(inputId = "edge_type", 
                                            label = "edge type",
                                            choices = c('solid', 'dashed', 'dotted', 'longdash', 'twodash')),
                                
                                selectInput(inputId = "vertex_type", 
                                            label = "vertextype",
                                            choices = c('circle', 'square', 'csquare', 'dotdash', 'rectangle', 'crectangle','vrectangle','pie','sphere','none')),
                                selectInput(inputId = "family", 
                                            label = "font faimly",
                                            choices = c('mono', 'sans', 'serif')),
                                wellPanel(
                                  checkboxInput(inputId = "show_modal",
                                                label = "Show window notified that your vertex added successfully",
                                                value = TRUE)
                                ),
                                wellPanel(
                                  fileInput('file', 'Choose CSV file',
                                            accept = '.csv'),
                                  actionButton(inputId = "user_file", 
                                               label = "read as a graph model")
                                )
                              )
                       )
                     )
              ),
              column(8,
                     # Output:
                     mainPanel(
                       height=10,
                       
                       tabsetPanel(type ='tabs',
                                   tabPanel(title = 'graph',
                                            plotOutput(outputId = "network_plot", height='700px'),
                                            downloadButton('download_png', 'Download PNG')),
                                   tabPanel(title = 'matrix',
                                            plotOutput(outputId = 'matrix_plot', height='700px'),
                                            downloadButton('download_csv', 'Download matrix(CSV)'),
                                            HTML('&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;'),
                                            downloadButton('download_txt', 'Download matrix(TXT)')),
                                   tabPanel(title = 'degree',
                                            plotOutput(outputId = 'degree_plot', height='700px')))
                       
                     ))
            )
            
            
  )
)

