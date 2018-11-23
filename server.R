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




shinyServer(
  function(input, output, session) {
    observeEvent(input$add_vertex_btn, {
      print(which(input$vertex == V(g)$name) )
      if((input$vertex != '' && !length(V(g))) || (input$vertex != '' && length(unique(V(g)$name == input$vertex)) != 2)) {
        g <<- add_vertices(g, 1, name=input$vertex)
        updateTextInput(session, "vertex", value = '')
        updateSelectInput(session, "edge_from",
                          choices = V(g)$name)
        updateSelectInput(session, "edge_to",
                          choices = V(g)$name)
        updateSelectInput(session, "shortest_vertex_from",
                          choices = V(g)$name)
        updateSelectInput(session, "shortest_vertex_to",
                          choices = V(g)$name)
        
        if(input$show_modal) {
          showModal(modalDialog(
            title = paste('Completion add'),
            paste('your vertex : ', input$vertex), 
            easyClose = TRUE,
            footer = NULL
          ))
        }
      } else {
        showModal(modalDialog(
          title = 'Exist same vertex in this graph or you have omitted vertex name',
          'Please check once again',
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    observeEvent(input$add_edge_btn, {
      g <<- add_edges(g, c(input$edge_from, input$edge_to), label=input$edge_label)
    })
    
    observeEvent(input$add_edge_un_btn, {
      g <<- add_edges(g, c(input$edge_from, input$edge_to, input$edge_to, input$edge_from), label=input$edge_label)
    })
    
    observeEvent(input$network_plotting, {
      if(length(V(g)) < 1) {
        showModal(modalDialog(
          title = paste('At least you must add vertices before creating graph'),
          paste('Fail'), 
          easyClose = TRUE,
          footer = NULL
        ))
      } else {
        output$network_plot = renderPlot({
          par(mfrow=c(1,1))
          plot(g, 
               layout=layout.fruchterman.reingold,
               main = input$graph_title,
               vertex.color = input$vertex_color,
               vertex.size = input$vertex_size,
               vertex.frame.color = input$vertex_frame_color,
               vertex.label.family = input$family,
               vertex.label.color = input$vertex_label_color,
               vertex.label.cex = input$vertex_label_size,
               vertex.shape = input$vertex_type,
               edge.color = input$edge_color,
               edge.width = input$edge_width,
               edge.lty = input$edge_type,
               edge.arrow.size = input$arrow_size,
               #edge.label = input$edge_label,
               edge.label.family = input$family,
               edge.curved = input$edge_curved 
          )
          whichGraph <<- 'original'
        }) 
      }
    })
    observeEvent(input$matrix_plotting, {
      output$matrix_plot = renderPlot({
        par(mfrow=c(1,1))
        ggplot(tbl_df(get.data.frame(g)), aes(x=from, y=to, color=from)) + geom_point() + theme_dark()
      })
    })
    
    observeEvent(input$degree_plotting, {
      output$degree_plot = renderPlot({
        par(mfrow=c(1,1))
        vertex = V(g)$name
        b = degree(g)
        df = tbl_df(data.frame(vertex, b))
        print(df)
        ggplot(df, aes(x=factor(vertex),y=b, color=vertex, group=1)) + geom_point() + geom_line() + xlab('vertex') + ylab('frequency') +  ggtitle('degree distribution') + theme_bw() + theme(plot.title = element_text(hjust = 0.5, size=20, face='bold')) 
      })
    })
    
    output$download_csv = downloadHandler(
      filename = function() {
        paste0('matrix.','csv')
      },
      content = function(f) {
        print(get.data.frame(g))
        #write_csv(get.data.frame(g), path = file)
        write.csv(get.data.frame(g), file = f)
        
      }
    )
    
    output$download_txt = downloadHandler(
      filename = function() {
        paste0('matrix.','txt')
      },
      content = function(file) {
        sink(file)
        print(as.matrix(as_adjacency_matrix(g)))
        sink()
      }
    )
    
    output$download_png = downloadHandler(
      filename = function() {
        if(input$graph_title != '') paste0(input$graph_title,'.png')
        else paste0('graph.', 'png')
      },
      content = function(file) {
        if(length(V(g)) == 0) {
          showModal(modalDialog(
            title = paste('Error'),
            'At least you must add any vertex before exporting',
            easyClose = TRUE,
            footer = NULL
          ))
        } else {
          png(file)
          
          if(whichGraph == 'induced') {
            par(mfrow=c(len,3))
            for(i in 1:length(wc)) {
              plot(induced.subgraph(g, wc[[i]]), 
                   layout=layout.fruchterman.reingold,
                   main = input$graph_title,
                   vertex.color = input$vertex_color,
                   vertex.size = input$vertex_size,
                   vertex.frame.color = input$vertex_frame_color,
                   vertex.label.family = input$family,
                   vertex.label.color = input$vertex_label_color,
                   vertex.label.cex = input$vertex_label_size,
                   vertex.shape = input$vertex_type,
                   edge.color = input$edge_color,
                   edge.width = input$edge_width,
                   edge.lty = input$edge_type,
                   edge.arrow.size = input$arrow_size,
                   #edge.label = input$edge_label,
                   edge.label.family = input$family,
                   edge.curved = input$edge_curved 
              )
            }
          } else if(whichGraph == 'original') {
            par(mfrow=c(1,1))
            plot(g, 
                 layout=layout.fruchterman.reingold,
                 main = input$graph_title,
                 vertex.color = input$vertex_color,
                 vertex.size = input$vertex_size,
                 vertex.frame.color = input$vertex_frame_color,
                 vertex.label.family = input$family,
                 vertex.label.color = input$vertex_label_color,
                 vertex.label.cex = input$vertex_label_size,
                 vertex.shape = input$vertex_type,
                 edge.color = input$edge_color,
                 edge.width = input$edge_width,
                 edge.lty = input$edge_type,
                 edge.arrow.size = input$arrow_size,
                 #edge.label = input$edge_label,
                 edge.label.family = input$family,
                 edge.curved = input$edge_curved 
            )  
          } else if(whichGraph == 'shortestPath') {
            par(mfrow=c(1,1))
            print(ecol)
            plot(g, 
                 layout=layout.fruchterman.reingold,
                 main = input$graph_title,
                 vertex.color = input$vertex_color,
                 vertex.size = input$vertex_size,
                 vertex.frame.color = input$vertex_frame_color,
                 vertex.label.family = input$family,
                 vertex.label.color = input$vertex_label_color,
                 vertex.label.cex = input$vertex_label_size,
                 vertex.shape = input$vertex_type,
                 edge.color = ecol,
                 edge.width = ew,
                 edge.lty = input$edge_type,
                 edge.arrow.size = input$arrow_size,
                 #edge.label = input$edge_label,
                 edge.label.family = input$family,
                 edge.curved = input$edge_curved 
            )
          }
          
          dev.off() 
        }
      }
    )
    
    observeEvent(input$user_file, {
      if(!is.null(input$file) && !is.na(str_locate(input$file$datapath, '.csv')[1])) {
        print(input$file$datapath)
        data = read.csv(input$file$datapath, header=T)
        g <<- graph.data.frame(data)
        showModal(modalDialog(
          title = paste('Completed successfully'),
          easyClose = TRUE,
          footer = NULL
        ))
        
        updateSelectInput(session, "edge_from",
                          choices = V(g)$name)
        updateSelectInput(session, "edge_to",
                          choices = V(g)$name)
        updateSelectInput(session, "shortest_vertex_from",
                          choices = V(g)$name)
        updateSelectInput(session, "shortest_vertex_to",
                          choices = V(g)$name)
        
      } else {
        showModal(modalDialog(
          title = paste('Failure to read'),
          easyClose = TRUE,
          footer = NULL
        ))
      }
    })
    
    observeEvent(input$draw_shortest_path, {
      path = shortest_paths(g,
                            from = V(g)[input$shortest_vertex_from],
                            to = V(g)[input$shortest_vertex_to],
                            output='both')
      ecol <<- rep(input$edge_color, ecount(g))
      ecol[unlist(path$epath)] <<- input$shortest_vertex_color
      ew <<- rep(input$edge_width, ecount(g))
      ew[unlist(path$epath)] <<- input$shortest_path_width
      
      output$network_plot = renderPlot({
        par(mfrow=c(1,1))
        plot(g, 
             layout=layout.fruchterman.reingold,
             main = input$graph_title,
             vertex.color = input$vertex_color,
             vertex.size = input$vertex_size,
             vertex.frame.color = input$vertex_frame_color,
             vertex.label.family = input$family,
             vertex.label.color = input$vertex_label_color,
             vertex.label.cex = input$vertex_label_size,
             vertex.shape = input$vertex_type,
             edge.color = ecol,
             edge.width = ew,
             edge.lty = input$edge_type,
             edge.arrow.size = input$arrow_size,
             #edge.label = input$edge_label,
             edge.label.family = input$family,
             edge.curved = input$edge_curved 
        )
        whichGraph <<- 'shortestPath'
      })
    })
    
    observeEvent(input$draw_sub_graph, {
      output$network_plot = renderPlot({
        wc <<- walktrap.community(g)
        len <<- length(wc)/3
        len <<- len + length(wc)%%3
        par(mfrow=c(len,3))
        
        for(i in 1:length(wc)) {
          plot(induced.subgraph(g, wc[[i]]), 
               layout=layout.fruchterman.reingold,
               main = input$graph_title,
               vertex.color = input$vertex_color,
               vertex.size = input$vertex_size,
               vertex.frame.color = input$vertex_frame_color,
               vertex.label.family = input$family,
               vertex.label.color = input$vertex_label_color,
               vertex.label.cex = input$vertex_label_size,
               vertex.shape = input$vertex_type,
               edge.color = input$edge_color,
               edge.width = input$edge_width,
               edge.lty = input$edge_type,
               edge.arrow.size = input$arrow_size,
               #edge.label = input$edge_label,
               edge.label.family = input$family,
               edge.curved = input$edge_curved 
          )
        }
        
        whichGraph <<- 'induced'
      })
    })
  }
)

