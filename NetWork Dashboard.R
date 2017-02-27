# Load Packages
library(shiny)
library(shinydashboard)
library(visNetwork)

# Define User Interface
ui <- shinydashboard::dashboardPage(
  dashboardHeader(
    title = 'Network Studio'
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Edit Network", tabName = 'EditNetwork')
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = 'EditNetwork',
        uiOutput('editnetwork'),
        fluidRow(
          visNetworkOutput('editor_output')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  

  # Define two dataframes that will carry the node and edge information of our network
  network <- reactiveValues(edges = NULL, nodes = NULL, remove_elements = NULL, groups = c())
  
  # The Network editing box with options to add, remove or edit nodes and elements
  output$editnetwork <- renderUI({
    fluidRow(
      box(collapsible = T, width = "100%", title = "Network Builder",
      tabBox( 
             id = 'networkbuilder',
             width = "100%",
             tabPanel("Add Nodes",
                      textInput('node_name', 
                                label = 'Node Name',
                                placeholder = 'Enter Node Name',
                                width = "50%"
                                ),
                      selectizeInput('node_group', 
                                     choices = network$groups, 
                                     label = 'Node group', width = "50%",
                                     options = list(create = TRUE)),
                      actionButton('new_node', 'Add Node', width = '100%')
                      ),
             tabPanel("Add Edges",
                      selectizeInput('end_one', choices = network$nodes$id, 
                                     label = "Start Node:", width = "50%"),
                      selectizeInput('end_two', choices = network$nodes$id, 
                                     label = "End Node:", width = "50%"),
                      actionButton('new_edge', 'Add Edge', width = "100%")
                      ),
             tabPanel("Remove Element",
                                       selectizeInput('elements_for_removal', 
                                                      choices = list("Nodes" = as.list(network$nodes$id), 
                                                                     "Edges" = as.list(edge_removal_helper(network$edges))), 
                                                      label = 'Elements', 
                                                      width = "100%", multiple = T),
                      actionButton('remove_element', 'Remove Element', width = "100%")
                      ),
             tabPanel("Edit Element",
                      radioButtons('element_type', "Element Type:",
                                   choices = c("Node", 'Edge'), selected = 'Node'),
                      conditionalPanel('input.element_type == "Node"', 
                                       selectizeInput('node_for_edit', 
                                                      choices = network$nodes$id, 
                                                      label = 'Node Name:', 
                                                      width = "100%")
                      ),
                      conditionalPanel('input.element_type == "Edge"',
                                       selectizeInput('edge_for_edit', 
                                                      choices = edge_removal_helper(network$edges), 
                                                      label = 'Edge Name:', 
                                                      width = "100%")
                      ),
                      actionButton('edit_element', 'Edit Element', width = "100%")
                      )
             )
    ))
  })
  
  observeEvent(input$remove_element, {
    node_list <- network$nodes$id
    
    edges <- edge_removal_helper(network$edges)
    
    network$remove_elements <- input$elements_for_removal

    for(i in network$remove_elements) {
      if (i %in% node_list) {
        removal <- which(i == node_list)
        
        network$nodes <- network$nodes[-removal, ]
        
        remove_edges <- which((i == network$edges$from) | (i == network$edges$to))

        network$edges <- network$edges[-remove_edges, ]
        
      } else if (i %in% edges) {
        removal <- which(i == edges)
        
        network$edges <- network$edges[-removal, ]
      }
      
      
    }
  })
  
  observeEvent(input$new_node, {
    if(is.null(network$nodes)) {
      network$nodes <- data.frame(id = c(input$node_name), group = ifelse(input$node_group == "", "None", input$node_group)[[1]])
    } else {
      network$nodes <- as.data.frame(rbind(network$nodes, data.frame(id = input$node_name,
                                                                     group = ifelse(input$node_group == "", "None", input$node_group)[[1]])))
    }
    
    if (!(input$node_group %in% network$groups)) {
      network$groups <- c(network$groups, input$node_group)
    }
  })
  
  observeEvent(input$new_edge, {
    if(is.null(network$edges)) {
      network$edges <- data.frame(from = c(input$end_one), to = c(input$end_two))
    } else {
      network$edges <- rbind(network$edges, data.frame(from = c(input$end_one), to = c(input$end_two)))
    }
    
  })
  
  output$editor_output <- renderVisNetwork({
    if((is.null(network$nodes))) {
      return(NULL)
    } else if ((nrow(network$nodes) == 0)) {
      return(NULL)
    } else {
      if(is.null(network$edges)) {
        net <- visNetwork(nodes = network$nodes, edges = data.frame(from = c(), to = c()))%>% 
          visInteraction(hover = T) %>% 
          visOptions(highlightNearest = T) 
        
        if (length(network$groups) > 1) {
          net <- net %>% visLegend()
        }
        
        net
      } else {
        net <- visNetwork(nodes = network$nodes, edges = network$edges) %>% 
          visInteraction(hover = T) %>% 
          visOptions(highlightNearest = T) 
        
        if (length(network$groups) > 1) {
          net <- net %>% visLegend()
        }
        
        net
      }
    }
  })
  
}

edge_removal_helper <- function(edges) {
  
  if(is.null(edges)) {
    return(NULL)
  } else if(nrow(edges) == 0) {
    return(NULL)
  }
  
  edges <- edges[, 1:2]
  
  collapsed <- apply(edges, 1, paste, collapse = " ~ ")
  
  return(as.character(collapsed))
}

shinyApp(ui = ui, server = server)