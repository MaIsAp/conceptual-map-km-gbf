install.packages(c("shiny","dplyr","stringr", "visNetwork","DT"),repos = "http://cran.us.r-project.org")

library(shiny)
library(dplyr)
library(stringr)
library(visNetwork)
library(DT)

# Load data from Sheets
load_data <- function() {
  edges <- read.csv("./input_matrix-edges.csv")
  nodes <- read.csv("./input_matrix-nodes.csv")
  return(list(edges,nodes))
}

# UI of app
ui <- fluidPage(
  titlePanel("Conceptual map of the KM-GBF"),
  
  sidebarPanel(
    checkboxGroupInput("filter_elements", 
                       "Choose elements:",
                       choices = c("goal","target","category","headline","binary","component","complementary"),
                       selected = c("goal","target","category","headline","binary"))
  ),
  
  fluidRow(
    column(12,
           visNetworkOutput("mynetworkid", height = "800px"))
  ),
  
  fluidRow(
    column(12,
           actionButton("refresh", "Update data"))
  ),
  
  fluidRow(
    column(12,
           dataTableOutput('table')
    )
  )
)

# Server of app
server <- function(input, output, session) {
  
  # load data when pressing button
  data <- reactive({
    input$refresh
    isolate({
      load_data()
    })
  })
  
  # create network graph
  output$mynetworkid <- renderVisNetwork({
    #Extract from data loaded
    nodes <- data()[[2]]
    edges <- data()[[1]]
    
    # Get categories to filter network visualization (one from goal, target, category, indicator...)
    req(input$filter_elements)
    filter_indicator <- input$filter_elements
    
    # create node table from data, filtered if required
    vis.nodes <- nodes |> filter(type %in% filter_indicator)
    # create links table from data, filtered if required
    vis.links <- edges |> filter(from %in% vis.nodes$id &
                                   to %in% vis.nodes$id)
    # assign colors to edge categories (source) dynamically
    edge_sources <- unique(vis.links$source)
    base_palette <- c("#d4cf9a",
                      "#9cc0ed",
                      "#9fdbbb",
                      "#70d3e3",
                      "#d8b3e1",
                      "#ebaea7")
    palette <- if (length(edge_sources) <= length(base_palette)) {
      base_palette[seq_along(edge_sources)]
    } else {
      grDevices::hcl.colors(length(edge_sources), "Set 3")
    }
    edge_color_map <- stats::setNames(palette, edge_sources)
    vis.links$color <- unname(edge_color_map[vis.links$source])
    # enter linebreak after 50 characters
    vis.links$source <- str_wrap(vis.links$source, width = 50)
    
    vis.nodes$shadow <- TRUE # Nodes will drop shadow
    vis.nodes$borderWidth <- 2 # Node border width
    
    # assign color and shape to nodes
    vis.nodes$color <- str_replace_all(vis.nodes$type,c("category"= "#56ae6c", "goal"= "#b54f90","target"= "#7066bc", 
                                                        "headline"="#CA943C", "binary"="#CA943C", "component"="#F3DF95", "complementary"="#F3DF95"))
    vis.nodes$shape <- str_replace_all(vis.nodes$type,c("category"= "square", "goal"= "diamond", "target"= "diamond",
                                                        "headline"="dot", "binary"="dot", "component"="dot", "complementary"="dot"))
    
    # count connections to define node sizes
    count_goals_targets <- vis.links |> count(from) |> rename(id=from,value=n) |> mutate(id=as.character(id))
    count_indic_categories <- vis.links |> count(to) |> rename(id=to,value=n) |> mutate(id=as.character(id))
    
    count_nodes <- count_goals_targets |> bind_rows(count_indic_categories)
    count_nodes2 <- count_nodes |> group_by(id) |> summarise(n=sum(value))
    
    vis.nodes2 <- vis.nodes |> mutate(id=as.character(id)) |> 
      left_join(count_nodes2 , by="id" ) |> mutate(value=case_when(type=="category" ~ n/2,
                                                                   TRUE ~n))
    # create network figure
    visNetwork(vis.nodes2, vis.links) |>
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) |> 
      visLegend(useGroups = FALSE, addNodes = data.frame(label = c("Category","Goal","Target","Headline/Binary","Component/Complementary"), 
                                                         shape = c("square","diamond","diamond","dot","dot"),
                                                         color=c("#56ae6c","#b54f90","#7066bc","#CA943C","#F3DF95")),
                addEdges = data.frame(color = unname(edge_color_map),
                                      label = str_wrap(names(edge_color_map), width = 50),
                                      font.align = "bottom"))
  })
  
  # insert table for targets and goals
  output$table <- renderDataTable({
    descriptions <- data()[[2]]
    # descriptions
    descriptions |>
      dplyr::filter(type!="category") |>
      dplyr::select(-id)
  })
}

# deploy app
shinyApp(ui, server)
