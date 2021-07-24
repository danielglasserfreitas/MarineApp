source("utils.R")

uiShip <- function(id, label = "Vessel") {
  ns <- NS(id)
  tagList(
    div(class="ui center aligned header", "Marine Traffic"),
    segment(
      selectInput(ns("shipType"), "Select Vessel Type:", unique(ship_data$ship_type), selected = "Cargo"),br(),
      div(class = "ui three column stackable grid container",
          div(class = "column",
              custom_ui_message(head = textOutput(ns("ship_total_value")),
                                content = "Total Vessels",
                                color = "blue",
                                icon_name = "ship")
          ),
          div(class = "column",
              custom_ui_message(textOutput(ns("ship_speedAVG_value")),
                                "Avg.Speed (knots)",
                                color = "olive",
                                icon_name = "running")
          ),
          div(class = "column",
              custom_ui_message(textOutput(ns("ship_LENGTH_value")),
                                "Avg.Length (meters)",
                                color = "brown",
                                icon_name = "anchor")
          )
      )
    ),
    segment(
      #segment(
      #  p("Select ShipName"),
      #  uiOutput(ns("ship_search"))
      #),
      div(class = "ui two column stackable grid container",
          div(class = "column",
              leafletOutput(ns("result"))
          ),
          div(class = "column",
              p("Select Vessel Name:"),
              uiOutput(ns("ship_search")),br(),
              semantic_DTOutput(ns("tab_position")))
      )
    ), br()
  )
}


shipServer <- function(id) {
  
  
  storeWarn<- getOption("warn")
  options(warn = -1) 
  
  
  ns <- NS(id)
  moduleServer(id, function(input, output, session) {
    vessel_data <- reactive({
      validate(
        need(input$shipType, "Fetching data...")
      )
      toast("Wait while Vessel Data is loading...", class = "warning", duration = 2)
      ship_data %>% filter(ship_type == input$shipType)
    })
    
    
    
    output$ship_total_value <- renderText({
      get_n_ship(vessel_data())
    })
    
    output$ship_speedAVG_value <- renderText({
      get_avg_speed_shipType(vessel_data() %>% filter(is_parked==0))
    })
    
    output$ship_LENGTH_value <- renderText({
      get_avg_LENGTH_shipType(vessel_data()%>% filter(is_parked==0))
    })
    
    
    output$ship_search <- renderUI(
      search_selection_choices(ns("shipNames"), get_ship_name(vessel_data()) , value = head(vessel_data() %>% select("SHIPNAME"),1), multiple = F)
    )
    
    output$result <- renderLeaflet({
      validate(
        need(input$shipType, "Fetching data..."),
        need(input[["shipNames"]], "Fetching data...")
      )
      shinyjs::delay(expr =({ 
        options(warn = storeWarn) 
      }) ,ms = 100) 
      
      get_ship_map(vessel_data() %>% filter(SHIPNAME== input[["shipNames"]]))
      
    })
    
    output$tab_position <- renderDataTable({
      validate(
        need(input$shipType, "Fetching data..."),
        need(input[["shipNames"]], "Fetching data...")
      )
   
      
      subset <- get_ship_details(vessel_data() %>% filter(SHIPNAME== input[["shipNames"]]))
      semantic_DT(subset, options = list(bInfo = F, dom = "rt"))
    })

  })
}
