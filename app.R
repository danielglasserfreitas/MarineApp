library("glue")
library("DT")
library("tidyverse")
library("shiny")
library("shiny.router")
library("shiny.semantic")
library("leaflet")
library("data.table")
library("geosphere")
library("shinyjs")
source("module_ship.R")

info_page <- div(
  div(class = "ui two column stackable grid container",
      div(class = "six wide column",
          img(src="imgs/Dani2.jpg")
      ),
      div(class = "ten wide column",
          div(class="ui center aligned big header", "Marine Shiny App"),
          p("This app was created for the purposes of demonstrating ",
            a(tags$b("shiny.semantic"), href = "https://github.com/Appsilon/shiny.semantic"),
            " features in creating an interactive data visualization. This dashboard uses ",
            a("Marine ", href = "https://drive.google.com/file/d/1IeaDpJNqfgUZzGdQmR6cz2H3EQ3_QfCV/view?usp=sharing"), "data and you can check the ",
            "source code at my ", a("GitHub", href = "https://github.com/danielglasserfreitas"), "."),
          div(class="ui center aligned", style = "text-align: center;",
              action_button("go_modal", "Learn more", class = "teal"),
              br(),br(),
            HTML('<iframe src="https://www.youtube.com/embed/7aGlcFENOrs" width="90%" height="300" frameborder="1"></iframe>')
          )
      )
  )
)

router <- make_router(
  route("index", info_page),
  route("Vessel", uiShip("p1"))
)

server <- function(input, output, session) {
  # router pages
  router$server(input, output, session)#router(input, output) #
  shipServer("p1")

  # modal
  observeEvent(input$go_modal, {
    create_modal(modal(
      id = "simple-modal",
      title = "Click here to know more about it",
      content = list(style = "background: lightblue",
        `data-custom` = "value",
        p( "What information is being transmitted from the ships via their AIS signal and what can we learn from it?",
           "Know more about",
           a("AIS signal Data", href = "https://www.marinetraffic.com/blog/information-transmitted-via-ais-signal/"), ".")
      ),
      p(tags$b("This app was created by Daniel Freitas for"),a("AppSilon",href="https://appsilon.com/shiny/"),".")
    ))
  })
}


ui <- semanticPage(
  title = "Marine Traffic App",
  tags$head(
    tags$link(rel="stylesheet", href="style.css", type="text/css" )
  ),
  horizontal_menu(
    list(
      list(name = "Info", link = route_link("index"), icon = "world"),
      list(name = "Vessel", link = route_link("Vessel"), icon = "ship")
    ), logo = "imgs/Logo.png"
  ),
  router$ui,#router_ui(),
  tags$footer("Created by Daniel Freitas for Appsilon", align = "center", style = "
                        position:fixed;
                        bottom:0;
                        right:0;
                        left:0;
                        background:#062a61;
                        color: white;
                        padding:10px;
                        box-sizing:border-box;
                        z-index: 1000;
                        text-align: center")
)


shinyApp(ui, server)
