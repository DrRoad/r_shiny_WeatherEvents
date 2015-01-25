library(shiny)
library(leaflet)

# options for ui-selects
dams <- c("all" = "damage", "Fatalities" = "fatalities","Injuries" = "injuries")
tops <- c( "25"="25", "100"="100", "500"="500", "1000"="1000","5000"="5000", "all"="all")

shinyUI(navbarPage("US Weather Events", id="nav",

  tabPanel("Interactive map",
    div(class="outer",
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),
      
      leafletMap("map", width="100%", height="100%",
        initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
        options=list(
          center = c(37, -93),
          zoom = 4,
          maxBounds = list(list(12,-135), list(55,-54)) # focus on US
        )
      ),
      
      uiOutput("eventExplore"),
      absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
        top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        
        h2("Event explorer"),
        selectInput("damageType", label= "Damage Type", dams, width = '100%'),  
        selectInput("topLimit", label= "Display count", tops, width = '30%'  ),     
        uiOutput("eventType"),
        checkboxInput("yearAna", label = "Year analysis", value = TRUE),
        uiOutput("yearSlider"),
        hr(),
        textOutput('numEvents'),
        textOutput('numDamAbs'),
        textOutput('numDamRel'),
        hr(), 
        plotOutput("eventHist", height = 200) 
      ),
      
      tags$div(id="cite",
        'Data: ', tags$em('US National Weather Service'), 
        ' -- Layout: ', tags$em('Joe Cheng @ github.com/jcheng5')
      )
    )
  ),

  tabPanel("Docu",           
    a("GitHub Repro + Documentation",href="https://github.com/SteHyr/r_shiny_WeatherEvents")
  ),
  
  conditionalPanel("false", icon("crosshair"))
))
