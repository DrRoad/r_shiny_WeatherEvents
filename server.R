library(shiny)
library(ggplot2)
library(leaflet)
library(RColorBrewer)
library(dplyr)
library(gtools)

# additional function for diagram plotting from >>Cookbook for R<<
# source http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   require(grid)
   
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   
   numPlots = length(plots)
   
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
      # Make the panel
      # ncol: Number of columns of plots
      # nrow: Number of rows needed, calculated from # of cols
      layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                       ncol = cols, nrow = ceiling(numPlots/cols))
   }
   
   if (numPlots==1) {
      print(plots[[1]])
      
   } else {
      # Set up the page
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
      
      # Make each plot, in the correct location
      for (i in 1:numPlots) {
         # Get the i,j matrix positions of the regions that contain this subplot
         matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
         
         print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                         layout.pos.col = matchidx$col))
      }
   }
}
 
shinyServer(function(input, output, session) {
   
   ## 1---- global initialization 
   
   # initial data loading 
   data <- read.csv(file="./data/stormData_filtered.csv",head=TRUE ) 
   
   # get the time limits
   minYear <-  min( as.numeric( data$year ) )
   maxYear <-  max( as.numeric( data$year ) )
   
   # event colors 
   colorPal <- colorRampPalette(brewer.pal( 8 , "Spectral") )
   eventColors <- colorPal(length(unique(data$etype)))
   
   # set colors for the event markers, depending on the number of available event types 
   names(eventColors) <- levels(unique(data$etype) )
   data$eCol <- eventColors [eventColors = data$etype]
     
   # convert all remarks($REMARK) and event types($etype) into characters - for more easy computing
   data$remark<-as.character(data$remark)
   data$etype<-as.character(data$etype)
   
   
   # Create the map
   map <- createLeafletMap(session, "map")
   
   
   ## 2---- creating user interface sliders and selects  - runs once in the beginning
   
   # dynamically create year slider, depending on available data
   output$yearSlider <- renderUI({  
      yearAna <- input$yearAna

      if (yearAna) {
         sliderInput(
            "eveYear", 
            "Year",
            min = minYear, 
            max = maxYear,
            step = 1,
            value = minYear,
            width = '100%',
            animate=TRUE,
            format="####")
      }
   })
  
   # dynamically create select for all available event types
   output$eventType <- renderUI({
      
      # get event labels 
      eventTypes <- as.character(unique(data$etype))
      
      # alphabetic order of event labels 
      eventTypes <- eventTypes[order(eventTypes)] 
      
      # format for select Input
      names(eventTypes) <- factor(eventTypes)
      
      # add "all" option in the list 
      eventTypes <- c ( "all types" = "all", eventTypes)
      
      # create select Input
      selectInput("eventType", label= "Event Type",  eventTypes , width = '100%') 
   })

   
   ## 3---- plot diagram and map features - runs when inputs are changed
   
   # generate chart events per year and damage per event per year
   output$eventHist <- renderPlot({
      
      # If no data available, skip ploting
      if (nrow(data) == 0)
         return(NULL)
      
      # get input 
      damType <- input$damageType 
      
      # return, if not fatalities or injuries
      switch (damType, fatalities = { }, injuries = { }, damage = { }, return(NULL) )
      
      # filter all events with no damage for fat. or inj. out
      dataLocalFilter<- data[ data[[damType]] != 0 ,]
      
      # event type filter 
      eventType <- input$eventType
      if (!invalid(eventType)){
         if (eventType != "all" & nrow(dataLocalFilter) > 0 ){
            dataLocalFilter<- dataLocalFilter[ dataLocalFilter$etype == eventType ,]
         } 
      }
      
      # aggregrate damage per year
      filterList<-aggregate(x = dataLocalFilter[[damType]], by=list(dataLocalFilter$year), FUN=sum) 
      fl2<-aggregate(x=dataLocalFilter[[damType]], by=list(dataLocalFilter$year), FUN=length)
      filterList$len <- fl2$x
      
      # general part of diagram 
      p <- ggplot(data = filterList, aes( x = Group.1 ) ) 
      p <- p + theme_classic() + xlim(minYear, maxYear)
      p <- p + labs( x = "year" , y = "")
      
      # specific part of diagram 
      p1 <- p + geom_line(aes (  y = x ) ) + labs( title = sprintf("%s (absolut)", input$damageType ) )  
      p2 <- p + geom_line(aes (  y = x/len) ) + labs( title = sprintf("%s/event (relative)",input$damageType ) )
      
      # plot absulut/relative diagram
      multiplot(p1, p2, cols=1)
   })
   
   # session$onFlushed is necessary to work around a bug in the Shiny/Leaflet
   # integration; without it, the addCircle commands arrive in the browser
   # before the map is created.
   session$onFlushed(once=TRUE, function() {
      paintEvent <- observe({
         
         # Clear existing circles before drawing
         map$clearShapes()

         # time filter
         if (input$yearAna) {
            year<-input$eveYear
            if (invalid(year)){ return(NULL) }
            dataLocalFilter <- data[ data$year == year,]
         } else {
            dataLocalFilter<-data
         }
               
         # damage type filter
         damType <- input$damageType
         
         # return, if damType does not fit
         switch (damType,fatalities = {},injuries = {}, damage = {}, return(NULL) ) 

         # remove events with no damage
         dataLocalFilter<- dataLocalFilter[ dataLocalFilter[[damType]] != 0 ,]
         
         # order events by impact  
         dataLocalFilter<-dataLocalFilter[order(dataLocalFilter[[damType]], decreasing = TRUE),]
      
         
         # event type filter 
         eventType <- input$eventType
         if (!invalid(eventType)){
            if (eventType != "all" & nrow(dataLocalFilter) > 0 ){
               dataLocalFilter<- dataLocalFilter[ dataLocalFilter$etype == eventType ,]
            } 
         }

         # filter top n (to reduce computation effort)
         if (input$topLimit != "all"){
            topLimit <- min( as.numeric(input$topLimit), nrow(dataLocalFilter) )
            dataLocalLimit <- dataLocalFilter[1:topLimit,]
         } else { dataLocalLimit <- dataLocalFilter }     
         
         # overall damage
         allCasOrDam = sum(dataLocalLimit[[damType]]) 
         
         # short textual summary/feedback
         output$numDamRel<- renderText({
            sprintf('%s/Event: %.1f', damType, allCasOrDam/nrow(dataLocalFilter)) })
         output$numEvents<- renderText({
            sprintf('Events (all): %.0f', nrow(dataLocalFilter)) })
         output$numDamAbs<- renderText({
            sprintf('%s (total): %.0f', damType, allCasOrDam) })
         
         # Draw in batches of n = chunksize to appear the app feel responsive
         chunksize <- 50
         # drawing the events in the map
         for (from in seq.int(1, nrow(dataLocalLimit), chunksize)) {
            
            # get index for data chunk ending 
            to <- min(nrow(dataLocalLimit), from + chunksize)
            
            # get data chunk
            dataChunk <- dataLocalLimit[from:to,]
            dataChunk$effect <-  sqrt((dataChunk[[damType]])* 10^9 / pi )
            
            # plot circles into map
            try(
               map$addCircle(
                  dataChunk$lat, 
                  dataChunk$lng,
                  dataChunk$effect,
                  layerId = dataChunk$X,
                  options=list(  
                     stroke = FALSE, 
                     fill=TRUE ,       
                     fillOpacity = .5),
                  eachOptions = list(
                     fillColor = dataChunk$eCol) 
               ) 
            )
         } 
         
      })
    session$onSessionEnded(paintEvent$suspend)
  })  


   # When map is clicked, show a popup with city info
   clickObs <- observe({
      
      # delete possibly existing popup
      map$clearPopups()
      
      # return, if no popup clicked, 
      event <- input$map_shape_click
      if (is.null(event))
         return()
    
      # get clicked event
      focusEvent <- data[data$X == event$id,]
       
      # create remarks box or info, if no remarks available
      if (nchar(focusEvent$remark) > 1 ){ 
         remarksDiv <- tags$div(
            tags$h5("Remarks"),
            tags$div( 
               style="max-height:120px;max-width:300px;border:1px;overflow:auto;",
               sprintf("%s",focusEvent$remark)
               )
            )
      } else { remarksDiv <- tags$h5("no Remarks available") }
    
      # create content box
      content <- as.character(tagList(
         tags$h5("Event Info"),
         sprintf("Weather Type: %s", focusEvent$etype ), tags$br(),
         sprintf("Fatalities: %s", focusEvent$fatalities ), tags$br(),
         sprintf("Injuries: %s",  focusEvent$injuries ), tags$br(),
         remarksDiv
         )
      )
    
      # create the popup at the clicked location 
      isolate({
         map$showPopup(event$lat, event$lng, content )
      })
   })
   session$onSessionEnded(clickObs$suspend)
 
})