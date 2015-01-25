---
title       : Exploring Weather Events
subtitle    :  
author      : Ste Hyr
job         :  
framework   : io2012        # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---
 

## Overview

Extrem weather-events such as Tornados and Floods can cause human lives. With the climate change, the probability of and therefore the danger of extrem weather events increases. 

For the US, the Shiny-App on "Exploring Weather Events" allows to visuallize number, impact and location of recorded events. 

![plot of chunk unnamed-chunk-1](assets/fig/unnamed-chunk-1-1.png) 

--- 

## Event Explorer

The Event-explorer allows to select different event types, one or all years, the number of displayed events and the damage type. A short summary about the number of events and  relative/absolut caused damage provides a brief overview. 

Development over the years can be explored as animation (Play button under slider)

![plot of chunk unnamed-chunk-2](assets/fig/unnamed-chunk-2-1.png) 

---

## Single Events

Clicking on an Event opens a popup with details about the Event.

![plot of chunk unnamed-chunk-3](assets/fig/unnamed-chunk-3-1.png) 

--- 

## Impressum

**Data** US National Weather Service [here(PDF-file)](http://www.weather.gov/)

**Layout** based on [SuperZip App](https://github.com/jcheng5/superzip) by [Joe Cheng](https://github.com/jcheng5)

[Github Code](https://github.com/SteHyr/r_shiny_WeatherEvents) with documentation

[App Link ](https://stehyr.shinyapps.io/r_shiny_WeatherEvents/)
