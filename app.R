# Clean workspace
rm(list =ls())

#Set up packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(highcharter)
library(leaflet)
library(sp)
library(rgdal)
library(exactextractr)


## Laterite colors for graphs
laterite_1 <- c("#D6EDD8", "#7DD9BA", "#ABE1B9") # to be used for single-select graphs
laterite_2 <- c("#7DD9BA") # to be used for multiple-select graphs
laterite_3 <- c("#DA302C", "#F27317", "#241F21") # to be used for multiple-select LINE graphs (over time)
laterite_maps <- c("#7DD9BA") # to be used for bubbles in maps

#User Interface Elements-------------------------------------------------------------------- 
ui <- fluidPage(tags$head(tags$title("Laterite - Geospatial Data")),
                #Social Media Icon Colors
                tags$style(
                  ".fa-facebook{color:#000000}"),
                tags$style(
                  ".fa-linkedin{color:#000000}"
                ),
                tags$style(
                  ".fa-twitter{color:#000000}"
                ),
                tags$style("a{color:black; font-size:13px;}"),
                tags$style(".textbox{font-size:14px; text-align:justify; border: 2px solid #DA302C; padding: 5px; box-shadow: 4px 4px 10px 2px #E8C4AA;}")
                
, setBackgroundColor(color = "#C5E3C6"), theme = "bootstrap.css", useShinydashboard(),

## Header -----------------------------------------------------------

titlePanel(fluidRow(column(width=3, img(src ="laterite-logo-dark.svg", height = 70, width = 130)))),

## Body -----------------------------------------------------------

tabsetPanel(type = c("tabs"),
tabPanel(h5(strong("Introduction")),
    #fluidRow(style = "background-color:#FFFFFF;", HTML('<p><img src="INSERT IMAGE" width="100%"/></p>')), #Host the image on imgur and then 
    fluidRow(style = "background-color:#FFFFFF;", br()), #Code for a line of whitespace
    fluidRow(style = "background-color:#FFFFFF;", column(width = 6, offset = 3, h2(style = "text-align: center;", "Welcome to the Laterite geospatial data dashboard!"), hr(), tags$p(style = "font-size:16px; text-align:justify; border: 2px solid #DA302C; border-radius: 25px; padding: 20px;", "This tool will allow you to obtain data on many different geospatial variables for the four Laterite african countries. This data can be used to gain more insights on most research projects, so feel free to take a look. Geospatial data can be accessed in two ways : First, it can be accessed for specific GPS coordinates, for example ones you would get with survey data. Then, it can also be accessed for specific administrative units (e.g. regions, districts, or below) for which the resulting data is some descriptive statistics about how the geospatial variables are distributed in that unit.")))
), 

tabPanel(h5(strong("Variables")),
  fluidRow(style = "background-color:#FFFFFF;", br()),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Variables description")))),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, tags$p(class = "textbox", "Choose a country to get a description of all the geospatial variables that are available for that country. For each variable, you will get information such as its source, its theme and at what resolution it is available."))),
  fluidRow(style = "background-color:#FFFFFF;", br()),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, selectInput("countrySelectVar",h4("Select Country"),c("Ethiopia", "Kenya",  "Rwanda", "Uganda")))),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 10, offset = 1, DT::dataTableOutput("vardesc")))
),

tabPanel(h5(strong("From GPS coordinates")),
  #fluidRow(style = "background-color:#FFFFFF;", HTML('<p><img src="IMAGE SOURCE ON IMGUR" width="100%"/></p>')), #Host the image on imgur and then 
  fluidRow(style = "background-color:#FFFFFF;", br()),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Loading GPS coordinates")))),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, tags$p(class = "textbox", "Select a CSV file containing GPS coordinates for which you want to extract geospatial data, and the country they're situated in. Then, select the variables that represent the longitude and the latitude in your data."))),
  fluidRow(style = "background-color:#FFFFFF;", br()), 
  fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, selectInput("countryselect",h4("Select Country"),c("Ethiopia", "Kenya",  "Rwanda", "Uganda")))),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, fileInput("surveydata", label = h4("Enter survey data CSV"), buttonLabel = h4("Browse"), accept = c("text/csv",
                                                                                                                                    "text/comma-separated-values,text/plain",
                                                                                                                                    ".csv"))), column(width = 3, uiOutput("longitudevarSelect")), column(width = 3, uiOutput("latitudevarSelect")), column(width = 1, uiOutput("gobutton"))),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 10, offset = 1, conditionalPanel("input.gobtton", leafletOutput("map")))),
  uiOutput("variableSelectCoord"),
  uiOutput("varrep"),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 10, offset = 1, DT::dataTableOutput("summary"))),
  fluidRow(style = "background-color:#FFFFFF;", br()),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 10, offset = 1, conditionalPanel("input.plot", highchartOutput("varplot")))),
  uiOutput("downloadCoord"), 
),

tabPanel(h5(strong("By administrative units")),
  fluidRow(style = "background-color:#FFFFFF;", br()),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Administrative Unit Selection")))),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, tags$p(class = "textbox", "Select the country you're interested in and click on the button. It may take some time because all the administrative units from that country have to be loaded."))),
  fluidRow(style = "background-color:#FFFFFF;", br()),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, selectInput("countrySelectUnit",h4("Select Country"),c("Ethiopia", "Kenya",  "Rwanda", "Uganda"))), actionButton("unitsbutton", h3("Load units"), icon("layer-group"), class = "btn-outline-success btn-lg")),
  uiOutput("unitchoice"),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 10, offset = 1, conditionalPanel("input.addUnitButton", leafletOutput("unitsMap")))),
  uiOutput("variableSelectUnit"),
  fluidRow(style = "background-color:#FFFFFF;", br()),
  fluidRow(style = "background-color:#FFFFFF;", column(width = 9, offset = 1, DT::dataTableOutput("unitStats"))),
  uiOutput("downloadUnit")
         
         
)

), 
## Footer -----------------------------------------------------------
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style = "background-color:#C5E3C6;",column(width=2, offset = 1, tags$a(href= "https://www.laterite.com/", img(src ="laterite-logo-dark.svg", height = 80, width = 150)), br(), tags$a(href="https://web.facebook.com/Laterite/?_rdc=1&_rdr", icon("facebook", lib = "font-awesome")), tags$a(href="https://twitter.com/LateriteAfrica", icon("twitter", lib = "font-awesome")), tags$a(href="https://www.linkedin.com/company/laterite", icon("linkedin",lib = "font-awesome"))), column(width=2, offset = 1,br(),br(), h3(strong("Get in touch")), tags$a(href = "https://www.laterite.com/contact/", "Contact us")), column(width = 2, offset = 2, br(), br(), h3(strong("From data to policy")), tags$a(href="https://www.laterite.com/services/data-collection/", "Data |"), tags$a(href="https://www.laterite.com/services/research-services/", "Research |"), tags$a(href = "https://www.laterite.com/services/advisory-services/", "Advisory "))),
#fluidRow(style = "background-color:#C5E3C6;", column(width=2, offset = 1, tags$a(href="https://web.facebook.com/Laterite/?_rdc=1&_rdr", icon("facebook", lib = "font-awesome")), tags$a(href="https://twitter.com/LateriteAfrica", icon("twitter", lib = "font-awesome")), tags$a(href="https://www.linkedin.com/company/laterite", icon("linkedin",lib = "font-awesome"))), column(width = 2, offset = 1, tags$a(href = "https://www.laterite.com/contact/", "Contact us")), column(width = 4, offset = 2, tags$a(href="https://www.laterite.com/services/data-collection/", "Data |"), tags$a(href="https://www.laterite.com/services/research-services/", "Research |"), tags$a(href = "https://www.laterite.com/services/advisory-services/", "Advisory "))),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;",column(width=2, br())),
fluidRow(style= "background-color:#C5E3C6;")
)

#Server--------------------------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  
#Introduction Page--------------------------------------------------------------------- 
  
#Variables Page-------------------------------------------------------------------------

  
#Show the variables table for the selected country
output$vardesc <- DT::renderDataTable({
  datatable(read.csv(paste0("Data/", input$countrySelectVar, "/", input$countrySelectVar, "_variables.csv")), class = "hover cell-border", rownames = FALSE, options = list(pageLength = 50, searchHighlight = TRUE, orderClasses = TRUE, initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#7DD9BA', 'color': '#fff'});",
    "}")
    )
  )
})


#From coordinates Page-------------------------------------------------------------------------

#Loading the user CSV and choosing which variables are the latitude and longitude

surveydata <- reactive({
  req(input$surveydata)
  read.csv(input$surveydata$datapath)
})

output$latitudevarSelect = renderUI({
  req(surveydata())
  selectInput("lat", h4("Select Latitude Variable"), names(surveydata()))
})

output$longitudevarSelect = renderUI({
  req(surveydata())
  selectInput("lon", h4("Select Longitude Variable"), names(surveydata()))
})

output$gobutton <- renderUI({
  req(input$surveydata)
  actionButton("gobtton", h3("Go!"), icon("globe-africa"), class = "btn-outline-success btn-lg")
})

surveydata_coord <- eventReactive(input$gobtton, {
  req(input$surveydata, input$lat, input$lon)
  if(input$lat != input$lon){
    rename(surveydata(), lat = input$lat, lon = input$lon) %>% select(lon, lat)
  }
})


#Showing the map of the survey data

country <- eventReactive(input$gobtton, {
  req(tolower(input$countryselect))
})


output$map <- renderLeaflet({
  coord <- req(surveydata_coord())
  leaflet(coord) %>% addTiles() %>% addCircleMarkers(color = "#DA302C", radius = 1, opacity = 0.5)
})


#Giving the choice of variables to the user


# Function that returns a box which contains checkboxes for all the variables available from a given source
getVarBox <- function(src, country, tab){
  res <- list.dirs(paste0("Data/", country, "/", src), full.names = FALSE)[-1]
  res <- res[!grepl("/", res)]
  vars_unique <- list()
  vars_multi <- list()
  for(r in res){
    vars_multi[[r]] <- list.dirs(paste0("Data/", country, "/", src, "/", r), full.names = TRUE)[-1]
    names(vars_multi[[r]]) <- unname(sapply(vars_multi[[r]], function(x){ substr(x, max(which(strsplit(x, "")[[1]]=="/")) + 1, nchar(x))}))
    vars_unique[[r]] <- setdiff(list.files(paste0("Data/", country, "/", src, "/", r), full.names = TRUE), vars_multi[[r]])
    names(vars_unique[[r]]) <- unname(sapply(vars_unique[[r]], function(x){ substr(x, max(which(strsplit(x, "")[[1]]=="/")) + 1, nchar(x) - 4)}))
  }
  checkbox <- list()
  for(r in res){
    checkbox[[length(checkbox)+1]] <- checkboxGroupInput(paste0(tab, "var", src, r), label = h3(r), choiceNames = names(vars_unique[[r]]), choiceValues = unname(vars_unique[[r]]))
    for(i in seq_len(length(vars_multi[[r]]))){
      v <- vars_multi[[r]][i]
      choices <- list.files(v, full.names = TRUE)
      names(choices) <- unname(sapply(choices, function(x){ substr(x, max(which(strsplit(x, "")[[1]]=="/")) + 1 + nchar(names(v)), nchar(x) - 4)}))
      checkbox[[length(checkbox)+1]] <- selectInput(paste0(tab, "var", src, names(v)), choices = choices, multiple = TRUE, label = names(v))
    }
  }
  
box(title = h1(src, align = "center", style = "color: #FFFFFF"), width = NULL, solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE, status = "success", checkbox)
}

output$variableSelectCoord <- renderUI({
  req(surveydata_coord())
  tags <- tagList()
  tags[[1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[2]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1,  br(h1("Geospatial variables selection"))))
  tags[[3]] <- fluidRow(style = "background-color:#FFFFFF;", br(), column(width = 8, offset = 1, HTML("<p class = 'textbox'> Now, select the geospatial variables you want extracted for your data. The variables are sorted by sources and resolutions, and they can be selected by checking the box next to them. For variables with multiple time observations available, you can select any you want by clicking on the input bar.</p>")))
  tags[[4]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  src <- list.dirs(paste0("Data/", country()), recursive = FALSE, full.names = FALSE)
  box <- lapply(src, getVarBox, country = country(), tab = "coord")
  i <- 1
  while(i+2 <= length(box)){
     tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, box[[i]]), column(width = 3, box[[i+1]]), column(width = 3, box[[i+2]]))
     i <- i+3
  }
  if(i < length(box)) tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, box[[i]]), column(width = 3, box[[i+1]]))
  if(i == length(box)) tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, box[[i]]))
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, actionButton("coordextractbutton", h3("Extract"), icon("table"), class = "btn-outline-success btn-lg")))
  tags
})


#Extracting the chosen geospatial variables for the loaded GPS coordinates

geoDataCoord <- eventReactive(input$coordextractbutton, {
  vars <- unname(unlist(isolate(reactiveValuesToList(input))[grepl("coordvar", names(input))]))   #Getting the variables that were checked and putting them in a list
  res <- factor(unlist(lapply(vars, function(x){ slash <- gregexpr("/", x)[[1]]                   #Getting the resolutions of these variables
                                   substr(x, slash[3]+1, slash[4]-1 )})))
  vars_res <- list()
  for(l in levels(res)){
    vars_res[[l]] <- vars[res==l]   #Separating the variables per resolution to know which ones to stack together
  }
  data <- surveydata_coord()
  stacks <- list()
  for(i in 1:length(levels(res))){
    stacks[[i]] <- raster::extract(raster::stack(vars_res[[i]]), data)
  }
  cbind(surveydata(), do.call("cbind", stacks))
})


#Making the user choose a geospatial variable from the extracted data to plot (histogram for continuous) and give a statistical summary

output$varrep <- renderUI({
  req(geoDataCoord())
  tagList(
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Geospatial variables distribution")))),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, tags$p(class = "textbox", "For each of the variables extracted, you can check a statistical summary and an histogram of its distribution among the GPS coordinates loaded."))),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", column(width = 4, offset = 1, selectInput("plot", h4("Select Variable"), setdiff(names(geoDataCoord()), names(surveydata())))))
  )
  
})

output$summary <- DT::renderDataTable({
  req(geoDataCoord())
  req(input$plot)
  v <- geoDataCoord()[, input$plot]
  df <- data.frame(Minimum = min(v,na.rm = TRUE), `First Quartile` = quantile(v, 0.25,na.rm = TRUE), Median = median(v,na.rm = TRUE), Mean = mean(v,na.rm = TRUE), `Third Quartile` = quantile(v, 0.25,na.rm = TRUE), Maximum = max(v,na.rm = TRUE), `Standard Deviation` = sd(v,na.rm = TRUE))
  df <- apply(df, c(1,2), round, digits = 2)
  datatable(df, class = "hover cell-border", rownames = FALSE, options = list(ordering = FALSE, dom = "t", initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#7DD9BA', 'color': '#fff'});",
    "}")
  )
  )
})

output$varplot <- renderHighchart({
  req(geoDataCoord())
  req(input$plot)
  h <- hchart(hist(geoDataCoord()[, input$plot]), color = "#7DD9BA", name = input$plot, showInLegend = FALSE)
})


#Making the geospatial data we obtained available to download as a CSV

output$downloadDataCoord <- downloadHandler(
  filename = function() {
    paste0(substr(input$surveydata$name, 0, nchar(input$surveydata$name)-4), "_Geospatial.csv")
  },
  content = function(file) {
    write.csv(geoDataCoord(), file, row.names = FALSE)
  },
  contentType = "text/csv"
)

output$downloadCoord <- renderUI({
  req(geoDataCoord())
  tagList(
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Data Exportation")))),
    fluidRow(style = "background-color:#FFFFFF;", br(), column(width = 8, offset = 1, HTML("<p class = 'textbox'> You can choose to export the extracted data as a CSV. The CSV you obtain will be the original loaded CSV along with the variables you selected for each row.</p>"))),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", column(width = 6, offset = 1, downloadButton("downloadDataCoord", h3("Export as CSV"), class = "btn-outline-success btn-lg"))),
    fluidRow(style = "background-color:#FFFFFF;", br())
  )
})

#By administrative units Page-------------------------------------------------------------------------


#Names of the variable representing the name and the code of the administrative units, to update if the shape files change

unitsvarnames <- eventReactive(input$unitsbutton, {
  req(input$countrySelectUnit)
  switch(input$countrySelectUnit, "Ethiopia" = c("REGIONNAME", "ZONENAME", "WOREDANAME"),
                                "Rwanda" = c("NAME_1", "NAME_2", "NAME_3", "NAME_4", "NAME_5"),
                                "Kenya" = c("NAME_1", "NAME_2", "NAME_3", "NAME_4", "NAME_5"),
                                "Uganda" = c("NAME_1", "NAME_2", "NAME_3", "NAME_4") 
  )
})

unitsvarcodes <- eventReactive(input$unitsbutton, {
  req(input$countrySelectUnit)
  switch(input$countrySelectUnit, "Ethiopia" = c("RID", "Z4ID", "WOREDANO_"),
         "Rwanda" = c("ID_1", "ID_2", "ID_3", "ID_4", "ID_5"),
         "Kenya" = c("ID_1", "ID_2", "ID_3", "ID_4", "ID_5"),
         "Uganda" = c("ID_1", "ID_2", "ID_3", "ID_4") 
  )
})


#Loading the shapefiles for the selected country

units <- eventReactive(input$unitsbutton, {
  req(input$countrySelectUnit)
  levels <- list.dirs(paste0("data/shapes/", input$countrySelectUnit))[-1]
  units <- lapply(levels, function(l) {
    shp <- list.files(l, full.names = FALSE)[1]
    readOGR(l, substr(shp, 0, nchar(shp) - 4))
  })
  names(units) <- lapply(levels, function(x) { substr(x, regexpr("_", x)[[1]] + 1, nchar(x))})
  units
})

#Creating multiple dropdown select inputs allowing the user to choose the administrative units they're interested in

output$unitchoice <- renderUI({
  req(units())
  units <- units()
  varnames <- unitsvarnames()
  varcodes <- unitsvarcodes()
  tags <- tagList()
  tags[[1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[2]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[3]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, HTML("<p class = 'textbox'>You can now pick the administrative units you want by selecting them through the dropdown menus and clicking the Add Unit button. As you add units to your selection, they will also appear on the map.</p>")))
  tags[[4]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  listUnits <- paste(names(units)[1], units[[1]][[varcodes[1]]], "-", units[[1]][[varnames[1]]], sep = " ")
  names(listUnits) <- units[[1]][[varnames[1]]] 
  select <- list(selectInput(inputId = paste0("unit", names(units)[1]), label = h4(paste0("Select ", names(units)[1])), choices = listUnits))
  if(input$countrySelectUnit != "Ethiopia"){
    for(i in 2:length(units)){
      namevar <- varnames[i]
      codevar <- varcodes[i]
      nametopvar <- varnames[i-1]
      codetopvar <- varcodes[i-1]
      output[[paste0("out", names(units)[i])]] <- eval(parse(text = paste0("renderUI({
        topunit <- req(input[[paste0('unit', names(units)[", i, "-1])]])
        if(topunit != 'None'){
              spaces <- gregexpr(' ', topunit)[[1]]
              topunitcode <- substr(topunit, spaces[1] + 1, spaces[2] - 1)
              subunits <- base::subset(units[[", i, "]], ", codetopvar, " == topunitcode)
              if(length(subunits) > 0) {
                listSubunits <- paste(names(units)[", i, "], subunits$", codevar, ", '-', subunits$", namevar, ", sep = ' ')
                names(listSubunits) <- subunits$", namevar, "
              }
              else listSubunitd <- c()
        }
        else listSubunits <- c()
        selectInput(inputId = paste0('unit', names(units)[", i, "]), label = h4(paste0('Select ', names(units)[", i, "])), choices = c('None', listSubunits), selected = 'None')
      })")))
    select[[i]] <- uiOutput(paste0("out", names(units)[i]))
    }
  }
  else for(i in 2:length(units)){
    namevar <- varnames[i]
    codevar <- varcodes[i]
    output[[paste0("out", names(units)[i])]] <- eval(parse(text = paste0("renderUI({
        topunit <- req(input[[paste0('unit', names(units)[", i, "-1])]])
        if(topunit != 'None'){
              spaces <- gregexpr(' ', topunit)[[1]]
              topunitcode <- substr(topunit, spaces[1] + 1, spaces[2] - 1)
              subunits <- base::subset(units[[", i, "]], substr(", codevar, ", 0, 2*(", i, "-1)) == topunitcode)
              if(length(subunits) > 0) {
                listSubunits <- paste(names(units)[", i, "], subunits$", codevar, ", '-', subunits$", namevar, ", sep = ' ')
                names(listSubunits) <- subunits$", namevar, "
              }
              else listSubunits <- c()
        }
        else listSubunits <- c()
        selectInput(inputId = paste0('unit', names(units)[", i, "]), label = h4(paste0('Select ', names(units)[", i, "])), choices = c('None', listSubunits), selected = 'None')
      })")))
    select[[i]] <- uiOutput(paste0("out", names(units)[i]))
  }
  i <- 1
  while(i+2 <= length(select)){
    tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, select[[i]]), column(width = 3, select[[i+1]]), column(width = 3, select[[i+2]]))
    i <- i+3
  }
  if(i < length(select)) tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, select[[i]]), column(width = 3, select[[i+1]]))
  if(i == length(select)) tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, select[[i]]))
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, actionButton("addUnitButton", h4("Add unit"), icon("map-marked"), class = "btn-outline-success btn-lg")))
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, selectInput("listChosenUnits", label = "Chosen units", choices = NULL, multiple = TRUE)))
  tags
})


observeEvent(input$addUnitButton, {
  units <- req(units())
  varcodes <- unitsvarcodes()
  chosenunit <- "None"
  i <- length(units)
  while(chosenunit == "None" & i>0) {
    chosenunit <- input[[paste0("unit", names(units)[i])]]
    i <- i-1
  }
  if(chosenunit != "None") {
    newchosenunits <- c(input$listChosenUnits, chosenunit)
    updateSelectInput(session, inputId = "listChosenUnits", choices = c(chosenunit, input$listChosenUnits), selected = c(chosenunit, input$listChosenUnits))
  }
})


chosenUnits <- reactive({
  units <- req(units())
  listChosen <- req(input$listChosenUnits)
  varcodes <- req(unitsvarcodes())
  unitNames <- list()
  l <- lapply(listChosen, function(unit){
    spaces <- gregexpr(" ", unit)[[1]]
    unitType <- substr(unit, 0, spaces[1] - 1)
    unitID <- substr(unit, spaces[1] + 1, spaces[2] - 1)
    varcode <- varcodes[which(names(units) == unitType)]
    eval(parse(text = paste0("subset(units[[unitType]], ", varcode, " == unitID)")))
  })
  setNames(l, listChosen)
  
})


#Showing the chosen administrative units as polygons on a map

chosenUnitsPolygons <- reactive({
  req(input$listChosenUnits)
  chosenunits <- req(chosenUnits())
  poly <- list()
  for(i in seq_len(length(chosenunits))){
    poly[[i]] <- chosenunits[[i]]@polygons[[1]]
    poly[[i]]@ID <- as.character(i)
  }
  names <- lapply(names(chosenunits), function(x){
    spaces <- gregexpr(" ", x)[[1]]
    substr(x, spaces[3] + 1, nchar(x))
  })
  list(polygons = poly, names = names)
})

output$unitsMap <- renderLeaflet({
  chosenunitspoly <- req(chosenUnitsPolygons())
  leaflet(SpatialPolygons(chosenunitspoly$polygons)) %>% addTiles() %>% addPolygons(label = chosenunitspoly$names, color = "#DA302C", weight = 4, labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                                                                                                                                              style = list(
                                                                                                                                                                                "color" = "#DA302C",
                                                                                                                                                                                "font-family" = "serif",
                                                                                                                                                                                "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                                                                                                                                                "font-size" = "12px",
                                                                                                                                                                                "border-color" = "rgba(0,0,0,0.5)")))
})

#Giving the choice of variables to the user, like in the GPS coordinates page and also the statistics they want for these variables

output$variableSelectUnit <- renderUI({
  req(units())
  tags <- tagList()
  tags[[1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[2]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1,  br(h1("Geospatial variables selection"))))
  tags[[3]] <- fluidRow(style = "background-color:#FFFFFF;", br(), column(width = 8, offset = 1, HTML("<p class = 'textbox'> Now, select the geospatial variables you want extracted for your data. The variables are sorted by sources and resolutions, and they can be selected by checking the box next to them. For variables with multiple time observations available, you can select any you want by clicking on the input bar. For administrative units, the returned data represents statistics which have been computed on all the raster cells present in that unit. For that reason, you also have to select which one of these statistics you want to have.</p>")))
  tags[[4]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  src <- list.dirs(paste0("Data/", input$countrySelectUnit), recursive = FALSE, full.names = FALSE)
  box <- lapply(src, getVarBox, country = input$countrySelectUnit, tab = "unit")
  i <- 1
  while(i+2 <= length(box)){
    tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, box[[i]]), column(width = 3, box[[i+1]]), column(width = 3, box[[i+2]]))
    i <- i+3
  }
  if(i < length(box)) tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, box[[i]]), column(width = 3, box[[i+1]]))
  if(i == length(box)) tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, box[[i]]))
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 11, offset = 1, checkboxGroupInput("stats", label = h3("Choose statistics"), inline = TRUE, choiceValues = list("min", "max", "mean", "quantile", "median", "variance", "stdev"), choiceNames = list(h4("Min"), h4("Max"), h4("Mean"), h4("First and third quartile"), h4("Median"), h4("Variance"), h4("Standard Deviation")) )))
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", br())
  tags[[length(tags)+1]] <- fluidRow(style = "background-color:#FFFFFF;", column(width = 3, offset = 1, actionButton("unitextractbutton", h3("Extract"), icon("table"), class = "btn-outline-success btn-lg")))
  tags
})

#Extracting the chosen statistics for the chosen geospatial variables for each of the chosen administrative units

geoDataUnit <- eventReactive(input$unitextractbutton, {
  stats <- req(input$stats)
  chosenUnits <- req(chosenUnits())
  vars <- unname(unlist(isolate(reactiveValuesToList(input))[grepl("unitvar", names(input))]))   #Getting the variables that were checked and putting them in a list
  res <- factor(unlist(lapply(vars, function(x){ slash <- gregexpr("/", x)[[1]]                   #Getting the resolutions of these variables
  substr(x, slash[3]+1, slash[4]-1 )})))
  vars_res <- list()
  for(l in levels(res)){
    vars_res[[l]] <- vars[res==l]   #Separating the variables per resolution to know which ones to stack together
  }
  stacks <- list()
  for(i in seq_len(length(levels(res)))){
    ras <- raster::stack(vars_res[[i]])
    values <- list()
    for(j in seq_len(length(chosenUnits))){
      values[[j]] <- exact_extract(ras, chosenUnits[[j]], fun = stats, full_colnames = TRUE, quantiles = c(0.25, 0.75))
    }
    stacks[[i]] <- do.call("rbind", values)
  }
  table <- do.call("cbind", stacks)
  row.names(table) <- names(chosenUnits)
  table
})


#Showing the results, i.e a table with one row for each chosen administrative units and the chosen statistics as columns
output$unitStats <- DT::renderDataTable({
  table <- req(geoDataUnit())
  table <- apply(table, c(1,2), round, digits = 2)
  datatable(table, class = "hover cell-border", options = list(ordering = FALSE, dom = "t", scrollX = TRUE,  initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#7DD9BA', 'color': '#fff'});",
    "}")
  )
  )
})

#Making these geospatial statistics available to download as a CSV

output$downloadDataUnit <- downloadHandler(
  filename = function() {
    paste0("Geospatial_Data_", input$countrySelectUnit, ".csv")
  },
  content = function(file) {
    write.csv(geoDataUnit(), file)
  },
  contentType = "text/csv"
)

output$downloadUnit <- renderUI({
  req(geoDataUnit())
  tagList(
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", column(width = 8, offset = 1, br(h1("Data Exportation")))),
    fluidRow(style = "background-color:#FFFFFF;", br(), column(width = 8, offset = 1, HTML("<p class = 'textbox'> You can choose to export the extracted data as a CSV. The CSV you obtain will be the summary table you can see above.</p>"))),
    fluidRow(style = "background-color:#FFFFFF;", br()),
    fluidRow(style = "background-color:#FFFFFF;", column(width = 6, offset = 1, downloadButton("downloadDataUnit", h3("Export as CSV"), class = "btn-outline-success btn-lg"))),
    fluidRow(style = "background-color:#FFFFFF;", br())
  )
})


})


shinyApp(ui = ui, server = server)

