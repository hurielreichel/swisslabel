library(shiny)
library(rgdal)
library(dplyr)
library(mapview)
library(mapedit)
library(leaflet)

# ui object
ui <- fluidPage(
  titlePanel(p("swisslabel", style = "color:#00468c")),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "filemap",
        label = "Upload map. Choose shapefile",
        multiple = TRUE,
        accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
      ),
      selectInput(
        inputId = "variableselected",
        label = "Select variable",
        choices = c("score", "area")
      ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)

# server()
server <- function(input, output) {
  data <- reactive({
    req(input$filedata)
    read.csv(input$filedata$datapath)
  })

  map <- reactive({
    req(input$filemap)

    # shpdf is a data.frame with the name, size, type and
    # datapath of the uploaded files
    shpdf <- input$filemap

    # The files are uploaded with names
    # 0.dbf, 1.prj, 2.shp, 3.xml, 4.shx
    # (path/names are in column datapath)
    # We need to rename the files with the actual names:
    # fe_2007_39_county.dbf, etc.
    # (these are in column name)

    # Name of the temporary directory where files are uploaded
    tempdirname <- dirname(shpdf$datapath[1])

    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }

    # Now we read the shapefile with readOGR() of rgdal package
    # passing the name of the file with .shp extension.

    # We use the function grep() to search the pattern "*.shp$"
    # within each element of the character vector shpdf$name.
    # grep(pattern="*.shp$", shpdf$name)
    # ($ at the end denote files that finish with .shp,
    # not only that contain .shp)
    map <- readOGR(paste(tempdirname,
      shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
      sep = "/"
    ))
    map
  })

  output$map <- renderLeaflet({
    if (is.null(data()) | is.null(map())) {
      return(NULL)
    }

    map <- map()
    data <- data()

    # Add data to map
    datafiltered <- data[which(data$score == input$scoreselected), ]
     map@data <- datafiltered

    # Create variableplot
    map$variableplot <- as.numeric(
      map@data[, input$variableselected])

    # Create leaflet
    pal <- colorBin("YlOrRd", domain = map$variableplot, bins = 7)
    labels <- sprintf("%s: %g", map$score, map$variableplot) %>%
      lapply(htmltools::HTML)

    l <- leaflet(map) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~ pal(variableplot),
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels
      ) %>%
      leaflet::addLegend(
        pal = pal, values = ~variableplot,
        opacity = 0.7, title = NULL
      )
  })
}

# shinyApp()
shinyApp(ui = ui, server = server)
