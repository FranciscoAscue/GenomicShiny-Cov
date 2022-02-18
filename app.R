################################################################# GenomicShiny-cov v.0.0.1######################################################################

## Source scripts 
source("R/data-preprocesing.R", local = TRUE) # Pre processing data with scripts (pacient status metadata from GISAID)
# source("R/data-mysql.R", local = TRUE) # Optional management data from MySQL database 
source("R/ui-panel.R", local = TRUE)
source("R/dependencies.R", local = TRUE)
source("config.R", local = TRUE)

################################################################################################################################################################

## Plot functions
# heat map of lineage frequency and region distribution 

#hovertemplate = paste('CDC: %{text}', '<br>%{y:.0%}<br>')
line_stack_plot = function(data, stack){
  data <- ungroup(data)
  if(stack == "stack"){
      plot <- plot_ly(data, x = ~Date, y = ~Frecuency, name = ~Select,
                      groupnorm = "Percentaje", text = ~paste("CDC week:", Epi.Week),
                      type = 'scatter', mode ="lines", stackgroup = 'two')
    }else{
      plot <- plot_ly(data , x = ~Date, y = ~Frecuency, name = ~Select,
                      groupnorm = "Percentaje", text = ~paste("CDC week:", Epi.Week),
                      type = 'scatter', mode ="lines")
    }
    
    return(plot)
}


heatmap_plot = function(data, kmns, nk, mthd) { 
  if(kmns == "k_means"){
      phmap = pheatmap(data,cluster_rows = TRUE, cluster_cols = TRUE, kmeans_k = nk, 
                    clustering_distance_rows = mthd,clustering_distance_cols = mthd, 
                    cutree_rows = nk, cutree_cols = nk, color = RColorBrewer::brewer.pal(9, "BuGn"))
      return(phmap)
  } else{
    phmap = pheatmap(data,cluster_rows = TRUE, cluster_cols = TRUE,  
                     clustering_distance_rows = mthd,clustering_distance_cols = mthd, 
                     cutree_rows = nk, cutree_cols = nk, color = RColorBrewer::brewer.pal(9, "BuGn"))
    return(phmap)
  }
}

## ui shiny, source tab panel from ui-panel.R
ui <- navbarPage("GenomicShiny-Cov", id="nav",
           UploadData, MapStadictics, Analysis) 

## server shiny
server <- function(input, output){
  ## upload data 
  # geoJSON data from URL / file
  geojson <- reactive({
    url <- input$geojsonurl
    
    if(!is.null(input$geojson)){
      map <- geojson_sf(geojson = input$geojson$datapath)
      map2 <- rjson::fromJSON(file= input$geojson$datapath)
      return(list(map=map, map2 = map2))
    }
    
    map <- geojson_sf(geojson = url)
    map2 <- rjson::fromJSON(file= url)   
    return(list(map=map, map2 = map2))
  })
  
  # metadata upload (GISAID metadata / custom metadata) 
  meta <- reactive({
    req(input$metadata)
    if(is.null(input$metadata)){
      return(NULL)
    }
    
    if(input$selectInput == "GISAID"){
        req(input$pais)
        metadata <- read.csv(input$metadata$datapath, sep = input$separator, header = TRUE)
        metadata <- metadata_preprosesing(metadata,input$Location, input$pais, geojson()$map)
        colnames(metadata) <- col$NEW
        #metadata$date <- as.Date(metadata$date)
    }else{
      metadata <- read.csv(input$metadata$datapath, sep = input$separator, header = TRUE)
      metadata <- metadata %>% filter(nchar(as.character(date)) == 10)
    }
    return(metadata)
    })
  
  epidem_data <- reactive({
    req(input$emetadata)
    if(is.null(input$emetadata)){
      return(NULL)
    }
    
    emetadata <- read.csv(input$emetadata$datapath, sep = ";")
    emetadata$FECHA_FALLECIMIENTO <- as.Date(as.character(emetadata$FECHA_FALLECIMIENTO),format = "%Y%m%d")
    return(emetadata)
  })
  #dead <- read.csv("/home/shinyProject/shinycovid/data/fallecidos_covid.csv", sep = ";")
  #dead$FECHA_FALLECIMIENTO <- as.Date(as.character(dead$FECHA_FALLECIMIENTO),format = "%Y%m%d")
#   LocationCountry <- read.csv("Data/Location-country.txt", header = TRUE, sep = ";")
#   col <- read.csv("Data/colnames.csv", header = TRUE)
  
  datamap <- reactive({
    
    datos_ins <- as.data.frame(meta())
    datos_ins <- datos_ins %>% filter( date >= input$Daterange[1] , date <= input$Daterange[2] )
    centers <- as.data.frame(st_coordinates(st_centroid(geojson()$map)))
    x <- mean(centers$X)
    y <- mean(centers$Y)
    req(input$Variant)
    if(input$Variant == "Total"){
      datos_ins <- datos_ins
    } else { 
      a <- input$Variant
      datos_ins <- datos_ins %>% filter(VOC.VOI == a)
    }
    
    if(input$Escala == "linear"){
      conteo <- datos_ins %>% group_by(region) %>% summarise( n = n())
    } else{
      conteo <- datos_ins %>% group_by(region) %>% summarise( n = log10(n()))
    }
    
    conteo$region <- toupper(conteo$region)
    colnames(conteo) <- c("Location", "N")
    NOM <- conteo$Location
    N <- conteo$N
    return(list(df2 = NOM, df3 = N, df4 = "Reeds", x = x, y = y))
    
  })
  
  

  leaflet_data <- reactive({
    map <- geojson()$map
    cities <- as.data.frame(st_coordinates(st_centroid(map)))
    map$Location <- toupper(map$Location)
    cities$region <- map$Location
    datos_ins <- as.data.frame(meta())
    datos_ins$region <- toupper(datos_ins$region)
    datos_ins <- datos_ins %>% filter(date >= input$Daterange[1] , date <= input$Daterange[2])
    for( var in unique(datos_ins$VOC.VOI)){
      temp <- datos_ins %>% filter(VOC.VOI == var) %>% group_by(region) %>% summarise( !!paste0(var) := n())
      cities <- merge(x  = cities, y = temp, by = 'region', all = TRUE  )
    }
    total <- datos_ins %>% group_by(region) %>% summarise(total = n())
    cities <- merge(x  = cities, y = total, by = 'region', all = TRUE )
    
    conteo <- epidem_data() %>% filter(FECHA_FALLECIMIENTO >= input$Daterange[1], FECHA_FALLECIMIENTO <= input$Daterange[2])
    conteo <- conteo %>% group_by(DEPARTAMENTO) %>% summarise( n = n())
    conteo$DEPARTAMENTO <- toupper(conteo$DEPARTAMENTO)
    colnames(conteo) <- c("Location", "N")
    Merge_data <- inner_join(map,conteo, by = 'Location' )
    Merge_data$Ratio <- (Merge_data$N/Merge_data$Population)*1000000
    pal <- colorNumeric(  palette = "Greys", NULL)
    long <- cities$X
    lat <- cities$Y
    var <- cities[,4:(length(cities)-1)]
    total <- cities$total
    return( list( df = Merge_data, df2 = pal, long = long, lat = lat, var = var, total = total))
    
  })
  
  lineage_var_data <- reactive({
    datos_ins <- as.data.frame(meta())
    #datos_ins$date <- as.Date(datos_ins$date)
    datos_ins <- stackvariant(datos_ins, input$lineageDate[1], input$lineageDate[2], input$ngenomes, input$Varline)
    
  })
  
  hist_plot <- reactive({
    
    datos_ins <- as.data.frame(meta())
    datos_ins$date <- as.Date(datos_ins$date)
    data_lin <- freq_voc_voi(datos_ins, input$lineage)
    data_lin
  })
  
  
  heatmap_data <- reactive({
    data <- as.data.frame(meta())
    data$date <- as.Date(data$date)
    data <- data %>% filter(date >= input$heatmapDate[1], date <= input$heatmapDate[2])
    counts <- data %>% group_by(region, lineage) %>% 
      summarise(n = n())
    names(counts) <- c("region", "lineage", "Freq")
    
    counts$Freq <- log10(counts$Freq)
    #counts$Freq <- round(counts$Freq, digits = 3)
    counts <- as.data.frame(counts)
    cuadro_motivo <- create.matrix(counts, tax.name = "region",
                                   locality = "lineage",
                                   abund.col = "Freq",
                                   abund = TRUE)
    
    if(input$transpose == "lineages_row"){
      return(t(as.data.frame(cuadro_motivo)))
    }
    cuadro_motivo
  })
  
  
  ############################################################ Output ####################################################################################  
  
  
  output$map <- renderPlotly({
    
    fig <- plot_ly()
    fig <- fig %>% add_trace(
      type="choroplethmapbox",
      geojson=geojson()$map2,
      locations=datamap()$df2,
      z=datamap()$df3,
      colorscale=datamap()$df4,
      reversescale =F,
      marker=list(line=list(
        width=0),
        opacity=1
      ),
      featureidkey= "properties.Location"
    )
    fig <- fig %>% colorbar(title = "Nº\nGenomes")
    fig <- fig %>% layout(
      mapbox=list(
        style="carto-positron",
        zoom =4,
        center=list(lon= datamap()$x, lat=datamap()$y)), height = 500
    )
    fig
    
  })
  
  output$leaflet_map <- renderLeaflet({
    basemap <- leaflet(leaflet_data()$df) %>% 
      addTiles() %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.4, fillOpacity = 1,
                  fillColor = ~leaflet_data()$df2(Ratio),
                  label = ~paste0(Location, ": ", formatC(Ratio, big.mark = ","))) %>%
      addLegend(pal = leaflet_data()$df2, values = ~Ratio, title = "Tasa de Mortalidad", opacity = 1.0) %>%  
      addMinicharts(
        leaflet_data()$long, leaflet_data()$lat,
        type = "pie",
        chartdata = leaflet_data()$var, 
        opacity = 0.8,
        colorPalette = brewer.pal(n = 10, name = "Paired"), 
        width = 50 * sqrt(leaflet_data()$total) / sqrt(max(leaflet_data()$total)), transitionTime = 0) %>% 
      addMiniMap(toggleDisplay = TRUE, position = "bottomleft") 
    
    basemap
  })
  
  output$lineplot <- renderPlotly({ 
    line_stack_plot(lineage_var_data(), input$stack)
  })
  
  output$hist <- renderPlotly({ 
    plot_ly(hist_plot(), x = ~Date , y = ~Frecuency, type = 'bar', color = I("light blue"),
            hoverinfo = ~Frecuency, text = ~paste("CDC week:", epi_week))
  })
  
  output$heatmap <- renderPlot({
    heatmap_plot(heatmap_data(), input$Kmeans, input$clusters, input$method)
  })
  
  output$variants <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$metadata))
      return()
    
    # Get the data set with the appropriate name
    Vocvoi <- c("Total")
    Vocvoi <- append(Vocvoi, unique(meta()$VOC.VOI))
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "Variant", 
                label = "Select a variant (VOC-VOI)", 
                choices = as.list(Vocvoi),
                selected = "Total")
  })
  
  output$selectLocation <- renderUI({
    # If missing input, return to avoid error later in function
    if(input$selectInput == "Custom")
      return()
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "Location", 
                label = "Select a Location", 
                choices = as.data.frame(LocationCountry$Location, col.names = "Location"),
                selected = "South America /")
  })
  
  output$Country <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$Location) | input$selectInput == "Custom")
      return()
    Fill <- filter(LocationCountry, Location == input$Location)
    # Get the data set with the appropriate name
    Lista <- as.data.frame(strsplit(Fill$Country,","), col.names = "Country")
    
    # Create the checkboxes and select them all by default
    selectInput(inputId = "pais", 
                label = "Select a Country", 
                choices = Lista,
                selected = "Total")
  })
  
 
  output$tabla <- DT::renderDataTable(head(meta(), n = 100),
                                      options = list(scrollX = TRUE),
                                      rownames = FALSE)
  
}

shinyApp(ui = ui, server = server)
