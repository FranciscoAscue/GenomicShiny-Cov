################################################################# GenomicShiny-cov v.0.0.1######################################################################

#### Source scripts 
source("R/data-preprocesing.R", local = TRUE) # Pre processing data with scripts (pacient status metadata from GISAID)
source("R/ui-panel.R", local = TRUE)
source("R/dependencies.R", local = TRUE)
source("config.R", local = TRUE)
source("tests/test_metadata.R", local = TRUE)
# source("R/data-mysql.R", local = TRUE) # Optional management data from MySQL database 


#### Plot functions ####

line_stack_plot <-  function(data, stack){
  data <- ungroup(data)
  if(stack == "stack"){
    plot <- plot_ly(data, x = ~Date, y = ~Frecuency, name = ~Select, groupnorm = "Percentaje", 
                    text = ~paste("CDC week:", Epi.Week), type = 'scatter', mode ="lines", stackgroup = 'two')
  }else{
    plot <- plot_ly(data , x = ~Date, y = ~Frecuency, name = ~Select, groupnorm = "Percentaje", 
                    text = ~paste("CDC week:", Epi.Week), type = 'scatter', mode ="lines")
  }
  
  return(plot)
}

hist_plot <- function(data, lineage = "AY.102", ndf = 5){
  
  if(is.null(data$Date)){
    return(plot_ly(data.frame(NULL), type = "scatter", mode ="line"))
  }else{
    model <- lm(Frecuency ~ ns(Date, df = ndf), data = data)
    plot <- plot_ly(data, x = ~Date , y = ~Frecuency, name = lineage, type = 'bar', color = I("light blue"),
                    hoverinfo = ~Frecuency, text = ~paste("CDC week:", epi_week)) %>%
      add_trace(x = ~Date, y = ~fitted(model), type = "scatter", mode ="line", color = I("red"))
    
    return(plot)
  }
}


leaflet_plot <- function(data, palette, titleLegend, scale=FALSE, long = FALSE, lat = FALSE, 
                         var = FALSE, total = FALSE){
  basemap <- leaflet(data) %>% 
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(stroke = FALSE, smoothFactor = 0.4, fillOpacity = 1, fillColor = ~palette(N),
                label = ~paste0(Location, ": ", formatC(N, big.mark = ","))) %>%
    addLegend(pal = palette, values = ~N, title = titleLegend, opacity = 1.0) 
  
  if(!isFALSE(lat) & !isFALSE(long) & !isFALSE(var) & !isFALSE(total)){
    basemap <- basemap %>% addMinicharts(long, lat ,type = "pie", chartdata = var, opacity = 0.8, 
      colorPalette = brewer.pal(n = ncol(var), name = "Paired"), 
      width = 50 * sqrt(total) / sqrt(max(total)), transitionTime = 0)
    
    return(basemap)
  }
  
  return(basemap)
}


#### ui shiny ####
ui <- bootstrapPage( navbarPage(theme = shinytheme("flatly"), "GenomicShiny-Cov", id="nav", 
                                UploadData, MapStatistics, AnalysisLineages) )

#### server shiny ####

server <- function(input, output){
  ### upload data
  # geoJSON data from URL / file
  geojson <- reactive({
    
    url <- input$geojsonurl
    if(!is.null(input$geojson)){
      map <- geojsonsf::geojson_sf(geojson = input$geojson$datapath)
      return(map)
    }
    map <- geojsonsf::geojson_sf(geojson = url)
    map$Location <- toupper(map$Location)
    return(map)
  })
  
  geodownload <- reactive({
    search <- gsub(" /", "", input$pais, fixed = TRUE)
    org <- getData("GADM", country=search, level=1)
    org
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
      metadata <- metadata_preprosesing(metadata, input$Location, input$pais, geojson())
      colnames(metadata) <- col$NEW
      
    }else{
      metadata <- read.csv(input$metadata$datapath, sep = input$separator, header = TRUE)
      metadata <- metadata %>% filter(nchar(as.character(date)) == 10)
      metadata <- add_epi_week(metadata, "date", system = "cdc")
      
      metadata$Date <- epi_week_date(metadata$epi_week, metadata$epi_year,system = "cdc")
    }
    
    return(metadata)
  })
  
  # epidemio metadata
  
  epidem_data <- reactive({
    req(input$emetadata)
    if(is.null(input$emetadata)){
      
      return(NULL)
    }
    
    emetadata <- read.csv(input$emetadata$datapath, sep = input$Episeparator)
    emetadata$date <- as.Date(as.character(emetadata$date),  format = input$DateFormat)
    
    return(emetadata)
  })
  
  #### shiny reactive -> Output ### 
  
  var_datamap <- reactive({

    datamap <- variant_distribution(map = geojson(), epidem = epidem_data(), 
                                    metadata = meta(), input$Daterange[1], input$Daterange[2],input$switch, input$EpidemInput)
    
    return( list( df = datamap$df, pal = datamap$pal, long = datamap$long, lat = datamap$lat, 
                  var = datamap$var, total = datamap$total))
  })
  
  sampling_datamap <- reactive({
    req(input$Variant)
    data_map <- sampling_distribution(map = geojson(), metadata = meta(), mindate = input$Daterange[1], 
                                      maxdate = input$Daterange[2], input$Variant, input$Escala)

    return(list(df = data_map$df, pal = data_map$pal))
    
  })
  
  lineage_var_data <- reactive({
    metadata <- as.data.frame(meta())
    metadata <- stackvariant(metadata,  input$lineageDate[1], input$lineageDate[2], 
                             input$ngenomes, input$Varline)
    
  })
  
  hist_data <- reactive({
    
    metadata <- as.data.frame(meta())
    metadata$date <- as.Date(metadata$date)
    data_lineage <- freq_voc_voi(metadata, input$lineage)
    data_lineage
  })
  
  heatmap_data <- reactive({
    cuadro_motivo <- matrix_distribution(metadata = meta(), mindate = input$heatmapDate[1], 
                                         maxdate = input$heatmapDate[2], frecuency = input$mfrecuency, transp = input$transpose )
  })
  
  MetadataTest <- reactive({
    data <- metadata_test(metadata = meta(),  geojson = geojson(), epidemio = epidem_data())
  })
  
  mutation_data <- reactive({
    req(input$Lineages)
    data <- mutations(meta(),freq = input$pfrecuency, gene =  input$Gene, lineage=input$Lineages)
  })
  
  mutatation_change <- reactive({
    req(input$Gene)
    req(input$Lineages)
    mutations_perfil  <- split_lineages(meta(), input$Lineages, input$Gene, input$pfrecuency )
    if(mutations_perfil$table == "NaN"){
      mt = matrix(0, 8, 4)
      mt = as.data.frame(mt)
      colnames(mt) = c(rep("No Mutations",4))
      mutations_perfil$table = mt
    }
    return(list(mutations_list = mutations_perfil$mutations,  heatmap_mutations = mutations_perfil$heatmap, mutations_table = mutations_perfil$table))
    
  })
  
#### shiny Output ####
  
  dataModal <- function(failed = FALSE) {
    modalDialog(
      shinycssloaders::withSpinner(
        tableOutput("metadataTest"),
        type = 7, color.background = "white"),
      easyClose = TRUE,
      footer = NULL
    )
  }
  
  observeEvent(input$RunTest, {
    showModal(dataModal())
  })
  
  output$map <- renderLeaflet({
    basemap <- leaflet_plot(data = sampling_datamap()$df, palette = sampling_datamap()$pal, 
                            titleLegend = "Nº\nGenomes" )   

  })
  
  output$leaflet_map <- renderLeaflet({
    basemap <- leaflet_plot(data = var_datamap()$df, palette = var_datamap()$pal,
                            long = var_datamap()$long, lat = var_datamap()$lat, 
                            titleLegend = "Mortality rate x10⁵", var = var_datamap()$var,
                            total = var_datamap()$total)
  })
  

  
  output$heatmap <- renderPlotly({
    plot_ly(x = colnames(heatmap_data()), y = rownames(heatmap_data()), 
            z = heatmap_data() , type = "heatmap")
  })
  
  output$lineplot <- renderPlotly({ 
    line_stack_plot(lineage_var_data(), input$stack)
  })
  
  output$hist <- renderPlotly({ 
    hist_plot(hist_data(), lineage = input$lineage) 
  })
  
  output$mutation <- renderPlotly({
    fig <- plot_ly(mutation_data(),x = ~week_date, y = ~freq, type = 'scatter', 
                   name=~mutation,mode = 'lines') 
    fig
  })
  
  output$perfil_mutations <- renderPlotly({
    tryCatch({
      fig <- ggplot(mutatation_change()$heatmap_mutations, aes(x = epi_week, y=Profiles,fill = count)) +
        scale_fill_gradient(low="#D5DBDB", high="red") + geom_tile() + theme_bw()
      ggplotly(fig)
    },
    
    error = function(e){
      mt = matrix(0, 10, 10)
      mt = as.data.frame(mt)
      plot_ly(x = colnames(mt), y = rownames(mt), z = mt, type = "heatmap")
    })

  })
  
  output$metadataTest <- renderTable({
    isolate({
      MetadataTest()
    })
  }, sanitize.text.function = function(x) x)

  output$tabla <- DT::renderDataTable(head(meta()), 
              options = list(scrollX = TRUE),rownames = FALSE)
  
  output$mutation_tabla <- DT::renderDataTable(mutatation_change()$mutations_table, 
               options = list(scrollX = TRUE), rownames = FALSE)
  
  
  output$downloadjson <- downloadHandler(
    filename = function() {
      paste("map.geojson")
    },
    content = function(file) {
      writeOGR(geodownload(), file, layer = "org", driver="GeoJSON")
    }
  )
  
  
  
  
  
  #### renderUI panels and selectInput ####
  
  output$selectLocation <- renderUI({
    if(input$selectInput == "Custom")
      return()
    
    selectInput(inputId = "Location", 
                label = "Select a Location", 
                choices = as.data.frame(LocationCountry$Location, 
                                        col.names = "Location"),
                selected = "South America /")
  })
  
  output$selectCountry <- renderUI({
    
    if(is.null(input$Location) | input$selectInput == "Custom")
      return()
    
    Fill <- filter(LocationCountry, Location == input$Location)
    Lista <- as.data.frame(strsplit(Fill$Country,","),col.names = "Country")
    
    selectInput(inputId = "pais", 
                label = "Select a Country", 
                choices = Lista,
                selected = "Total")
  })
  
  output$selectVariants <- renderUI({
    if(is.null(input$metadata))
      return()
    
    Vocvoi <- c("Total")
    Vocvoi <- append(Vocvoi, unique(meta()$VOC.VOI))
    
    selectInput(inputId = "Variant", 
                label = "Select a variant (VOC-VOI)", 
                choices = as.list(Vocvoi),
                selected = "Total")
  })
  
  output$selectLineages <- renderUI({
    if(is.null(input$metadata))
      return()
    
    selectInput(inputId = "Lineages", 
                label = "Select a lineage", 
                choices = as.list(unique(meta()$lineage)),
                selected = NA)
    
  })
  
}

shinyApp(ui = ui, server = server)
