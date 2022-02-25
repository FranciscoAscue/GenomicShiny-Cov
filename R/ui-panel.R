################################################################################################################################################################
# Ui panel


UploadData <- tabPanel("Upload Data",
                       column(4, 
                              h3(p(style="color:black;text-align:left", 
                                   tags$img(src="https://upload.wikimedia.org/wikipedia/commons/3/34/GISAID_logo.png",width="120px",height="60px"),
                                   tags$img(src="https://cov-lineages.org/assets/images/pangolin.png",width="60px",height="60px"),
                                   tags$img(src="https://nextstrain.org/static/nextstrain-logo-small-ea8c3e13e8c17436264760d638ab970e.png",width="60px",height="60px")
                              )),
                              textInput(inputId = "geojsonurl",
                                        label = h4("Url to Geojson (Optional)"),
                                        value = NULL),
                              fileInput(inputId = "geojson", h4("Upload Geojson File"), 
                                        accept=c('.geojson',
                                                 '.geocsv')),
                              
                              column(3,radioButtons("selectInput", "Select input",
                                                    choices = list("GISAID" = "GISAID", "Custom Metadata" = "Custom"), selected = "GISAID")),
                              column(3,radioButtons("separator", "Select separator",
                                                    choices = list("Tab" = "\t", "Comma" = ","), selected = "\t")),
                              
                              uiOutput("selectLocation"),
                              uiOutput("Country"),
                              
                              fileInput(inputId = "metadata", h4("Upload metadata file (.csv/.tsv)"), 
                                        accept=c('text/csv',
                                                 'text/comma-separated-values,text/plain',
                                                 '.csv','.tsv')),
                              column(8,radioButtons("Episeparator", "Select epidemiological file separator",
                                                    choices = list("Tab" = "\t", "Comma" = ",", "semicolon" = ";"), selected = "\t", inline = TRUE)),
                              selectInput(inputId = "DateFormat", 
                                          label = "Select a date format", 
                                          choices = c("%Y%m%d","%Y-%m-%d","%Y/%m/%d","%d-%m-%Y","%d/%m/%Y"),
                                          selected = "South America /"),
                              fileInput(inputId = "emetadata", h4("Upload Epidemiological CSV File"), 
                                        accept=c('text/csv',
                                                 'text/comma-separated-values,text/plain',
                                                 '.csv','.tsv')),
                              actionButton(inputId = "RunAnalysis", 
                                           label = h4(icon(name = "file-import"),
                                                      "Run Analysis"),
                                           width = "200px")
                       ),
                       
                       column(8, 
                              shinycssloaders::withSpinner(
                                DT::dataTableOutput("tabla"), type = 7, color.background = "white")
                       ),
                       
                       column(8,
                              shinycssloaders::withSpinner(
                                tableOutput("metadataTest"), type = 7, color.background = "white")
                       ),
                       tags$style(".fa-check {color:#1AC20B}"),
                       tags$style(".fa-grip-lines {color:#E87722}"),
                       tags$style(".fa-exclamation {color:#F31507}"),
)


################################################################################################################################################################



################################################################################################################################################################
MapStadictics <- tabPanel( "Statistics",
                           
                           column(12, 
                                  column(6, shinycssloaders::withSpinner(leafletOutput(outputId = "leaflet_map", height = 550))),
                                  
                                  column(6,
                                         column(5, dateRangeInput("Daterange", "Select date range", 
                                                                  start  = "2020-03-01",
                                                                  end    = "2022-01-25")      
                                         ),
                                         column(4,
                                                
                                                uiOutput("variants"),
                                                
                                         ),
                                         column(3,
                                                radioButtons("Escala", "Select scale",
                                                             choices = list("linear" = "linear", "logarithmic" = "log"), selected = "log"),),
                                         column(12, shinycssloaders::withSpinner(plotlyOutput("map")))
                                         
                                  )
                           ),
                           
                           column(12, h3(" ")),
                           column(12, 
                                  
                                  column(6, shinycssloaders::withSpinner(plotlyOutput("lineplot"))),
                                  column(2,
                                         
                                         textInput(inputId = "lineage",
                                                   label = "Write a linage",
                                                   value = "AY.117"),
                                         #column(4,
                                         numericInput("ngenomes", label = "Minimun genomes", min = 1, 
                                                      max = 30, value = 1 ),#),
                                         dateRangeInput("lineageDate", "Select date range",
                                                        start  = "2021-06-01",
                                                        end    = "2022-01-27"),
                                         column(4, radioButtons("stack", "Select plot",
                                                                choices = list("stack" = "stack", "lines" = "lines"), selected = "stack")),
                                         column(8, radioButtons("Varline", "Select (VOC.VOI/Lineages)",
                                                                choices = list("VOC.VOI" = "VOC.VOI", "Lineages" = "Lineages"), selected = "VOC.VOI")),
                                         
                                  ),
                                  column(4, shinycssloaders::withSpinner(plotlyOutput("hist")))
                           )
                           
)

################################################################################################################################################################

################################################################################################################################################################

Analysis <- tabPanel(
  "Analysis",
  column(5, shinycssloaders::withSpinner(plotOutput("heatmap"), type = 3, color.background = "white", color = "green")),
  column(2,
         dateRangeInput("heatmapDate", "Select date range",
                        start  = "2021-06-01",
                        end    = "2022-01-27"),
         column(6,radioButtons("Kmeans", "kmeans clusters",
                               choices = list("k_means " = "k_means", "Location" = "Location"), selected = "Location")),
         column(6,radioButtons("transpose", "Transpose matrix",
                               choices = list("region_row" = "region_row", "lineages_row" = "lineages_row"), selected = "region_row")),
         column(5, numericInput("clusters", label = "Nº Clusters", min = 1, 
                                max = 8, value = 2 )),
         column(6, numericInput("mfrecuency", label = "Minimun frecuency", min = 1, 
                                max = 30, value = 1 )),
         selectInput(inputId = "method", 
                     label = "Distances method", 
                     choices = c("euclidean","maximum","manhattan","binary","minkowski"),
                     selected = ""),
         
         
         
  ),
  column(5, shinycssloaders::withSpinner(plotlyOutput("mutation"), type = 3, color.background = "white", color = "green")),
  
)


################################################################################################################################################################

##DataExp <- tabPanel("Data explorer",
##                    
##                    h4("A"),
##                    DT::dataTableOutput("tablemysql22")
##)

################################################################################################################################################################
