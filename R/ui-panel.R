#############################################################################################################################################
# Ui panel


UploadData <- tabPanel("Upload Data",
                       column(3,
                              
                         column(12, style = "background-color:#EAEDED;",    
                              h3(p(style="color:black;text-align:left", 
                                   tags$img(src="https://upload.wikimedia.org/wikipedia/commons/3/34/GISAID_logo.png",
                                            width="150px",height="60px"),
                                   tags$img(src="https://1000marcas.net/wp-content/uploads/2020/02/GitHub-logo-1.jpg",
                                            width="100px",height="60px")
                              ))),
                         column(12, style = "background-color:#E5E7E9;",  
                              textInput(inputId = "geojsonurl", label = h4("Url to Geojson (Optional)"),value = NULL),
                              
                              fileInput(inputId = "geojson", h4("Upload Geojson File"), accept=c('.geojson','.geocsv')),
                              
                              h3(" "),
                              downloadButton("downloadjson", "Download", label = "Download GeoJson"),
                              h3(" "),
                              
                       ),
                       
                              h3(" "),
                       
                       column(12 ,  style = "background-color:#D7DBDD;", 
                              column(3,radioButtons("selectInput", "Select input",
                                                    choices = list("Gisaid Pacient status" = "GISAID", "Gisaid augur input" = "augur"), 
                                                    selected = "GISAID")),
                              
                              column(3,radioButtons("separator", "Select separator",
                                                    choices = list("Tab" = "\t", "Comma" = ","), selected = "\t")),
                              h3(" "),
                                column(12, 
                              uiOutput("selectLocation"),
                              uiOutput("selectCountry"),
                              ),
                              fileInput(inputId = "metadata", h4("Upload metadata file (.csv/.tsv)"), 
                                        accept=c('text/csv','text/comma-separated-values,text/plain', '.csv','.tsv')),
                       
                       ),
                      column( 12 ,  style = "background-color:#E5E7E9;",       
                              selectInput(inputId = "DateFormat", 
                                          label = "Select a date format", 
                                          choices = c("%Y%m%d","%Y-%m-%d","%Y/%m/%d","%d-%m-%Y","%d/%m/%Y"),
                                          selected = "South America /"),
                              
                              column(8,radioButtons("Episeparator", "Select epidemiological file separator",
                                                    choices = list("Tab" = "\t", "Comma" = ",", "semicolon" = ";"), 
                                                    selected = "\t", inline = TRUE)),
                              
                              column(8,radioButtons("EpidemInput", "Select epidemiological file",
                                                    choices = list("Cumulative cases per Day" = "CC", "Cases per Day" = "CD"), 
                                                    selected = "CD", inline = TRUE)),
                              
                              fileInput(inputId = "emetadata", h4("Upload Epidemiological CSV File"), 
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.tsv')),
                      ),
                              actionButton(inputId = "RunTest", label = h4(icon(name = "file-import"), "Run test"), width = "200px"),
                              

                       ),
                       
                       column(8, aling="center",
                              shinycssloaders::withSpinner( DT::dataTableOutput("tabla"), type = 7, color.background = "white")
                       ),

                       tags$style(".fa-check {color:#1AC20B}"),
                       tags$style(".fa-grip-lines {color:#E87722}"),
                       tags$style(".fa-exclamation {color:#F31507}"),
)

MapStatistics <- tabPanel( 
  "Descriptive epidemiology",
  
  column(12, 
         column(6, 
                shinycssloaders::withSpinner( leafletOutput(outputId = "leaflet_map", height = 550))),
         column(6,
                column(4,
                       
                column(12, uiOutput("rangedate1")     
                ),
                column(12,uiOutput("selectVariants")),
                column(12,radioButtons("Escala", "Select scale",
                                    choices = list("linear" = "linear", 
                                                   "logarithmic" = "log"), 
                                    selected = "log", 
                                    inline = TRUE)),
                column(12,radioButtons("switch", "Switch Lineages / VOC-VOI",
                                    choices = list("lineage" = "lineage",
                                                   "VocVoi" = "VocVoi"),
                                    selected = "VocVoi",
                                    inline = TRUE))),
                
                column(8,  shinycssloaders::withSpinner(leafletOutput("map", height = 550)))
         )
  ),
  
  column(12, h3(" ")),
  column(12, 
         column(6, shinycssloaders::withSpinner(plotlyOutput("lineplot"))),
         column(2,  uiOutput("histLineageinput"),
                
                numericInput("ngenomes", label = "Minimun genomes", min = 1, 
                             max = 30, value = 1 ),
                uiOutput("rangedate2"),
                column(4, 
                       radioButtons("stack", "Select plot",
                                    choices = list("stack" = "stack", "lines" = "lines"),
                                    selected = "stack")),
                column(8, 
                       radioButtons("Varline", "Select (VOC.VOI/Lineages)",
                                    choices = list("VOC.VOI" = "VOC.VOI","Lineages" = "Lineages"), 
                                    selected = "VOC.VOI")),
         ),
         column(4, shinycssloaders::withSpinner(plotlyOutput("hist")))
  )
  
)

AnalysisLineages <- tabPanel(
  "Mutation surveillance",
  
  column(12, 
  column(5, shinycssloaders::withSpinner(plotlyOutput("heatmap"), type = 3,  color.background = "white", color = "green")),
  column(2, uiOutput("rangedate3"),
         
         column(12,radioButtons("transpose", "Transpose matrix",
                               choices = list("region_row" = "region_row", 
                                              "lineages_row" = "lineages_row"), 
                               selected = "region_row", inline = TRUE)),
         
         column(12, numericInput("mfrecuency", label = "Minimun frecuency", min = 1, 
                                max = 30, value = 1 ))
  ),
  column(5, shinycssloaders::withSpinner(plotlyOutput("mutation"), 
                                         type = 3, color.background = "white", color = "green"))
  ),
  
  column(12,
  column(5, shinycssloaders::withSpinner(plotlyOutput("perfil_mutations"), 
                                         type = 3, color.background = "white", color = "green")),
  column(2,
         column(12, h2(" ")),
         
         numericInput("pfrecuency", label = "Minimun frecuency", 
                      min = 1, max = 30, value = 1 ),
         
         uiOutput("selectLineages"),
         selectInput(inputId = "Gene", 
                     label = "Select Gene", 
                     choices = c("Spike_","N_","NSP1_","NSP2_","NSP3_","NSP4_","NSP5_","NSP6_","NSP7_","NSP8_","NSP9_",
                                 "NSP10_","NSP12_","NSP13_","NSP14_","NSP15_","NSP16_","NS3_","NS3a_","NS3b_","E_","M_","NS6_",
                                 "NS7a_","NS7b_","NS8_","NS9a_","NS9b_","NS10_"),
                     selected = "")
  ),
    column(5, shinycssloaders::withSpinner(DT::dataTableOutput("mutation_tabla"), 
                                           type = 3, color.background = "white", color = "green"))
  )
)

