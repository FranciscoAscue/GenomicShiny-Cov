#############################################################################################################################################
# Ui panel


UploadData <- tabPanel("Upload Data",
                       column(3, 
                              h3(p(style="color:black;text-align:left", 
                                   tags$img(src="https://upload.wikimedia.org/wikipedia/commons/3/34/GISAID_logo.png",
                                            width="120px",height="60px"),
                                   tags$img(src="https://cov-lineages.org/assets/images/pangolin.png",
                                            width="60px",height="60px"),
                                   tags$img(src="https://nextstrain.org/static/nextstrain-logo-small-ea8c3e13e8c17436264760d638ab970e.png",
                                            width="60px",height="60px")
                              )),
                              textInput(inputId = "geojsonurl", label = h4("Url to Geojson (Optional)"),value = NULL),
                              
                              fileInput(inputId = "geojson", h4("Upload Geojson File"), accept=c('.geojson','.geocsv')),
                              
                              column(3,radioButtons("selectInput", "Select input",
                                                    choices = list("GISAID" = "GISAID", "Custom Metadata" = "Custom"), 
                                                    selected = "GISAID")),
                              
                              column(3,radioButtons("separator", "Select separator",
                                                    choices = list("Tab" = "\t", "Comma" = ","), selected = "\t")),
                              
                              column(12, h3("")),
                              uiOutput("selectLocation"),
                              uiOutput("selectCountry"),
                              
                              fileInput(inputId = "metadata", h4("Upload metadata file (.csv/.tsv)"), 
                                        accept=c('text/csv','text/comma-separated-values,text/plain', '.csv','.tsv')),
                              
                              selectInput(inputId = "DateFormat", 
                                          label = "Select a date format", 
                                          choices = c("%Y%m%d","%Y-%m-%d","%Y/%m/%d","%d-%m-%Y","%d/%m/%Y"),
                                          selected = "South America /"),
                              
                              column(8,radioButtons("Episeparator", "Select epidemiological file separator",
                                                    choices = list("Tab" = "\t", "Comma" = ",", "semicolon" = ";"), 
                                                    selected = "\t", inline = TRUE)),
                              
                              fileInput(inputId = "emetadata", h4("Upload Epidemiological CSV File"), 
                                        accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv','.tsv')),
                              
                              actionButton(inputId = "RunTest", label = h4(icon(name = "file-import"), "Run test"), width = "200px")
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
                column(12, dateRangeInput("Daterange", 
                                         "Select date range", 
                                         start  = "2020-03-01",
                                         end    = "2022-01-25")      
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
         column(2, textInput(inputId = "lineage",
                          label = "Write a linage",
                          value = "AY.117"),
                
                numericInput("ngenomes", label = "Minimun genomes", min = 1, 
                             max = 30, value = 1 ),
                
                dateRangeInput("lineageDate", "Select date range",
                               start  = "2021-06-01",
                               end    = "2022-01-27"),
                
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
  column(2, dateRangeInput("heatmapDate", "Select date range",
                        start  = "2021-06-01",
                        end    = "2022-01-27"),
         
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

