################################################################################
# Preprocesing data
################################################################################
LocationCountry <- read.csv("Data/Location-country.txt", header = TRUE, sep = ";")
col <- read.csv("Data/colnames.csv", header = TRUE)

Added_VocVoi <- function(metadata){
  voc_voi <- metadata$Lineage
  lineage2var <- read.csv("Data/VOC.VOI.csv")
  
  for(i in 1:length(lineage2var$Lineages)){
    voc_voi <- gsub(lineage2var$Lineages[i], lineage2var$VOC.VOI[i], voc_voi)
  }
  metadata$VOC.VOI <- voc_voi
  return(metadata)
}

metadata_preprosesing  <- function(metadata, region, country, geoLocation){
  
  metadata$Location <- gsub(region,"", metadata$Location, fixed = T) 
  metadata$Location <- gsub(country,"", metadata$Location, fixed = T)
  metadata$Location <- gsub("\\/.*","", metadata$Location)
  metadata$Location <- trimws(metadata$Location, which = "both", whitespace = "[ \t\r\n]")
  metadata$Location <- toupper(metadata$Location)
  
  Locations <- unique(metadata$Location)
  
  if(length(Locations) != length(geoLocation$Location)){
    warning("Different numbers of locations!!")
    dff <- setdiff(unique(metadata$Location), geoLocation$Location)
    for( i in dff){
      metadata <- metadata %>% filter(Location != i)
    }
  }else{
    message("Pass")
  }
  metadata <- Added_VocVoi(metadata)
  metadata <- metadata %>% filter(nchar(as.character(Collection.date)) == 10)
  return(metadata)
}

stackvariant <- function(data, mindate, maxdate, ngenomes, varline){
  
  inter <- function(data){
    data <- data %>% filter(date >= mindate, date <= maxdate)
    data <- add_epi_week(data, "date", system = "cdc")
    data$Date <-  epi_week_date(data$epi_week, data$epi_year, system = "cdc")
    return(data)
  }

  if( varline == "Lineages"){
      data <- inter(data)
      data_1 <- data %>% group_by(Date, epi_week,  lineage) %>% summarise( n = n()) %>%
        mutate(Frecuency = n / sum(n))
      
  }else{
    data <- inter(data)
    data_1 <- data %>% group_by(Date,epi_week,  VOC.VOI) %>% summarise( n = n()) %>%
      mutate(Frecuency = n / sum(n))
  }
  
  data_1$Frecuency = round(data_1$Frecuency, 2)
  names(data_1) <- c("Date", "Epi.Week","Select","N", "Frecuency")
  data_1 <- data_1 %>% filter(N >= ngenomes)
  
  return(data_1)
}


freq_voc_voi <- function(data_1, lin){
  
  if(is.element(lin, unique(data_1$lineage))){
    
    # separamos por año para obtener una mejor vision
    data_1 <- data_1 %>% filter(lineage == lin)
    #data_1$date <- as.Date(as.character(data_1$date))
    data_1 <- add_epi_week(data_1, "date", system = "cdc")
    data_1$Date <-  epi_week_date(data_1$epi_week, data_1$epi_year, system = "cdc")
    data_1 <- data_1 %>% group_by(Date, epi_week) %>% summarise(Frecuency = n())
    return(data_1)
    
  } else{
    return(data.frame(Date = c(""), epi_week = c(""), Frecuency = c("")))
  }
  
}

