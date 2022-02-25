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
  geoLocation$Location <- toupper(geoLocation$Location)
  
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
    return(data.frame(Date = NULL, epi_week = NULL, Frecuency = NULL))
  }
  
}



#read data#
mutations <- function(data,xmin= "2021-01-01", xmax="2022-02-25" ,freq = 50,variant="Omicron"){
  
  data <- data[data$VOC.VOI == variant,] 
  data <- data %>% filter(date >= xmin, date <= xmax)
  data <- add_epi_week(data, "date", system="cdc")
  data$week_date <- epi_week_date(data$epi_week,data$epi_year,system="cdc")
  data <- as.data.frame(data)
  data$dec_date <- decimal_date(ymd(data$week_date))
  
  #replace "(" and ")"#
  data$Substitutions <- gsub("(","",data$Substitutions , fixed = T )
  data$Substitutions <- gsub(")","",data$Substitutions , fixed = T )
  
  #separate by commas#
  elements <- unlist(strsplit(data$Substitutions, ","))
  a <- sort(unique(elements))
  b <- grep("Spike", a, value = TRUE)
  
  #prepare the loop#
  
  c <- 0
  d <- 0 
  e <- 0 
  f <- 0 
  g <- 0 
  h <- 0 
  j <- 0
  for (i in 1:length(b)){
    c <- append(c,data[grep(b[i],data$Substitutions),1])
    d <- append(d,data[grep(b[i],data$Substitutions),2])
    e <- append(e,data[grep(b[i],data$Substitutions),3])
    f <- append(f,data[grep(b[i],data$Substitutions),4])
    g <- append(g,data[grep(b[i],data$Substitutions),15])
    h <- append(h,data[grep(b[i],data$Substitutions),22])
    j <- append(j,rep(b[i],length(grep(b[i],data$Substitutions))))
  }
  
  k <- data.frame(c,d,e,f,g,h,j)
  
  k <- k[-1,]
  l <- k%>%group_by(h,j)%>%summarise(n=n())
  #plot#
  l <- as.data.frame(l)
  l$date <- date_decimal(l$h)
  
  names(l) <- c("decimal_date","mutation","freq","week_date")
  
  mut_freqs <- data.frame(table(k$j))
  mut_freqs$perc <- (mut_freqs$Freq/nrow(data))*100
  mut_selec <- mut_freqs[mut_freqs$perc >= freq,1]
  mut_freqs2 <- mut_freqs[order(-mut_freqs$perc),]
  
  m <- l[l$mutation %in% mut_selec,]
  return(m)
}

