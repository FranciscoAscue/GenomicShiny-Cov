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
  metadata <- add_epi_week(metadata, "Collection.date", system = "cdc")
  metadata$Date <- epi_week_date(metadata$epi_week,metadata$epi_year,system = "cdc")
  metadata$AA.Substitutions = str_replace_all(metadata$AA.Substitutions, pattern = c("\\(" = "", "\\)"= ""))
  
  return(metadata)
}

stackvariant <- function(data, mindate, maxdate, ngenomes, varline){
  
#  inter <- function(data){
#    data <- data %>% filter(date >= mindate, date <= maxdate)
#    data <- add_epi_week(data, "date", system = "cdc")
#    data$Date <-  epi_week_date(data$epi_week, data$epi_year, system = "cdc")
#    return(data)
#}
  
  if( varline == "Lineages"){
    #data <- inter(data)
    data_1 <- data %>% group_by(Date, epi_week,  lineage) %>% summarise( n = n()) %>%
      mutate(Frecuency = n / sum(n))
    
  }else{
    #data <- inter(data)
    data_1 <- data %>% group_by(Date,epi_week,  VOC.VOI) %>% summarise( n = n()) %>%
      mutate(Frecuency = n / sum(n))
  }
  
  data_1$Frecuency = round(data_1$Frecuency, 2)
  names(data_1) <- c("Date", "Epi.Week","Select","N", "Frecuency")
  data_1 <- data_1 %>% filter(N >= ngenomes)
  
  return(data_1)
}

mutations <- function(data,xmin= "2021-01-01", xmax="2022-02-25" ,freq = 50,variant="Omicron"){
  
  data <- data[data$VOC.VOI == variant,] 
  data <- data %>% filter(date >= xmin, date <= xmax)
  #data$week_date <- epi_week_date(data$epi_week,data$epi_year,system="cdc")
  data <- as.data.frame(data)
  #data$dec_date <- decimal_date(ymd(data$Date))
  

  #separate by commas#
  elements <- unlist(strsplit(data$Substitutions, ","))
  a <- sort(unique(elements))
  b <- grep("Spike", a, value = TRUE)
  
  #prepare the loop#
  
  h <- c() 
  j <- c()
  for (i in 1:length(b)){
    h <- append(h,data[grep(b[i],data$Substitutions),21])
    j <- append(j,rep(b[i],length(grep(b[i],data$Substitutions))))
  }
  
  k <- data.frame(h,j)
  l <- k%>%group_by(h,j)%>%summarise(n=n())
  #plot#
  l <- as.data.frame(l)
  names(l) <- c("week_date","mutation","freq")
  
  mut_freqs <- data.frame(table(k$j))
  mut_freqs$perc <- (mut_freqs$Freq/nrow(data))*100
  mut_selec <- mut_freqs[mut_freqs$perc >= freq,1]
  mut_freqs2 <- mut_freqs[order(-mut_freqs$perc),]
  
  m <- l[l$mutation %in% mut_selec,]
  return(m) 
}

freq_voc_voi <- function(data_1, lin){
  
  if(is.element(lin, unique(data_1$lineage))){
    
    # separamos por año para obtener una mejor vision
    data_1 <- data_1 %>% filter(lineage == lin)
    #data_1$date <- as.Date(as.character(data_1$date))
    #data_1 <- add_epi_week(data_1, "date", system = "cdc")
    #data_1$Date <-  epi_week_date(data_1$epi_week, data_1$epi_year, system = "cdc")
    data_1 <- data_1 %>% group_by(Date, epi_week) %>% summarise(Frecuency = n())
    return(data_1)
    
  } else{
    return(data.frame(Date = NULL, epi_week = NULL, Frecuency = NULL))
  }
  
}


split_lineages <- function(tabla,lineage1,gene,val){ # Main function
  
  #tabla$date1 <- epi_week_date(tabla$epi_week,tabla$epi_year,system = "cdc")
  
  selection <- filter(tabla, lineage == lineage1)
  selection <- selection[,c("location","Substitutions","Date")] #selected columns for down analysis
  
  ## Function 1 add new column order and filter some gene
  interest_mutation <- all_mutations(selection,gene) # search all mutations in spike protein. These function can be search any gen
  
  ## Function 2 selected uniq profile of mutations 
  profiles <- uniq(interest_mutation,val,gene)
  
  ## Function 3 selected all mutations and return a dataframe in order 
  tbl_resume <- do_table(profiles,gene)
  
  ## Function 4 search mutations and return the final table to show
  result_table <- tbl_total(tbl_resume,profiles,gene)
  
  # aditional steps to do dataframe to input heatmap
  newdata <- interest_mutation[ (interest_mutation$gen_select %in% profiles$gen_select), ]
  heatmap_mutations <- newdata %>% count(newdata$gen_select,newdata$Date) 
  names(heatmap_mutations) <- c("gene","epi_week","count") ## Here is possible return value for heatmap
  
  haplotype <- c()
  
  for (z in heatmap_mutations$gene){
    haplo <- profiles[profiles$gen_select==z,]
    haplotype <- append(haplotype,haplo$hap)
  }
  
  heatmap_mutations$Profiles <- haplotype
  heatmap_mutations$gene = str_replace_all(heatmap_mutations$gene,gene,"")
  #return(heatmap_mutations)
  return(list(table = result_table, heatmap = heatmap_mutations))
}

all_mutations <- function(mutations,gene) {  #Function 1
  gen_muts <- c()
  for (i in 1:nrow(mutations)){
    static <- c()
    nums <- c()
    list_mutations <-strsplit(mutations$Substitutions[i], ",")
    for (p in list_mutations[[1]]){
      if(grepl(gene,p) == TRUE){
        num <- stringr::str_extract(p, "\\d+")
        nums <- append(nums,as.numeric(num))
        static <- append(static,p)
      } 
    }
    my_data <- data.frame(static, nums)
    my_data <- my_data[order(my_data$nums),]
    collapse <- paste(my_data$static, collapse = ",")
    gen_muts <- append(gen_muts,collapse)
    static <- c()
    nums <- c()
  }
  mutations$gen_select <- gen_muts
  return(mutations)
}

uniq <- function(interest_mutation,val,gene){ #Function 2
  figure <- interest_mutation %>% group_by(gen_select) %>% summarize(count=n()) %>%  filter(count > val)
  figure$hap <- sprintf("%03d", 1:nrow(figure))
  return(figure)
}

do_table <- function(profiles,gene){
  
  new_vector <- c()
  
  profiles$gen_select = str_replace_all(profiles$gen_select,gene,"")
  c <- strsplit(as.character(profiles$gen_select,gene),',')
  for (i in 1:nrow(profiles)) {
    new_vector <- append(new_vector,c[[i]])
  }
  
  uniq_mut <- unique(new_vector)
  
  reference  <- c()
  position <- c()
  change <- c()
  
  for (y in uniq_mut){
    
    pos <- stringr::str_extract(y, "\\d+")
    mut <- strsplit(y, split = pos)
    
    reference <- append(reference,mut[[1]][1])
    position <- append(position,as.numeric(pos))
    change <- append(change,mut[[1]][2])
    
  }
  
  table_1 <- data.frame(reference, change, position)
  table_1 <- table_1[order(table_1$position),]
  return(table_1)
  
}

tbl_total <- function(tbl_resume,profiles,gene) {
  
  profiles$gen_select = str_replace_all(profiles$gen_select,gene,"")
  for (y in 1:nrow(profiles)){
    bools <- c()
    for (i in 1:nrow(tbl_resume)){
      
      val <- paste0(tbl_resume$reference[i],tbl_resume$position[i],tbl_resume$change[i])
      bool <- str_detect(profiles$gen_select[y],val)
      bools <-append(bools,bool)
    }
    bools[isTRUE(bools)] <- 1
    tbl_resume[paste0(profiles$hap[y])] <- bools
  }
  return(tbl_resume)
}




