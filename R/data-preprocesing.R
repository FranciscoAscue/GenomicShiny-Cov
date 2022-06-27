#### Preprocesing data

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
  
  metadata$Date <- epi_week_date(metadata$epi_week, metadata$epi_year,system = "cdc")
  metadata$AA.Substitutions = str_replace_all(metadata$AA.Substitutions, 
                                              pattern = c("\\(" = "", "\\)"= ""))
  return(metadata)
}

stackvariant <- function(data, mindate, maxdate, ngenomes, varline){
  
  data <- data %>% dplyr::filter(date >= mindate, date <= maxdate)
  
  if( varline == "Lineages"){
    
    data <- data %>% group_by(Date, epi_week,  lineage) %>% summarise( n = n()) %>%
      mutate(Frecuency = n / sum(n))
    
  }else{
    data <- data %>% group_by(Date,epi_week,  VOC.VOI) %>% summarise( n = n()) %>%
      mutate(Frecuency = n / sum(n))
  }
  
  data$Frecuency = round(data$Frecuency, 2)
  names(data) <- c("Date", "Epi.Week","Select","N", "Frecuency")
  data <- data %>% filter(N >= ngenomes)
  
  return(data)
}

freq_voc_voi <- function(data, lin){
  
  if(is.element(lin, unique(data$lineage))){
    
    data <- data %>% filter(lineage == lin)
    data <- data %>% group_by(Date, epi_week) %>% summarise(Frecuency = n())
    return(data)
  } else{
    return(data.frame(Date = NULL,  epi_week = NULL,  Frecuency = NULL))
  }
  
}

matrix_distribution <- function(metadata, mindate, maxdate, frecuency, transp){
  
  data <- as.data.frame(metadata)
  data$date <- as.Date(data$date)
  data <- data %>% filter(date >= mindate, date <= maxdate)
  
  counts <- data %>% group_by(location, lineage) %>% summarise(n = n())
  names(counts) <- c("location", "lineage", "Freq")
  counts <- counts %>% filter(Freq >= frecuency)
  counts <- as.data.frame(counts)
  
  cuadro_motivo <- create.matrix(counts, tax.name = "location", locality = "lineage",
                                 abund.col = "Freq", abund = TRUE)
  
  if( transp == "lineages_row" ){
    return(t(as.data.frame(cuadro_motivo)))
  }
  return(cuadro_motivo)
}

variant_distribution <- function(map, metadata, epidem,  mindate, maxdate, switch = "VocVoi", input = "Cases per Day"){
  
  cities <- as.data.frame(st_coordinates(st_centroid(map)))
  map$Location <- toupper(map$Location)
  cities$location <- map$Location
  metadata <- metadata %>% filter(date >= mindate , date <= maxdate)
  metadata$location <- toupper(metadata$location)
  
  if( switch == "VocVoi" ){
    for( var in unique(metadata$VOC.VOI)){
      temp <- metadata %>% filter(VOC.VOI == var) %>% group_by(location) %>% summarise( !!paste0(var) := n())
      cities <- merge(x  = cities, y = temp, by = 'location', all = TRUE  )
    }
  }else{
    for( var in unique(metadata$lineage)){
      temp <- metadata %>% filter(lineage == var) %>% group_by(location) %>% summarise( !!paste0(var) := n())
      cities <- merge(x  = cities, y = temp, by = 'location', all = TRUE  )
    }
  }
  
  total <- metadata %>% group_by(location) %>% summarise(total = n())
  cities <- merge(x  = cities, y = total, by = 'location', all = TRUE )
  cities[is.na(cities)] <- 0
  for(i in 1:length(cities$total)){if(cities$total[i] == 0){cities$total[i] = 1}}
  
  if(input == "CD"){
    epidem_freq <- epidem %>% filter(date >= mindate, date <= maxdate)
    epidem_freq <- epidem_freq %>% group_by(Location) %>% summarise( N = n())
    epidem_freq$Location <- toupper(epidem_freq$Location)
    Merge_data <- inner_join(map,epidem_freq, by = 'Location' )
    Merge_data$N <- (Merge_data$N/Merge_data$Population)*100000
  }else{
    epidem_maxdate <- data %>% filter(date == maxdate)
    epidem_mindate <- data %>% filter(date == mindate)
    merged_epidem <- merge(filtrado, filtrado2, by = "location", all = TRUE)
    merged_epidem[is.na(merged_epidem)] <- 0
    merged_epidem$N <- merged_epidem$deaths.x - merged_epidem$deaths.y
    merged_epidem$Location <- toupper(merged_epidem$state)
    Merge_data <- inner_join(map,merged_epidem, by = 'location' )
    Merge_data$N <- (Merge_data$N/Merge_data$Population)*100000
  }
  
  pal <- colorNumeric(  palette = "Greys", NULL)
  long <- cities$X
  lat <- cities$Y
  var <- cities[,4:(length(cities)-1)]
  total <- cities$total
  return( list( df = Merge_data, pal = pal, long = long, lat = lat, var = var, total = total))
}

sampling_distribution <- function(map , metadata, mindate, maxdate, sampling, scale_map){
  
  metadata <- metadata %>% filter( date >= mindate, date <= maxdate )
  if(sampling == "Total"){
    metadata <- metadata
    pal <- colorNumeric(  palette = "Reds", NULL)
  } else { 
    metadata <- metadata %>% filter(VOC.VOI == sampling )
    pal <- colorNumeric(  palette = "BuPu", NULL)
  }
  
  if(scale_map == "linear"){
    count_region <- metadata %>% group_by(location) %>% summarise( n = n())
  } else{
    count_region <- metadata %>% group_by(location) %>% summarise( n = log10(n()))
  }
  
  count_region$location <- toupper(count_region$location)
  colnames(count_region) <- c("Location", "N")
  Merge_data <- merge(map, count_region , by  = "Location")
  return(list(df = Merge_data, pal = pal))
}

mutations <- function(data, gene, freq = 50,lineage="BA.1"){
  
  data <- data[data$lineage == lineage,] 
  data <- data 
  data <- as.data.frame(data)
  
  elements <- unlist(strsplit(data$Substitutions, ","))
  a <- sort(unique(elements))
  b <- grep(gene, a, value = TRUE)
  
  h <- c() 
  j <- c()
  for (i in 1:length(b)){
    h <- append(h,data[grep(b[i],data$Substitutions),21])
    j <- append(j,rep(b[i],length(grep(b[i],data$Substitutions))))
  }
  
  k <- data.frame(h,j)
  l <- k%>%group_by(h,j)%>%summarise(n=n())
  l <- as.data.frame(l)
  names(l) <- c("week_date","mutation","freq")
  
  mut_freqs <- data.frame(table(k$j))
  mut_freqs$perc <- (mut_freqs$Freq/nrow(data))*100
  mut_selec <- mut_freqs[mut_freqs$perc >= freq,1]
  mut_freqs2 <- mut_freqs[order(-mut_freqs$perc),]
  
  m <- l[l$mutation %in% mut_selec,]
  return(m) 
}

split_lineages <- function(tabla,lineage1,gene,val, ncores = 1){
  
  selection <- filter(tabla, lineage == lineage1)
  selection <- selection[,c("location","Substitutions","Date")]
  
  add_col <- selection$Substitutions
  add_col_new <- mclapply(add_col, all_mutations, gene, mc.cores = 1) # search all mutations in spike protein. These function can be search any gen
  new_col <- unlist(add_col_new)
  selection$gen_select <- new_col
  interest_mutation <- selection
  profiles <- uniq(interest_mutation,val,gene)

  if (sum(is.na(profiles)) == 0 & dim(profiles)[1] >= 1){
    tbl_resume <- do_table(profiles,gene)
    result_table <- tbl_total(tbl_resume,profiles,gene)
    newdata <- interest_mutation[ (interest_mutation$gen_select %in% profiles$gen_select), ]
    heatmap_mutations <- newdata %>% count(newdata$gen_select,newdata$Date) 
  
    names(heatmap_mutations) <- c("gene","epi_week","count") 
    haplotype <- c()
  
    for (z in heatmap_mutations$gene){
      haplo <- profiles[profiles$gen_select==z,]
     haplotype <- append(haplotype,haplo$hap)
     }
  
    heatmap_mutations$Profiles <- haplotype
  heatmap_mutations$gene = str_replace_all(heatmap_mutations$gene,gene,"")
  
  return(list(mutations = tbl_resume, table = result_table, heatmap = heatmap_mutations))
  
  }else{
    return(list(mutations = "NaN", table = "NaN", heatmap = "NaN"))
  }
}

all_mutations <- function(elements,gene) {  #Function 1
  static <- c()
  nums <- c()
  
  list_mutations <-strsplit(elements, ",")
  for (p in list_mutations[[1]]){
    if(grepl(gene,p) == TRUE){
      p2 = str_replace_all(p,gene,"")
      num <- stringr::str_extract(p2, "\\d+")
      nums <- append(nums,as.numeric(num))
      static <- append(static,p)
    }
  }
  
  my_data <- data.frame(static, nums)
  
  if (length(my_data) > 0) {
    my_data <- my_data[order(my_data$nums),]
    collapse <- paste(my_data$static, collapse = ",")
    
    static <- c()
    nums <- c()
    
    return(collapse)
    
  } 
  else {
    
    static <- c()
    nums <- c()
    
    return(NA)
  } 
  
}

uniq <- function(interest_mutation,val,gene){
  interest_mutation <- interest_mutation %>% drop_na()
  figure <- interest_mutation %>% group_by(gen_select) %>% summarize(count=n()) %>%  filter(count > val)
  
  if (dim(figure)[1] == 0) { 
    
    return(figure)
    
  } else {
    
    figure$hap <- sprintf("%03d", 1:nrow(figure))
    figure[complete.cases(figure),]
    return(figure)
  }
  
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
  reference <- gsub("ins","-", reference, fixed = TRUE)
  table_1 <- data.frame(reference, change, position)
  table_1 <- table_1[order(table_1$position),]
  na.omit(table_1)
  
  return(table_1)
  
}

tbl_total <- function(tbl_resume,profiles,gene) {
  
  profiles$gen_select = str_replace_all(profiles$gen_select,gene,"")
  
  for (y in 1:nrow(profiles)){
    bools <- c()
    for (i in 1:nrow(tbl_resume)){
      val <- paste0(tbl_resume$reference[i],tbl_resume$position[i],tbl_resume$change[i])
      bool <- str_detect(profiles$gen_select[y],val)
      
      if(bool == TRUE){
        bools <-append(bools,tbl_resume$change[i])
      }else{
        bools <-append(bools,NA)
      }
    }
    tbl_resume[paste0(profiles$hap[y])] <- bools
  }
  return(tbl_resume)
}
