metadata_test <- function(metadata, geojson, epidemio = NA){
  test_table <- read.csv("tests/metadata-test.csv", header = TRUE)
  cnt = 1
  for(i in c("location","lineage","date","VOC.VOI","Substitutions", "Population") ){
    
    if( i == "Population"){
      if(is.element(i,colnames(geojson)) ){
        test_table$RESULT[cnt] <- TRUE
        test_table$STATUS[cnt] <- as.character(icon("check","fa-solid"))
      }else{
        test_table$RESULT[cnt] <- FALSE
        test_table$STATUS[cnt] <- as.character(icon("exclamation","fa-solid"))
      }
    }else{
      if(is.element(i, colnames(metadata)) ){
        test_table$RESULT[cnt] <- TRUE
        test_table$STATUS[cnt] <- as.character(icon("check","fa-solid"))
      }else{
        test_table$RESULT[cnt] <- FALSE
        test_table$STATUS[cnt] <- as.character(icon("exclamation","fa-solid"))
      }
    }
    cnt = cnt + 1
  }
  
  if(length(setdiff(toupper(unique(metadata$location)), toupper(geojson$Location))) >= 1){
    test_table$RESULT[7] <- TRUE
    test_table$STATUS[7] <- as.character(icon("exclamation","fa-solid"))
  }else{
    test_table$RESULT[7] <- FALSE
    test_table$STATUS[7] <- as.character(icon("check","fa-solid"))
  }
  
  if(length(setdiff(toupper(unique(metadata$location)), toupper(unique(epidemio$Location)))) >= 1){
    test_table$RESULT[8] <- TRUE
    test_table$STATUS[8] <- as.character(icon("exclamation","fa-solid"))
  }else{
    test_table$RESULT[8] <- FALSE
    test_table$STATUS[8] <- as.character(icon("check","fa-solid"))
  }
  
  if(length(geojson$Location) == length(unique(metadata$location))){
    test_table$RESULT[9] <- TRUE
    test_table$STATUS[9] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[9] <- FALSE
    test_table$STATUS[9] <- as.character(icon("exclamation","fa-solid"))
  }
  
  if(length(unique(epidemio$Location)) == length(unique(metadata$location))){
    test_table$RESULT[10] <- TRUE
    test_table$STATUS[10] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[10] <- FALSE
    test_table$STATUS[10] <- as.character(icon("exclamation","fa-solid"))
  }
  
  if((max(epidemio$Date) > max(metadata$date)) 
     & (min(epidemio$Date) < min(metadata$date)) ){
    test_table$RESULT[11] <- TRUE
    test_table$STATUS[11] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[11] <- FALSE
    test_table$STATUS[11] <- as.character(icon("exclamation","fa-solid"))
  }
  
  return(test_table)
}
