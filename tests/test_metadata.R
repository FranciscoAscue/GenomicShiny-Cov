metadata_test <- function(metadata, geojson, epidemio){
  test_table <- read.csv("tests/metadata-test.csv", header = TRUE)
  if(!is.null(metadata$location)){
    test_table$RESULT[1] <- TRUE
    test_table$STATUS[1] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[1] <- FALSE
    test_table$STATUS[1] <- as.character(icon("exclamation","fa-solid"))
  }
  
  if(!is.null(metadata$lineage)){
    test_table$RESULT[2] <- TRUE
    test_table$STATUS[2] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[2] <- FALSE
    test_table$STATUS[2] <- as.character(icon("exclamation","fa-solid"))
    
  }
  
  if(!is.null(metadata$date) | !is.null(metadata$Date)){
    test_table$RESULT[3] <- TRUE
    test_table$STATUS[3] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[3] <- FALSE
    test_table$STATUS[3] <- as.character(icon("exclamation","fa-solid"))
    
  }
  
  if(!is.null(metadata$VOC.VOI)){
    test_table$RESULT[4] <- TRUE
    test_table$STATUS[4] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[4] <- FALSE
    test_table$STATUS[4] <- as.character(icon("grip-lines","fa-solid"))
  }
  
  
  if(!is.null(metadata$Substitutions)){
    test_table$RESULT[5] <- TRUE
    test_table$STATUS[5] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[5] <- FALSE
    test_table$STATUS[5] <- as.character(icon("grip-lines","fa-solid"))
  }
  
  
  if(!is.null(geojson$Population) ){
    test_table$RESULT[6] <- TRUE
    test_table$STATUS[6] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[6] <- FALSE
    test_table$STATUS[6] <- as.character(icon("exclamation","fa-solid"))
    
  }
  
  
  if(length(setdiff(toupper(metadata$location), toupper(geojson$Location))) >= 1){
    test_table$RESULT[7] <- TRUE
    test_table$STATUS[7] <- as.character(setdiff(toupper(metadata$location), toupper(geojson$Location)))
  }else{
    test_table$RESULT[7] <- FALSE
    test_table$STATUS[7] <- as.character(icon("check","fa-solid"))
  }
  
  if(length(setdiff(toupper(metadata$location), toupper(epidemio$Location))) >= 1){
    test_table$RESULT[8] <- TRUE
    test_table$STATUS[8] <- as.character(setdiff(toupper(metadata$location), toupper(epidemio$Location)))
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
  
  
  if(length(geojson$Location) == length(unique(metadata$location))){
    test_table$RESULT[11] <- TRUE
    test_table$STATUS[11] <- as.character(icon("check","fa-solid"))
  }else{
    test_table$RESULT[11] <- FALSE
    test_table$STATUS[11] <- as.character(icon("exclamation","fa-solid"))
  }
  
  return(test_table)
}
