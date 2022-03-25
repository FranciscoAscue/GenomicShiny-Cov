##if( !is.element("RMySQL",rownames(installed.packages() ) ) ){
##  install.packages("RMySQL")
##}

#library(RMySQL)

## data from MySQL
metadata_sql <- function(db, table, host = 'localhost', usr = 'user', pswd = 'password', mindate = NULL, maxdate = NULL){
  
  con <- dbConnect(MySQL(),
                   user = usr,
                   password = pswd,
                   host = host,
                   dbname = db)
  
  if( !is.null(mindate) & !is.null(maxdate)){
  query = paste0("SELECT * FROM `",table,"` WHERE `Date` BETWEEN '",mindate,"' AND '",maxdate,"';")
  }else{
    query = paste0("SELECT * FROM `",table,"`;")
  }
  rs = dbSendQuery(con, query);
  df = fetch(rs, -1);
  dbDisconnect(con)
  return(df)
}

