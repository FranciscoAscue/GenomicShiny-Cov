################################################################################
# Install missing packages
################################################################################

missingPackages <- function(pkg){
    if( !is.element(pkg,rownames(installed.packages() ) ) ){
      message(pkg, "-----> Package is not installed ")
      if(pkg == "epical"){
        remotes::install_github("chrismerkord/epical")
      }
      install.packages(pkg)
    }
}
################################################################################

dependencies <- c("shiny","shinycssloaders","pheatmap","plotly","fossil",
                  "remotes","dplyr","rgdal","sp","sf","geojsonsf","DT",
                  "leaflet","leaflet.minicharts","viridisLite","viridis",
                  "RColorBrewer","epical")

################################################################################
# Package R dependencies
################################################################################
for(i in dependencies){
  missingPackages(i)
  library(i, character.only = TRUE)
}
################################################################################

