################################################################################
# Install missing packages
################################################################################

missingPackages <- function(pkg){
    if( !is.element(pkg,rownames(installed.packages() ) ) ){
      message(pkg, "-----> Package is not installed ")
      install.packages(pkg)
    }
}
################################################################################

dependencies <- c("shiny","shinycssloaders","pheatmap","plotly","fossil",
                  "epical","dplyr","rgdal","sp","sf","geojsonsf","DT",
                  "leaflet","leaflet.minicharts","viridisLite","viridis",
                  "RColorBrewer")

################################################################################
# Package R dependencies
################################################################################
for(i in dependencies){
  missingPackages(i)
  library(i, character.only = TRUE)
}
################################################################################

