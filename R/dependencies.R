################################################################################
## ubuntu 20.04 / 21.10 / 22.04
## debian 10
# Install dependencies 
# . libprotobuf-dev
# . libjq-dev
# . protobuf-compiler
# . libudunits2-dev
# . libgdal-dev

################################################################################

# Install missing packages

missingPackages <- function(pkg){
    if( !is.element(pkg,rownames(installed.packages() ) ) ){
      message(pkg, "-----> Package is not installed ")
      if(pkg == "htmlwidgets"){
        install.packages(pkg, version = "1.5.4")
      }else{
        install.packages(pkg)
      }
    }
}

dependencies <- c("shiny","raster","shinycssloaders","shinythemes","plotly","fossil", "remotes","dplyr",
                  "rgdal","sp","sf","lwgeom","geojsonsf","DT","htmlwidgets", "leaflet","leaflet.minicharts",
                  "viridisLite","viridis", "RColorBrewer","stringr","tidyverse","ggplot2","splitstackshape","geojsonio")

# Package R dependencies
if( !is.element("devtools",rownames(installed.packages() ) ) ){
  install.packages("devtools")
}

library(devtools)

if( !is.element("rlang",rownames(installed.packages() ) )  ){
  install_version("rlang", version = "1.0.6", repos = "http://cran.us.r-project.org")
}


for(i in dependencies){missingPackages(i)
  library(i, character.only = TRUE)
}

if( !is.element("epical",rownames(installed.packages() ) ) ){
  remotes::install_github("chrismerkord/epical")
}

sf::sf_use_s2(FALSE)

library(epical)
library(splines)
################################################################################

