# GenomicShiny-Cov

GenomicShiny-Cov is a shiny application for genomics surveillance. This application takes tsv files and geojson to plot interactive graphs to show the epidemiological and genomics status of the pandemic.


## System requirements

ubuntu 20.04 / 21.10 / 22.04  
debian 10  
Install dependencies  
  - libprotobuf-dev  
  - libjq-dev  
  - protobuf-compiler  
  - libudunits2-dev  
  - libgdal-dev  

## Installation and runing locally

```r 
git clone https://github.com/FranciscoAscue/GenomicShiny-Cov.git
runApp(launch.browser = TRUE) 
```
## Runing from github 

```r
install.packages("shiny")
shiny::runGitHub("GenomicShiny-Cov","FranciscoAscue", launch.browser = TRUE)
```
## Input data

### GISAID patient status .tsv   
The app can upload metadata of patient status downloaded from the international database GISAID

### Epidemiological data of positive cases and death reported .tsv/.csv   
The epidemiological of positive cases have two principal headers label: Date of report and Location 

### Vectorial map in GeoJson format   
The vectorial map can be upload from URL and directly in geojson format  

### Support or Contact

Having trouble with GenomicShiny-Cov? Check out our [documentation](https://docs.github.com/categories/github-pages-basics/) 
