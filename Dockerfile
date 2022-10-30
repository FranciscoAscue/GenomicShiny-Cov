FROM rocker/shiny:latest

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
	libprotobuf-dev \
	libjq-dev \
	protobuf-compiler \
	libudunits2-dev \
	libgdal-dev \
	build-essential

RUN apt-get update && \
	apt-get upgrade -y && \
	apt-get clean

COPY . ./app

RUN Rscript /app/R/dependencies.R

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]

