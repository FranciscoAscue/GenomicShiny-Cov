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

COPY renv.lock ./renv.lock
COPY . ./app

RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp['/app', host = '0.0.0.0', port = 3838]"]

