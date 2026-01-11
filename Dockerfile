FROM rocker/r-ver:4.3.2

# System libs (robust for many R packages)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Install R packages
COPY install.R /app/install.R
RUN Rscript /app/install.R

# Copy app
COPY . /app

ENV PORT=8080
EXPOSE 8080

CMD ["R", "-e", "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT','8080')))"]
