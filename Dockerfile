FROM rocker/r-base:4.3.2

# Installa i pacchetti R necessari
RUN R -e "install.packages(c('shiny','shinyjs','readxl','dplyr','stringr','ggplot2','tibble','tidyr','rmarkdown'), repos='https://cloud.r-project.org')"

# Copia l'app dentro il container
WORKDIR /app
COPY . /app

# Render imposta la porta in $PORT: usiamola per Shiny
ENV PORT=8080

CMD R -e "shiny::runApp('/app', host='0.0.0.0', port=as.numeric(Sys.getenv('PORT')))"
