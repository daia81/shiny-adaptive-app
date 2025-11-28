FROM rocker/shiny:latest

# Install R packages
RUN R -e "install.packages(c('shiny','shinyjs','readxl','dplyr','stringr','ggplot2','tibble','tidyr','rmarkdown'))"

# Copy app files
COPY . /srv/shiny-server/

# Expose port
EXPOSE 8080

# Run Shiny app
CMD ["/usr/bin/shiny-server"]
