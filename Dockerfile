# Install R version 3.6
FROM r-base:3.6.0

# Install Ubuntu packages
RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev/unstable \
    libxt-dev \
    libssl-dev

# Download and install ShinyServer (latest version)
RUN wget --no-verbose https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://s3.amazonaws.com/rstudio-shiny-server-os-build/ubuntu-12.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb

# Install R packages that are required
# TODO: add further package if you need!
RUN R -e "install.packages(c('shiny', 'shinydashboard', 'dplyr', 'lubridate', 'zoo', 'R0', 'shinycssloaders', 'remotes', 'ggplot2'), repos='http://cran.rstudio.com/')"

# install rstudion/httpuv to enable compatibility with google cloud run https://github.com/rstudio/shiny/issues/2455
RUN R -e "remotes::install_github(c('rstudio/httpuv'))"

# Copy configuration files into the Docker image
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf
COPY /COVID19Explorer /srv/shiny-server

# Make the ShinyApp available at port 8080
EXPOSE 8080

# Copy further configuration files into the Docker image
COPY shiny-server.sh /usr/bin/shiny-server.sh
RUN chmod a+rx /usr/bin/shiny-server.sh

CMD ["/usr/bin/shiny-server.sh"]