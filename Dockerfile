FROM openanalytics/r-base

MAINTAINER Rafael Pereira "r.s.p.models@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/',dependencies=TRUE)"

# install dependencies of the Analysis app
RUN R -e "install.packages(c('pracma','plotly'), repos='https://cloud.r-project.org/',dependencies=TRUE)"


# copy the app to the image
copy app.R app.R
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('app.R',port=3838,host='0.0.0.0',launch.browser=FALSE)"]

