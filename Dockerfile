# ---- Base: Shiny Server + R ----
FROM rocker/shiny:latest
ENV DEBIAN_FRONTEND=noninteractive

# ---- System dependencies (incl. pandoc, GDAL/GEOS/PROJ) ----
RUN apt-get update && apt-get install -y --no-install-recommends \
pandoc \
libcurl4-openssl-dev \
libssl-dev \
libxml2-dev \
libfontconfig1-dev \
libfreetype6-dev \
libharfbuzz-dev \
libfribidi-dev \
libpng-dev \
libjpeg-dev \
libtiff5-dev \
fonts-roboto \
gdal-bin \
libgdal-dev \
libgeos-dev \
libproj-dev \
git make g++ \
&& rm -rf /var/lib/apt/lists/*
  
  # ---- App code ----
WORKDIR /srv/shiny-server/app
COPY . /srv/shiny-server/app
RUN chown -R shiny:shiny /srv/shiny-server

# ---- R packages ----
RUN R -q -e "install.packages(c('pak','renv'), repos='https://cloud.r-project.org')"

# If renv.lock exists -> restore; else install pkgs used in app.R
RUN R -q -e " \
  if (file.exists('renv.lock')) { \
    pak::pkg_sysreqs('renv.lock'); \
    renv::restore(prompt = FALSE); \
  } else { \
    pkgs <- c( \
      'shiny','shinydashboard','shinydashboardPlus','shinyalert','waiter', \
      'tidyverse','tidyr','dplyr','broom','reshape2', \
      'hierfstat','adegenet','pegas','poppr','boot', \
      'hrbrthemes','leaflet','plotly','kableExtra','ggplot2', \
      'foreach','doParallel' \
    ); \
    pak::pkg_sysreqs(pkgs); \
    pak::pkg_install(pkgs); \
    pak::pkg_install('appsilon/shiny.react'); \
  }"

# ---- Shiny Server ----
EXPOSE 3838
ENV SHINY_LOG_LEVEL=INFO

HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=5 \
CMD wget -qO- http://localhost:3838 || exit 1

CMD ["/usr/bin/shiny-server"]
