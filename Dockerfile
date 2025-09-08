FROM rocker/shiny:latest

# System deps (additions: build-essential, cmake, pkg-config, ninja-build, coreutils)
RUN apt-get update && apt-get install -y --no-install-recommends \
    pandoc \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
    libfontconfig1-dev libfreetype6-dev libharfbuzz-dev libfribidi-dev \
    libpng-dev libjpeg-dev libtiff5-dev fonts-dejavu-core \
    gdal-bin libgdal-dev libgeos-dev libproj-dev \
    build-essential cmake pkg-config ninja-build coreutils \
 && apt-get purge -y libabsl-dev || true \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/app

# R helpers
RUN R -q -e "install.packages(c('pak','renv'), repos='https://cloud.r-project.org')"

# Ensure parallel make doesn't choke on empty $(nproc)
ENV MAKEFLAGS=-j1

# (Optional: keep normal pkg-config paths, but no system absl after purge)
# ENV PKG_CONFIG_PATH=/usr/lib/x86_64-linux-gnu/pkgconfig:/usr/local/lib/pkgconfig:/usr/lib/pkgconfig

# Copy only what renv needs first (cache-friendly)
COPY renv.lock renv/activate.R ./

# Restore exact R env
RUN R -q -e "renv::restore(prompt = FALSE)"

# Then copy the rest of the app
COPY . /srv/shiny-server/app
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
