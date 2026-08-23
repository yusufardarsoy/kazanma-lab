FROM rocker/shiny:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev libssl-dev libsodium-dev libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

RUN install2.r --error --skipinstalled \
    bslib DBI dplyr ggplot2 glue httr2 jsonlite purrr RSQLite scales \
    shiny shinyjs sodium tidyr testthat

COPY . /srv/shiny-server/
RUN chown -R shiny:shiny /srv/shiny-server

EXPOSE 3838
HEALTHCHECK --interval=30s --timeout=5s --start-period=25s --retries=3 \
  CMD curl --fail http://localhost:3838/ || exit 1

CMD ["/usr/bin/shiny-server"]

