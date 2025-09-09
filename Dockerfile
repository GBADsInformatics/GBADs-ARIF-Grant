# Use the Rocker Shiny base image
FROM rocker/shiny

# Install system dependencies
RUN apt-get update && apt-get install -y \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libpq-dev \
    build-essential \
    python3 \
    python3-pip \
    libwebp-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r --error --skipinstalled \
    config \
    DBI \
    dplyr \
    DT \
    flextable \
    foreach \
    fresh \
    ggplot2 \
    ggrepel \
    httr \
    ISOcodes \
    jsonlite \
    plotly \
    purrr \
    readr \
    rlang \
    RPostgres \
    scales \
    shiny \
    shinycssloaders \
    shinydashboard \
    shinydashboardPlus \
    shinyjs \
    shinyWidgets \
    stringr \
    yaml \
    doParallel

# Install Python packages (override PEP 668 protection)
RUN pip3 install --break-system-packages requests

# Copy app and config
COPY . /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf
COPY entrypoint.sh /entrypoint.sh

# Make the entrypoint script executable
RUN chmod +x /entrypoint.sh

# Setup permissions
RUN mkdir -p /srv/shiny-server/account_Portal/app_cache && \
    chown -R shiny:shiny /srv/shiny-server/account_Portal

# Set environment variable
ENV _R_SHLIB_STRIP_=true

# Entrypoint script
ENTRYPOINT ["/entrypoint.sh"]
