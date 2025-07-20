# Use the Rocker Shiny base image
FROM rocker/shiny

# Install system dependencies (including PostgreSQL dev libs for RPostgres)
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    libpq-dev \
    build-essential \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages via install2.r (from littler) with error handling and skipping already installed packages
RUN install2.r --error --skipinstalled \
    shiny \
    fresh \
    shinyWidgets \
    ISOcodes \
    jsonlite \
    shinydashboard \
    shinydashboardPlus \
    DT \
    DBI \
    config \
    yaml \
    shinyjs \
    httr \
    RPostgres

# Copy your Shiny app code to the default directory for Shiny Server
COPY . /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# Create the app cache directory and give ownership to shiny user
RUN mkdir -p /srv/shiny-server/account_Portal/app_cache && \
    chown -R shiny:shiny /srv/shiny-server/account_Portal

# Set environment variable (optional)
ENV _R_SHLIB_STRIP_=true

# Run the Shiny server
CMD ["/usr/bin/shiny-server"]
