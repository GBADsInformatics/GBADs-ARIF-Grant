#!/bin/bash
set -e

# Download the config file
python3 /srv/shiny-server/download_config.py

# Start the Shiny server
exec /usr/bin/shiny-server
