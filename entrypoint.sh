#!/bin/bash
set -e

# Export DPM_AUTH_TOKEN to R via Renviron.site if set
if [ ! -z "$DPM_AUTH_TOKEN" ]; then
	echo "DPM_AUTH_TOKEN=$DPM_AUTH_TOKEN" > /home/shiny/.Renviron
    chown shiny:shiny /home/shiny/.Renviron
fi

# Start the Shiny server
exec /usr/bin/shiny-server
