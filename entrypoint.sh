#!/bin/bash
set -e

# Export environment variables to R via Renviron.site
{
    if [ ! -z "$DPM_AUTH_TOKEN" ]; then
        echo "DPM_AUTH_TOKEN=$DPM_AUTH_TOKEN"
    fi
    
    if [ ! -z "$COGNITO_CLIENT_ID" ]; then
        echo "COGNITO_CLIENT_ID=$COGNITO_CLIENT_ID"
    fi
    
    if [ ! -z "$ENV" ]; then
        echo "ENV=$ENV"
    fi
} > /home/shiny/.Renviron

if [ -f /home/shiny/.Renviron ]; then
    chown shiny:shiny /home/shiny/.Renviron
fi

# Start the Shiny server
exec /usr/bin/shiny-server
