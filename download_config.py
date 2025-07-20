import requests
import os

# Constants
URL = "https://gbadske.org/api/dpm/download"
PARAMS = {
    "bucket_name": "gbads-modelling-private",
    "object_name": "config/config.yml"
}
HEADERS = {
    "accept": "application/json",
    "Authorization": f"bearer {os.getenv("DPM_AUTH_TOKEN")}"
}
TEMP_FILENAME = "config_config.yml"
FINAL_FILENAME = "/srv/shiny-server/config.yml"

def download_and_rename():
    response = requests.get(URL, headers=HEADERS, params=PARAMS)
    
    if response.status_code != 200:
        raise Exception(f"Failed to download file: {response.status_code} {response.text}")

    with open(TEMP_FILENAME, "wb") as f:
        f.write(response.content)

    os.rename(TEMP_FILENAME, FINAL_FILENAME)
    print(f"File saved as {FINAL_FILENAME}")

if __name__ == "__main__":
    download_and_rename()
