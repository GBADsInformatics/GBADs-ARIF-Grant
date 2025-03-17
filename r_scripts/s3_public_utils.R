library("RCurl")

# The templates are in an S3 bucket that is public meaning that you shouldn't need security information to 
# access the data. This means that for public utils we can just use the RCurl package

download_template <- function(species = c("cattle","poultry","small_ruminants")){
  
  species <- match.arg(species)
  
  data_dictionary_url <- "https://gbads-modelling.s3.ca-central-1.amazonaws.com/20250317_DataDictionary_Placeholder.xlsx"
  instructions_url <- "https://gbads-modelling.s3.ca-central-1.amazonaws.com/20250317_UserInstructions.pdf"
  
  template_urls <- list(
    cattle = "https://gbads-modelling.s3.ca-central-1.amazonaws.com/20250310_DPM_template_cattle.xlsx",
    poultry = "https://gbads-modelling.s3.ca-central-1.amazonaws.com/20250310_DPM_template_poultry.xlsx",
    small_ruminants = "https://gbads-modelling.s3.ca-central-1.amazonaws.com/20250310_DPM_template_smallruminants.xlsx"
  )
  
  dest_files <- list(
    cattle = "DPM_template_cattle.xlsx",
    poultry = "DPM_template_poultry.xlsx",
    small_ruminants = "DPM_template_small_ruminants.xlsx"
  )
  
  download.file(url = template_urls[[species]],
                destfile = dest_files[[species]],
                method = "curl")
  
  download.file(url = data_dictionary_url,
                destfile = "data_dictionary.xlsx",
                method = "curl")
  
  download.file(url = instructions_url,
                destfile = "instructions.pdf",
                method = "curl")
  
}


