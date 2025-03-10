library("testthat")
source("../r_scripts/s3_public_utils.R")

test_that(desc = "Download species Dynamic Population Model (DPM) template from S3", code = {
  
  species = c("cattle","poultry","small_ruminants")
  
  dest_files <- list(
    cattle = "DPM_template_cattle.xlsx",
    poultry = "DPM_template_poultry.xlsx",
    small_ruminants = "DPM_template_small_ruminants.xlsx"
  )
  
  # Check to see if dest_files already exist in the testdir. If they do we want to remove them so that the test can test properly.
  
  
  for (i in species){
    
    if (file.exists(file = dest_files[[i]]) == TRUE) {
      stop("Species template already exists. Please remove it from the testing directory.")
    }
    
  }
  
  for (i in species){
    
    download_template(i)
    
    expect_that(object = file.exists(file = dest_files[[i]]), condition = equals(TRUE));
      
    }
    
  })
