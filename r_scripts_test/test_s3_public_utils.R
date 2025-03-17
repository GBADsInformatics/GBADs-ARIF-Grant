library("testthat")
source("../r_scripts/s3_public_utils.R")

test_that(desc = "Download species Dynamic Population Model (DPM) template from S3", code = {
  
  species = c("cattle","poultry","small_ruminants")
  
  dest_files <- list(
    cattle = "DPM_template_cattle.xlsx",
    poultry = "DPM_template_poultry.xlsx",
    small_ruminants = "DPM_template_small_ruminants.xlsx",
    instructions_dest_file = "instructions.pdf",
    data_dict_dest_file = "data_dictionary.xlsx"
  )
  
  # Check to see if dest_files already exist in the testdir. If they do we want to remove them so that the test can test properly.
  for (i in dest_files){

    if (file.exists(file = i)) {
      stop(sprintf("%s already exists. Please remove it from the testing directory.", i))
    }
    
  }
  
  for (i in species){
    
    download_template(i)
    
    # I feel like an apply function family situation would make the code below better, if there is time. 
    expect_that(file.exists(dest_files[[i]]), equals(TRUE))
    expect_that(file.exists(dest_files$instructions_dest_file), equals(TRUE))
    expect_that(file.exists(dest_files$data_dict_dest_file), equals(TRUE))
    
    file.remove(c(dest_files$data_dict_dest_file, dest_files$instructions_dest_file))
      
  }
  
  for (i in species){
    file.remove(dest_files[[i]])
  }
    
  })
