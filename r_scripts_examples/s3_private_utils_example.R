source("../r_scripts/s3_private_utils.R")

# First create the client, you will have to replace "aws_access_key" and "aws_secret_key" to your own 
# credentials 

svc <- create_client(aws_access_key, aws_secret_key)

# Get a url to put an object in bucket with file name 
url <- put_object_url("gbads-modelling-private","test_request.txt", svc)

# Use the url
my_request <- upload_put_url(url, "./requests_test.txt")
