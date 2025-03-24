library("paws")

s3 <- paws::s3()

file = "/Users/kassyraymond/Documents/GBADs-ARIF-Grant/r_scripts/test_upload.txt"

bucket_name = "gbads-modelling-private"

s3$list_objects(Bucket = "gbads-modelling-private")

# Create client
svc <- s3(
  config = list(
    credentials = list(
      creds = list(
        access_key_id = "your AWS access key",
        secret_access_key = "your AWS secret key"
      )
    )
  )
)

url <- svc$generate_presigned_url(
  client_method = "get_object",
  params = list(Bucket = bucket_name, Key = "test_upload.txt"),
  expires_in = 3600
)

url <- svc$generate_presigned_url(
  client_method = "put_object",
  params=list(Bucket = bucket_name, 
              Key = "test_file.txt"), 
  expires_in=3600,
  http_method = NULL)
