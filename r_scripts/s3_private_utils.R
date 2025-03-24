library("paws")

# Note that to make this work you need to first ensure that you set your sys.env 
# credentials using the secret keys in aws 

create_client <- function(){
  
  s3 <- paws::s3()
  
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
  
  return(svc)
  
}

get_object_url <- function(bucket_name, file_name){ 
  
  # The key here is the name of the file that you want to give the user access to.

  url <- svc$generate_presigned_url(
    client_method = "get_object",
    params = list(Bucket = bucket_name, Key = file_name),
    expires_in = 3600
  )
  
  return(url)
  
}

put_object_url <- function(bucket_name, file_name){
  
  # The key has to be the name of the file that you want to be able to write to.

  url <- svc$generate_presigned_url(
    client_method = "put_object",
    params=list(Bucket = bucket_name, 
                Key = file_name), 
    expires_in=3600,
    http_method = NULL)
}

