library("paws")
library("httr")

create_client <- function(aws_access_key, aws_secret_key){
  
  #' Create client for aws s3 access
  #' @param aws_access_key (string) access key for accessing private s3 bucket
  #' @param aws_secret_key (string) secret key for accessing private s3 bucket
  #' @returns returns client 
  
  s3 <- paws::s3()
  
  svc <- s3(
    config = list(
      signature_version = 's3v4',
      credentials = list(
        creds = list(
          access_key_id = aws_access_key,
          secret_access_key = aws_secret_key
        )
      ),
      region = "us-east-2"
    )
  )
  
  return(svc)
  
}

get_object_url <- function(bucket_name, file_name, svc){ 
  
  #' Generate pre signed url for a given bucket and file name for getting access to an object in s3
  #' @param bucket_name Name of the bucket that you want to give the user access to
  #' @param file_name Name of the key (file) that you want to give the user access to
  #' @param svc client
  #' @return A url is returned, providing access to the object in the bucket. 

  url <- svc$generate_presigned_url(
    client_method = "get_object",
    params = list(Bucket = bucket_name, Key = file_name),
    expires_in = 3600
  )
  
  return(url)
  
}

put_object_url <- function(bucket_name, file_name, svc){
  
  #' Generate pre signed url for a given bucket and file name for putting objects in a bucket in s3
  #' @param bucket_name Name of the bucket that you want to give the user access to
  #' @param file_name Name of the key (file) that you want to give the user access to
  #' @param svc Client
  #' @return A url is returned, which can be used to put objects in the bucket

  url <- svc$generate_presigned_url(
    client_method = "put_object",
    params=list(Bucket = bucket_name, 
                Key = file_name), 
    expires_in=3600,
    http_method = NULL)
}

upload_put_url <- function(url, file_path){
  
  # Need to figure out error handling in R for this.
  httr::PUT(url, body = file_path)
  
}



