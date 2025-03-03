# Get user info based on user_id 
get_user_info <- function(user_id){
  paste0("SELECT * FROM users WHERE user_id = ", user_id, ";" )
}