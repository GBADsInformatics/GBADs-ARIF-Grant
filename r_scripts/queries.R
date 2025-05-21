# Check if user exists
check_user_exists <- function(user_id) {
  paste0("SELECT EXISTS(SELECT 1 FROM users WHERE user_id = ", user_id, ") AS exists;")
}

# Check if email exists
check_email_exists <- function(user_email) {
  paste0("SELECT EXISTS(SELECT 1 FROM users WHERE user_email = '", user_email, "') AS exists;")
}

# Get user info based on user_id 
get_user_info <- function(user_id){
  paste0("SELECT * FROM users WHERE user_id = ", user_id, ";" )
}
