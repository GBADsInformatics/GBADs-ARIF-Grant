extract_yaml_names <- function(folder_path) {
  if (!dir.exists(folder_path)) return(list(ideal = character(), current = character()))
  
  files <- list.files(folder_path, pattern = "\\.yaml$", full.names = FALSE)
  pattern <- "^\\d{8}T\\d{6}_user\\d+_(.+)_(ideal|current)\\.yaml$"
  
  ideal <- character()
  current <- character()
  
  for (f in files) {
    match <- regexec(pattern, f)
    res <- regmatches(f, match)[[1]]
    if (length(res) == 3) {
      user_file <- res[2]
      type <- res[3]
      if (type == "ideal") {
        ideal <- c(ideal, user_file)
      } else if (type == "current") {
        current <- c(current, user_file)
      }
    }
  }
  
  list(ideal = unique(ideal), current = unique(current))
}