# install_packages.R

# List all R files in the current folder (app.R, ui.R, server.R, global.R, etc.)
files <- list.files(pattern = "\\.R$", full.names = TRUE)

# Extract package names used in library() or require() calls
pkgs <- unique(unlist(
  lapply(files, function(file) {
    lines <- readLines(file, warn = FALSE)
    matches <- regmatches(lines, gregexpr("\\b(library|require)\\(([^)]+)\\)", lines))
    unlist(lapply(matches, function(x) {
      gsub("\\b(library|require)\\(|\\)", "", x)
    }))
  })
))

# Clean and trim whitespace
pkgs <- trimws(pkgs)
pkgs <- pkgs[nzchar(pkgs)]  # remove any empty strings

# Find packages that are not yet installed
missing_pkgs <- pkgs[!(pkgs %in% installed.packages()[, "Package"])]

# Install missing packages
if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
} else {
  message("✅ All required packages are already installed.")
}
