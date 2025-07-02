readme_path <- "README.md"
active_dir <- "Active"
completed_dir <- "Completed"

# URL encode folder names (spaces → %20, etc.)
url_encode <- function(x) {
  utils::URLencode(x, reserved = TRUE)
}

get_project_links <- function(directory) {
  if (!dir.exists(directory)) return(character(0))
  
  folders <- list.dirs(directory, full.names = FALSE, recursive = FALSE)
  folders <- sort(folders)
  
  links <- character(0)
  for (folder in folders) {
    folder_path <- file.path(directory, folder)
    readme_file <- file.path(folder_path, "README.md")
    
    if (file.exists(readme_file)) {
      encoded_folder <- url_encode(folder)
      link <- sprintf("- [%s](%s/%s)", folder, directory, encoded_folder)
      links <- c(links, link)
    }
  }
  return(links)
}

update_readme <- function() {
  if (!file.exists(readme_path)) {
    stop("Main README.md not found.")
  }
  
  content <- readLines(readme_path)
  
  # Find section markers
  active_idx <- grep("^## Active", content)
  completed_idx <- grep("^## Completed", content)
  
  if (length(active_idx) == 0 || length(completed_idx) == 0) {
    stop("README does not contain both '## Active' and '## Completed' headers.")
  }
  
  before_active <- content[1:(active_idx - 1)]
  after_completed <- content[(completed_idx + 1):length(content)]
  
  active_links <- get_project_links(active_dir)
  completed_links <- get_project_links(completed_dir)
  
  active_block <- if (length(active_links)) active_links else "_No active projects listed._"
  completed_block <- if (length(completed_links)) completed_links else "_No completed projects listed._"
  
  new_content <- c(
    before_active,
    "## Active",
    active_block,
    "",
    "## Completed",
    completed_block,
    "",
    after_completed
  )
  
  writeLines(new_content, readme_path)
}

update_readme()
