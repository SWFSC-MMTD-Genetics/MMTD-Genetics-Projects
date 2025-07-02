readme_path <- "README.md"
active_dir <- "Active"
completed_dir <- "Completed"

# Encode folder names to be safe in Markdown links
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
  active_links <- get_project_links(active_dir)
  completed_links <- get_project_links(completed_dir)
  
  active_block <- if (length(active_links)) active_links else "_No active projects listed._"
  completed_block <- if (length(completed_links)) completed_links else "_No completed projects listed._"
  
  new_content <- c(
    "# Projects",
    "This repository contains various MMTD Genetics projects organized into subfolders",
    "",
    "# Project List",
    "",
    "<details>",
    "<summary><strong>Active Projects</strong></summary>",
    "",
    active_block,
    "",
    "</details>",
    "",
    "<details>",
    "<summary><strong>Completed Projects</strong></summary>",
    "",
    completed_block,
    "",
    "</details>",
    "",
    "# Create A New Project"
  )
  
  writeLines(new_content, readme_path)
}

update_readme()
