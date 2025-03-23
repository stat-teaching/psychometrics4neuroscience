make_slide_index <- function(exclude = c("extra")){
  qmd <- list.files("slides", pattern = "*.qmd", recursive = TRUE, full.names = TRUE)
  if(!is.null(exclude)){
    qmd <- qmd[!grepl(paste0(exclude, collapse = "|"), qmd)]
  }
  html <- paste0(tools::file_path_sans_ext(qmd), ".html")
  #qmd <- gsub("slides/", "", qmd)
  #html <- gsub("slides/", "", html)
  data.frame(
    Topic = tools::file_path_sans_ext(qmd),
    qmd = sprintf('<a href="%s" target="_blank">qmd</a>', qmd),
    html = sprintf('<a href="%s" target="_blank">html</a>', html)
  )
}
