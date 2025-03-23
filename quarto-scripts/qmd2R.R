library(here)
devtools::load_all()

qmds <- list.files("slides",
                   all.files = TRUE,
                   pattern = "*.qmd", 
                   recursive = TRUE)

qmds <- file.path("slides", qmds)
lapply(qmds, qmd2R)