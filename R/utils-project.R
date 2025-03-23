qmd2R <- function(x){
  out <- sprintf("%s.R", xfun::sans_ext(basename(x)))
  knitr::purl(input = x, 
              output = file.path(dirname(x), out), 
              documentation = 2,
              quiet = TRUE)
}