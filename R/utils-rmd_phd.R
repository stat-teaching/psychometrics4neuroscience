refbook <- function(key, page){
  refs <- sprintf("@%s [pp. %s]", key, page)
  refs <- paste0(refs, collapse = ", ")
  paste0("<aside class='citation refbook'>\n📘 ", refs, "\n</aside>")
}

nprint <- function(..., digits = 3){
  dots <- list(...)
  dots <- lapply(dots, round, digits)
  print(unlist(dots))
}

code <- function(x, format = c("html", "markdown", "latex")){
  format <- match.arg(format)
  if(format == "html"){
    sprintf("<code>%s</code>", x)
  }else if(format == "markdown"){
    sprintf("`%s`", x)
  }else{
    sprintf("\\texttt{%s}", x)
  }

}

latex <- function(text, ...){
  latex2exp::TeX(sprintf(text, ...))
}

dcode <- function(expr, between = NULL, digits = 3){
  code <- deparse(substitute(expr))
  result <- eval(expr, envir = parent.env(environment()))
  if(is.numeric(result)) result <- round(result, 3)
  if(is.null(between)){
    sprintf("`%s` %s", code, result)
  }else{
    sprintf("`%s` %s %s", code, between, result)
  }
}
