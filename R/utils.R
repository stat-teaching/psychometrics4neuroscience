fit_by_cluster <- function(formula, data, model = NULL, args = NULL){
  if(is.null(model)){
    model <- lm
  }
  
  parts <- lme4:::modelFormula(formula)
  groups <- as.character(parts$groups)
  datal <- split(data, data[[groups]])
  
  args$formula <- parts$model
  
  lapply(datal, function(x){
    do.call(model, args = c(args, list(data = x)))
  })
  
}

get_Z_matrix <- function(formula, data){
  data$y <- 1 # mock y
  formula <- paste0("y ~ ", as.character(formula)[2])
  mock_glmer <- lme4::lFormula(formula, data)
  Z <- t(as.matrix(mock_glmer$reTrms$Zt))
  return(Z)
}
cmc <- function(x, cluster){
  x - cm(x, cluster)
}

cm <- function(x, cluster){
  cm <- tapply(x, cluster, mean)
  cm[cluster]
}

gmc <- function(x){
  x - mean(x)
}

ggcurve <- function(expr, xlim = c(0, 1), n = 100){
  expr <- substitute(expr)
  x <- seq(xlim[1], xlim[2], length.out = n)
  eval(expr)
}

qplot <- function(x, y, type = "p", ...){
  xn <- deparse(substitute(x))
  yn <- deparse(substitute(y))
  dd <- data.frame(x, y)
  names(dd) <- c(xn, yn)
  pp <- ggplot2::ggplot(dd, 
                        ggplot2::aes(x = {{x}}, y = {{y}})) +
    ggplot2::xlab(xn) +
    ggplot2::ylab(yn)
  
  if(type == "p"){
    pp + ggplot2::geom_point(...)
  } else{
    pp + ggplot2::geom_line(...)
  }
}

filter_output <- function(x, lines = NULL, cat = TRUE){
  res <- capture.output(x)
  if(is.null(lines)) lines <- 1:length(res)
  
  if(!is.numeric(lines)){
    if(length(lines) != 1){
      stop("When lines is not a numeric vector, need to be a character vector of length 1 with regex 'start|end'!")
    }
    
    rr <- unlist(strsplit(lines, split = "\\|"))
    lines <- grep(rr[1], res):grep(rr[2], res)
  }
  
  if(cat){
    cat(res[lines], sep = "\n")
  }
}

rmnorm <- function(n, mu, r, names = NULL){
  R <- filor::rmat(r)
  X <- MASS::mvrnorm(n, mu, R)
  if(is.null(names)){
    names <- paste0("x", 1:length(mu))
  }
  X <- data.frame(X)
  colnames(X) <- names
  #tidyr::pivot_longer(X, 1:ncol(X), names_to = "var", values_to = "y")
  X
}

mkdir_if <- function(dir){
  if(!dir.exists(dir)) dir.create(dir)
}

link_refs <- function(){
  file <- xfun::embed_file("refs_to_download.bib", text = " Download .bib file")
  sprintf('<button class="btn"><i class="fa fa-download"></i>%s</button>', file)
}

#' quiet(function(x))
#' Suppresses output messages
#' By Hadley Wickham
#' http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
#'
#' @export

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

mtheme <- function(){
  theme_bw(15)
}

mkdir_if <- function(dir){
  if(!dir.exists(dir)) dir.create(dir)
}

link_refs <- function(){
  file <- xfun::embed_file("refs_to_download.bib", text = " Download .bib file")
  sprintf('<button class="btn"><i class="fa fa-download"></i>%s</button>', file)
}

#' quiet(function(x))
#' Suppresses output messages
#' By Hadley Wickham
#' http://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
#'
#' @export

quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}

get_yaml_params <- function(file){
  lines <- suppressWarnings(quiet(readLines(file)))
  yaml_index <- grep("^---$", lines)
  yaml::read_yaml(text = lines[yaml_index[1]:yaml_index[2]])
}

read_all_funs <- function(path = "R"){
  funs <- list.files("R/", pattern = "*.R", include.dirs = FALSE, full.names = TRUE)
  filor::get_funs(funs)
}

ht <- function(x, n = 5){
  if(is.vector(x)) k <- length(x) else k <- nrow(x)
  if(k < n*2){
    show <- x
  }else{
    idx <- c(1:n, (k - n):k)
    keep <- (1:k) %in% idx
    show <- subset(x, subset = keep)
  }
  if(is.vector(x)) names(show) <- idx
  return(show)
}

get_result <- function(..., lines = NULL, comment = NULL){
  res <- capture.output(...)
  if(!is.null(lines)){
    if(is.character(lines)){
      if(length(lines) == 1) lines[2] <- lines[1]
      start <- grep(lines[1], res)
      end <- grep(lines[2], res)
      res <- res[start:end]
    }else{
      res <- res[lines]
    }
  }
  if(!is.null(comment)){
    res <- paste(comment, res)
  }
  invisible(res)
  cat(res, sep = "\n")
}

as_tex_label <- function(x, pattern){
  x <- factor(x)
  labels <- sprintf(pattern, levels(x))
  factor(x, levels = levels(x), labels = latex2exp::TeX(labels))
}

get_all_funs <- function(dir = "R"){
  Rs <- list.files(dir, "*.R", full.names = TRUE)
  funs <- lapply(Rs, filor::get_funs)
  unlist(funs, recursive = FALSE)
}
