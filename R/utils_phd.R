#'@description
#'read rda file as rds giving a name
#'
read_rda <- function(x){
  env <- new.env()
  load(x, envir = env)
  get(ls(env), envir = env)
}


cut_extreme <- function(x, min, max){
  x[x < min] <- min
  x[x > max] <- max
  return(x)
}

mtheme <- function(size = 15){
  ggplot2::theme_minimal(base_size = size,
                base_family = "sans") +
  ggplot2::theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        strip.text = element_text(face = "bold"),
        panel.grid.minor = element_blank())
}

qplot <- function(x, y,
                  type = c("none", "p", "l"),
                  data = NULL, ...){

  require(ggplot2)

  type <- match.arg(type, type, several.ok = FALSE)

  if(is.null(data)){
    data <- data.frame(x, y)
  }

  pp <- ggplot2::ggplot(data = data, aes(x = x, y = y))

  if(type == "p"){
    pp + ggplot2::geom_point(...)
  } else if(type == "l"){
    pp + ggplot2::geom_line(...)
  } else if(type == "none") {
    pp
  } else{
    stop("method not implemented yet")
  }

}
