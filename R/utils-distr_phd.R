ggnorm <- function(mean = 0, sd = 1, lf = 4, ns = 1e4, annotate = FALSE, theme = NULL, size = 15){
  require(ggplot2)
  if(length(mean) != length(sd)){
    stop("mean and sd need to be of the same length!")
  }
  n <- length(mean)
  range <- c(min(mean) - lf * max(sd), max(mean) + lf * max(sd))
  x <- seq(range[1], range[2], length.out = ns)
  d <- mapply(function(m, s) dnorm(x, m, s), mean, sd, SIMPLIFY = FALSE)
  D <- data.frame(x = rep(x, n),
                  d = unlist(d),
                  mean = rep(mean, each = length(x)),
                  sd = rep(sd, each = length(x)))
  D$msd <- sprintf("m = %s, s = %s", D$mean, D$sd)
  labs <- sprintf("$\\mu = %s$, $\\sigma = %s$", mean, sd)

  p <- ggplot(D, aes(x = x, y = d, color = msd)) +
    geom_line(lwd = 1) +
    scale_color_discrete(labels = lapply(labs, latex2exp::TeX)) +
    ylab(latex2exp::TeX("$dnorm(x, \\mu, \\sigma)$")) +
    xlim(range) +
    theme_minimal(size) +
    theme(legend.position = "bottom",
          legend.title = element_blank())
  if(annotate){
    title <- sprintf("$\\mu = %.3f$, $\\sigma = %.3f$", mean, sd)
    title <- paste(title, collapse = " ")
    p <- p + ggtitle(latex2exp::TeX(title))
  }
  if(!is.null(theme)){
    p + theme(size)
  }
  return(p)
}

ggpois <- function(lambda, lf = 3, type = c("h", "pl"), annotate = FALSE, theme = NULL, size = 15){
  require(ggplot2)
  type <- match.arg(type)
  range <- c(0, max(lambda)*lf)
  if(-min(lambda)*lf < 0) range[1] <- 0 else range[1] <- -min(lambda)*lf
  x <- seq(range[1], range[2], 1)
  D <- expand.grid(x = x, lambda = lambda)
  D$d <- dpois(D$x, D$lambda)
  p <- ggplot(D,
         aes(x = x,
             y = d))
  if(type == "h"){
    p <- p + geom_col(aes(fill = factor(lambda)), width = 0.3)
  }else{
    p <- p +
      geom_point(aes(color = factor(lambda)),
                 size = 3) +
      geom_line(aes(group = factor(lambda),
                    color = factor(lambda)))
  }
  p <- p +
    xlab("x") +
    ylab(latex2exp::TeX("$dpois(x, \\lambda)$")) +
    labs(
      fill = latex2exp::TeX("$\\lambda$"),
      color = latex2exp::TeX("$\\lambda$")
    ) +
    theme_minimal(size) +
    theme(legend.position = "bottom") +
  if(!is.null(theme)) p <- p + theme(size)
  if(annotate){
    title <- sprintf("$\\lambda_{%s} = %s$", 1:length(lambda), lambda)
    title <- paste(title, collapse = ", ")
    p <- p + ggtitle(latex2exp::TeX(title))
  }
  return(p)
}

eqs <- function(){
  list(
    gamma = "mean  = shape * scale   mean  = shape/rate \nvar   = shape * rate^2  var   = shape/rate^2"

  )
}


gamma_params <- function(shape = NULL, scale = 1/rate, rate = 1,
                         mean = NULL, sd = NULL,
                         eqs = FALSE){
  if(eqs){
    cat(rep("=", 25), "\n")
    cat(eqs()$gamma, "\n")
    cat(rep("=", 25), "\n")
  }else{
      if(is.null(shape)){
      var <- sd^2
      shape <- mean^2 / var
      scale <- mean / shape
      rate <- 1/scale
    } else if(is.null(mean) & is.null(sd)){
      if(is.null(rate)){
        scale <- 1/rate
      } else{
        rate <- 1/scale
      }
      mean <- shape * scale
      var <- shape * scale^2
      sd <- sqrt(var)
    }else{
      stop("when shape and scale are provided, mean and sd need to be NULL (and viceversa)")
    }
    out <- list(shape = shape, scale = scale, rate = rate, mean = mean, var = var, sd = sd)
    # coefficient of variation
    out$cv <- 1/sqrt(shape)
    return(out)
  }
}

ggamma <- function(shape = NULL,
                   scale = 1/rate,
                   rate = 1,
                   mean = NULL,
                   sd = NULL,
                   show = c("msd", "sr", "ss"),
                   lf = 5,
                   size = 15,
                   ns = 1e4){
  show <- match.arg(show)
  argg <- as.list(environment())[1:5]
  gm <- do.call(gamma_params, argg)
  if(length(unique(sapply(gm, length))) != 1){
    stop("All vectors need to be of the same length!")
  }

  n <- length(gm$mean)
  range <- c(0, max(gm$mean) + lf * max(gm$sd))
  x <- seq(range[1], range[2], length.out = ns)
  d <- mapply(function(sh, sc) dgamma(x, shape = sh, scale = sc), gm$shape, gm$scale, SIMPLIFY = FALSE)
  D <- data.frame(x = rep(x, n),
                  d = unlist(d),
                  shape = rep(gm$shape, each = length(x)),
                  scale = rep(gm$scale, each = length(x)),
                  rate = rep(gm$rate, each = length(x)),
                  mean = rep(gm$mean, each = length(x)),
                  sd = rep(gm$sd, each = length(x))
  )

  if(show == "msd"){
    D$cond <- factor(sprintf("$\\mu = %.3f$, $\\sigma = %.3f$", D$mean, D$sd))
  }else if(show == "ss"){
    D$cond <- factor(sprintf("$k (shape) = %.3f$, $\\theta (scale) = %.3f$", D$shape, D$scale))
  }else{
    D$cond <- factor(sprintf("$\\alpha (shape) = %.3f$, $\\beta (rate) = %.3f$", D$shape, D$rate))
  }

  ggplot(D, aes(x = x, y = d, color = cond)) +
    geom_line(lwd = 1) +
    scale_color_discrete(labels = lapply(levels(D$cond), latex2exp::TeX)) +
    theme_minimal(15) +
    theme(legend.title = element_blank(),
          legend.position = c(.95, .95),
          legend.justification = c("right", "top"),
          legend.box.just = "right",
          legend.margin = margin(6, 6, 6, 6)
    ) +
    ylab(latex2exp::TeX("dgamma($x$, $\\alpha$/$k$, $\\theta$, $\\beta$)"))
}


gamma_shape <- function(x, method = c("glm", "invskew")){
  method = match.arg(method)
  if(method == "glm"){
    formula <- paste(deparse(substitute(x)), "~ 1")
    fit <- glm(as.formula(formula), family = Gamma(link = "log"))
    shape <- MASS::gamma.shape(fit)$alpha
  }else{
    skew <- psych::skew(x)
    shape <- 4/skew^2
  }
  return(shape)
}

cv <- function(x){
  sd(x)/mean(x)
}
