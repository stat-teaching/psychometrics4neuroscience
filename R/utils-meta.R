# simulate k studies with unstandardized mean difference
# between two independent groups using the aggregated approach

sim_k_umd <- function(k, D, n1, n2 = NULL){
  if(is.null(n2)) n2 <- n1
  yi <- rnorm(k, D, sqrt(1/n1 + 1/n2))
  vi <- (rchisq(k, n1 + n2 - 2) / (n1 + n2 - 2)) * (1/n1 + 1/n2)
  data.frame(yi, vi)
}

# simulate k studies with unstandardized mean difference
# between two independent groups using the participant level
# data approach

sim_studies <- function(k, es, tau2 = 0, n1, n2 = NULL, add = NULL, fast = FALSE){
  if(length(n1) == 1) n1 <- rep(n1, k)
  if(is.null(n2)){
    n2 <- n1
  } else{
    if(length(n2) == 1) n2 <- rep(n2, k)
  }
  
  if(length(es) == 1) es <- rep(es, k)
  
  if(fast){
    yi <- rnorm(k, es, sqrt(1/n1 + 1/n2 + tau2))
    vi <- (rchisq(k, n1 + n2 - 2) / (n1 + n2 - 2)) * (1/n1 + 1/n2)
    sim <- data.frame(id = 1:k, yi, vi, n1, n2)
  } else{
    yi <- rep(NA, k)
    vi <- rep(NA, k)
    
    # random effects
    deltai <- rnorm(k, 0, sqrt(tau2))
    
    for(i in 1:k){
      g1 <- rnorm(n1[i], 0, 1)
      g2 <- rnorm(n2[i], es[i] + deltai[i], 1)
      yi[i] <- mean(g2) - mean(g1)
      vi[i] <- var(g1)/n1[i] + var(g2)/n2[i]
    }
    
    sim <- data.frame(id = 1:k, yi, vi, n1 = n1, n2 = n2)
  
  }
  
  if(!is.null(add)){
    sim <- cbind(sim, add)
  }
  
  # convert to escalc for using metafor methods
  sim <- metafor::escalc(yi = yi, vi = vi, data = sim)
  
  return(sim)
}

# simulate sampling variances for a two-independent UMD effect size

sim_vi <- function(k, v1 = 1, v2 = NULL, n1, n2 = NULL){
  if(is.null(v2)) v2 <- v1
  if(is.null(n2)) n2 <- n1
  (rchisq(k, n1 + n2 - 2) / (n1 + n2 - 2)) * (v1/n1 + v2/n2)
}

# quick forest plot

qforest <- function(data, interval = TRUE, wi = FALSE, size = 20){
  xlim <- with(data, c(min(ci.lb) - 0.5, max(ci.ub) + 0.5))
  data$id <- factor(data$id, levels = 1:nrow(data), labels = paste("Study", 1:nrow(data)))
  
  ggplot(data) + {
    if(wi){
      geom_point(aes(x = yi, y = id, size = 1/vi),
                 shape = 15,
                 show.legend = FALSE)
    }else{
      geom_point(aes(x = yi, y = id),
                 shape = 15,
                 size = 3)
    }
  } + {
    if(interval){
      geom_segment(aes(x = ci.lb, y = id, 
                       xend = ci.ub, yend = id))
    }
  } +
    xlim(xlim) +
    xlab("Effect Size") +
    theme_minimal(size) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_text(hjust=0)) +
    scale_y_discrete(limits=rev)
}


# re-fit a meta-analysis using all tau2 estimators

all_rma <- function(fit){
  methods <- c("DL", "HE", "HS", "HSk", "SJ", "ML", "REML",
               "EB", "PM", "PMM")
  fitl <- lapply(methods, function(m) update(fit, method = m))
  names(fitl) <- methods
  return(fitl)
}

# summarise an rma model
summary_rma <- function(x,
                        b = "intrcpt",
                        extra_params = NULL) {
  
  if(x$model != "rma.uni"){
    stop("The function currently support only rma.uni models")
  }
  
  params <- c("b", "se", "zval", "pval", "ci.lb", "ci.ub")
  
  if(!is.null(extra_params)){
    extra_params <- base::setdiff(extra_params, params)
    params <- c(params, extra_params)
  }
  
  fixefs <- lapply(params, function(p) if(is.numeric(x[p])) as.numeric(x[p]) else x[p])
  names(fixefs) <- params
  fixefs <- data.frame(fixefs)
  fixefs <- fixefs[grepl(b, rownames(fixefs)), ]
  names(fixefs)[1] <- sprintf("%s (%s)", names(fixefs)[1], b)
  
  if (is.null(x$model)) {
    sigmas <- x$sigma2
  } else {
    sigmas <- x$tau2
  }
  
  if(x$model == "rma.uni"){
    fixefs$I2 <- x$I2
  }
  
  if(length(sigmas) > 1){
    names(sigmas) <- paste0("tau2_", 1:length(sigmas))
  }else{
    names(sigmas) <- "tau2"
  }
  
  out <- cbind(fixefs, t(data.frame(sigmas)))
  rownames(out) <- NULL
  return(out)
}

# compare two rma objects, similar to car::compareCoefs()
compare_rma <- function(..., fitlist = NULL, b = "intrcpt", extra_params = NULL) {
  if(!is.null(fitlist)){
    fits <- fitlist
    fitnames <- names(fitlist)
  }else{
    fits <- list(...)
    fitnames <- as.list(substitute(...()))
  }
  sums <- lapply(fits, summary_rma, b = b, extra_params = extra_params)
  sums <- do.call(rbind, sums)
  out <- data.frame(t(sums))
  out <- setNames(out, fitnames)
  calls <- sapply(fits, function(x) deparse(x$call))
  cat(sprintf("%s: %s", fitnames, calls), sep = "\n")
  cat("\n")
  out
}

# plot an egger regression line above a funnel plot
plot_regtest <- function(fit, main = "", ...){
  reg <- regtest(fit, ...)
  funnel(fit, refline = 0, main = main)
  se <- seq(0, 1.8, length = 100)
  lines(coef(reg$fit)[1] + coef(reg$fit)[2]*se, se, lwd=3, col = "firebrick")
}

# negative-exponential selection function
wnegexp <- function(p, delta = 0, ...){
  exp((-delta) * p)
}

# step selection function
wstep <- function(p, delta, steps, ...){
  delta[findInterval(p, steps) + 1]
}

# simulate a (biased) dataset according to a selection model
sim_biased_studies <- function(k, 
                               es, 
                               tau2 = 0, 
                               nmin, 
                               nmax = NULL, 
                               type,
                               alternative = "greater",
                               delta,
                               steps = NULL
){
  if(is.null(nmax)) nmax <- nmin
  type <- match.arg(type, c("stepfun", "negexp"))
  alternative <- match.arg(alternative, c("less", "greater", "two.sided"))
  
  wfun <- switch(type,
                 "negexp" = wnegexp,
                 "stepfun" = wstep
  )
  
  if(type == "stepfun"){
    if((!all(rev(delta) == delta)) & alternative == "two.sided"){
      warning("The delta (weights) of the step 
              function appear to be non-symmetric but alternative = 'two.sided'")
    }
  }
  
  i <- 1
  data <- vector(mode = "list", length = k)
  while(i <= k){
    n <- round(runif(1, nmin, nmax))
    d <- sim_studies(1, es, tau2, n)
    d <- summary(d)
    
    if(alternative == "greater"){
      pval <- 1 - pnorm(d$zi)
    }else if(alternative == "less"){
      pval <- pnorm(d$zi)
    }else{
      pval <- d$pval
    }
    
    w <- wfun(pval, delta, steps)
    keep <- rbinom(1, 1, w) == 1
    if(keep){
      data[[i]] <- d
      i = i + 1
    }
  }
  
  data <- do.call(rbind, data)
  return(data)
  
}

# analytical power analysis
power_meta <- function(es, k, tau2 = 0, n1, n2 = NULL, alpha = 0.05){
  if(is.null(n2)) n2 <- n1
  zc <- qnorm(1 - alpha/2)
  vt <- 1/n1 + 1/n2
  ves <- (vt + tau2)/k
  lambda <- es/sqrt(ves)
  (1 - pnorm(zc - lambda)) + pnorm(-zc - lambda)
}


summary_es <- function(data){
  out <- metafor::escalc(yi = yi, vi = vi, sei = sei, data = data)
  data.frame(summary(out))
}