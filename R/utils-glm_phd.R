# this is a wrapper of the predict function
# add prediction to a dataframe given a model
# extra arguments to predict are passed using ...
# it works well with pipes %>% or |>

add_predict <- function(data, fit, ...){
    pr <- predict(fit, newdata = data, ...)
    tibble::tibble(cbind(data, data.frame(pr)))
}

# print model equation with generic or actual values
# thanks to https://stats.stackexchange.com/a/433060 for the
# equation part. I've added the printing and prediction

model_equation <- function(model, values = NULL, ..., only_print = FALSE) {
    format_args <- list(...)
    model_coeff <- model$coefficients
    model_coeff <- round(model_coeff, 3)
    format_args$x <- abs(model_coeff)
    model_coeff_sign <- sign(model_coeff)
    model_coeff_prefix <- dplyr::case_when(model_coeff_sign == -1 ~ " - ",
                                           model_coeff_sign == 1 ~ " + ",
                                           model_coeff_sign == 0 ~ " + ")
    nms <- names(model_coeff[-1])
    # check if there are values to print
    if(!is.null(values)){
        # check if there are too much values to print
        if(length(values[[1]]) > 3){
            # formatting
            values <- lapply(values, function(x) sprintf("[%s ... %s]", x[1], x[length(x)]))
        }

        nms <- purrr::reduce2(names(values),
                              as.character(values),
                              .init = nms,
                              stringr::str_replace)
    }


    y <- strsplit(as.character(model$call$formula), "~")[[2]]
    b0 <- paste0(ifelse(model_coeff[1] < 0, "-", ""), do.call(base::format, format_args)[1])
    bs <- paste0(model_coeff_prefix[-1],
                 do.call(base::format, format_args)[-1],
                 "*",
                 cli::col_blue(nms),
                 sep = "", collapse = "")

    model_eqn <- sprintf("%s ~ %s%s", cli::col_green(y), b0, bs)
    cat(model_eqn)
    if(!only_print){
        invisible(model_eqn)
    }
}

# improved predict function, it works as the standard predict
# function but prints the model equation that is supporting
# the current prediction

epredict <- function(fit, values, ...){
    message(model_equation(fit, values, only_print = TRUE))
    pr <- predict(fit, values, ...)
    as.numeric(sapply(pr, unname))
}

# computes the odds of a probability
odds <- function(p){
    p / (1 - p)
}

# computes the odds ratio of two probabilities
# pn is the numerator and pd is the denominator
odds_ratio <- function(pn, pd){
    odds(pn) / odds(pd)
}

# plotting a parameter sampling distribution with different confidence
# intervals. b = parameter name from model summary
plot_param <- function(fit, b = NULL, conf.level = 0.95){
    if(is.null(b)) b <- names(coef(fit))
    fits <- broom::tidy(fit, conf.int = TRUE, conf.level = conf.level)
    fits <- fits[fits$term %in% b, ]
    title <- sprintf("Sampling distribution of $\\beta$ [%s]", b)

    plt <- ggplot(fits) +
      geom_pointrange(aes(y = term, x = estimate, xmin = conf.low, xmax = conf.high))

    if(min(fits$conf.low) < 0 & 0 < max(fits$conf.high)){
      plt <- plt +
        geom_vline(xintercept = 0, lty = "dashed", col = "firebrick") +
        xlab(latex("\\beta")) +
        theme(axis.title.y = element_blank())
    }
    return(plt)
}

# convert xtable to df (tibble)
ct_to_df <- function(ct){
    df <- data.frame(ct)
    names(df)[length(names(df))] <- "n"
    tibble::tibble(df)
}

# get the probability at the numerator
# of an odds ratio given the probability
# at the denominator and the desired odds
# ratio

pn_from_or <- function(pd, or){
    (or * pd) / (pd * (or - 1) + 1)
}

# compute the difference between two
# probabilities and the confidence interval
pdiff <- function(p1, p2, n1, n2 = NULL){
    if(is.null(n2)){
        n2 <- n1
    }
    pd <- p1 - p2
    se <- sqrt((p1 * (1 - p1))/n1 + (p2 * (1 - p2))/n2)
    ci <- pd + se * qnorm(c(0.025, 0.975))
    cat(sprintf("p1 - p2 = %.3f (SE = %.3f)\n95%% CI = [%.3f, %.3f])",
                pd, se, ci[1], ci[2]))
}

# binomial dataset to binary dataset given
# the number of correct (nc) and total trials (nt)
# the columns can be provided unquoted
bin_to_binary <- function(data, nc, nt){
    nt <- substitute(nt)
    nc <- substitute(nc)
    nts <- subset(data, select = eval(nt), drop = TRUE)
    ncs <- subset(data, select = eval(nc), drop = TRUE)
    drep <- data[rep(seq_len(nrow(data)), nts), ]
    y <- lapply(1:nrow(data), function(i){
        rep(c(1, 0), c(ncs[i], nts[i] - ncs[i]))
    })
    drep$y <- unlist(y)
    rownames(drep) <- NULL
    tibble::tibble(drep[, c("y", names(data))])
}

# binary dataset to binomial dataset
# the y variable is the 0/1 and ...
# are the varibles to group by and compute
# the total

binary_to_bin <- function(data, y, ...){
    y <- rlang::enexpr(y)
    dots <- rlang::enexprs(...)
    data |>
        dplyr::group_by(!!!dots) |>
        dplyr::summarise(nc = sum(!!y),
                         nf = n() - nc,
                         nt = n())
}

# plotting dfbeta with cutoffs given a model a eventually
# a vector of parameters
# onlyout = TRUE plot only the parameters with outliers
# cutoff if not specified is taken as 1/sqrt(n) where
# n is the number of observations

dfbeta_plot <- function(fit,
                        params = NULL,
                        onlyout = FALSE,
                        cutoff = NULL){
    n <- nrow(fit$data)
    if(is.null(cutoff)){ # default to
        cutoff <- 2/sqrt(n)
    }

    dfb <- data.frame(dfbeta(fit))

    if(!is.null(params)){ # select some parameters
        dfb <- dfb[, params]
    }

    dfb$id <- 1:nrow(dfb)
    names(dfb) <- c(names(coef(fit)), "id")
    dfb <- dfb[, c("id", names(coef(fit)))]
    dfb <- reshape2::melt(dfb, id.vars = "id")
    dfb$out <- abs(dfb$value) > cutoff

    if(onlyout){ # plot only parameters with at least 1 outlier
        sel <- unique(dfb$variable[dfb$out])
        if(length(sel) < 1){
            stop("The model has not influential observation!")
        }
        dfb <- dfb[dfb$variable %in% sel, ]
    }

    ggplot2::ggplot() +
        ggplot2::geom_vline(xintercept = c(-cutoff, cutoff),
                            linetype = "dashed",
                            color = "firebrick") +
        ggplot2::geom_segment(data = dfb,
                              aes(x = 0, xend = value, y = id, yend = id)) +
        ggplot2::geom_point(data = dfb[!dfb$out, ],
                            aes(x = value, y = id),
                            color = "blue") +
        ggplot2::geom_label(data = dfb[dfb$out, ],
                            fill = "white",
                            aes(x = value, y = id, label = id)) +
        ggplot2::facet_wrap(~variable, scales = "free_x") +
        ggplot2::xlab("DFBETAs") +
        ggplot2::ylab("Observations")
}

# plotting cook distances with cutoff given a model a eventually
# a vector of parameters
# onlyout = TRUE plot only the parameters with outliers
# cutoff if not specified is taken as 1/sqrt(n) where
# n is the number of observations

cook_plot <- function(fit, cutoff = NULL){
    n <- nrow(fit$data)
    cook <- data.frame(cooks.distance(fit))
    names(cook) <- "d"

    if(is.null(cutoff)){
        cutoff <- 4/n
    }

    cook$out <- cook$d > cutoff
    cook$id <- 1:nrow(cook)

    ggplot() +
        geom_segment(data = cook,
                     aes(x = 0, xend = d, y = id, yend = id)) +
        geom_vline(xintercept = cutoff, color = "red", linetype = "dashed") +
        geom_point(data = cook[!cook$out, ],
                   aes(x = d, y = id)) +
        geom_label(data = cook[cook$out, ],
                   aes(x = d, y = id, label = id)) +
        ylab("Observations") +
        xlab("Cook Distances")
}

# just a wrapper of stats::influence.measures()
# returning the dataframe of influence measures

infl_measure <- function(fit){
    data.frame(stats::influence.measures(fit)$infmat)
}

# return the error rate of a glm binary model
error_rate <- function(fit){
    pi <- predict(fit, type = "response")
    yi <- fit$y
    cr <- mean((pi > 0.5 & yi == 1) | (pi < 0.5 & yi == 0))
    1 - cr # error rate
}

# create a dataframe for simulating data
# ns = number of observation
#   - for numeric variables (nx) the ns is the total number of values
#   - for categorical variables (cx) is the number of observations per cell
# nx = named list() of numerical predictors
# cx = named list() of categorical predictors
# contrast = which contrast to apply to categorical variables, default
# to contr.treatement

sim_design <- function(ns, nx = NULL, cx = NULL, contrasts = contr.treatment){
    data <- data.frame(id = 1:ns)
    if(!is.null(cx)){
        data <- tidyr::expand_grid(data, !!!cx)
        data$id <- 1:nrow(data)
    }
    if(!is.null(nx)){
        data <- cbind(data, nx)
    }
    if(!is.null(contrasts) & !is.null(cx)){
        data <- set_contrasts(data, contrasts)
        data <- .num_from_contrast(data)
    }else{
        data <- tibble::tibble(dplyr::mutate(data, across(where(is.character), as.factor)))
    }
    data <- dplyr::select(data, id, everything())
    return(data)
}

# simulate data using the linpred expression and either a binomial
# or poisson model.
# linpred = expression that will be evaluated within the data object
# model = if using rbinom or poisson
sim_data <- function(data, linpred, model = c("binomial", "poisson")){
    model <- match.arg(model)
    linpred <- rlang::enexpr(linpred)
    data$lp <- with(data, eval(linpred))
    if(model == "binomial"){
        data$y <- rbinom(nrow(data), 1, data$lp)
    }else{
        data$y <- rpois(nrow(data), data$lp)
    }
    return(data)
}

# compute confusion matrix statistics from a fitted
# binomial model

#' classify
#'
#' @param data a dataframe
#' @param y name of the response (binary) variable
#' @param x name of the numeric predictor
#' @param c vector of cutoffs
#' @param na.rm logical indicating whether NA should be removed or not. Default to \code{FALSE}
#'
#' @return a dataframe with several classification metrics
#' @export
#'
classify <- function(data, y, x, c, na.rm = FALSE){
  xn <- deparse(substitute(x))
  yn <- deparse(substitute(y))

  if(na.rm){
    data <- data[complete.cases(data[, c(xn, yn)]), ]
  }

  confusion <- lapply(c, function(cr){
    # classify based on c
    yp <- ifelse(data[[xn]] >= cr, 1, 0)

    out <- data.frame(
      tp = sum(data[[yn]] == 1 & yp == 1),
      fp = sum(data[[yn]] == 0 & yp == 1),
      tn = sum(data[[yn]] == 0 & yp == 0),
      fn = sum(data[[yn]] == 1 & yp == 0)
    )

    # rates
    out$tpr <- with(out, tp / (tp + fn))
    out$fpr <- with(out, fp / (fp + tn))
    out$tnr <- 1 - out$fpr
    out$fnr <- 1 - out$tpr
    out$tot <- nrow(data)
    out$prevalence <- mean(data[[yn]] == 1)
    out$c <- cr
    out$ppv <- with(out, tp / (tp + fp))
    out$npv <- with(out, tn / (tn + fn))
    out
  })

  do.call(rbind, confusion)
}
# apply the contrast function f (e.g., contr.treatment) to each
# categorical variable of a dataframe
set_contrasts <- function(data, f){
    setcon <- function(col, f){
        if(is.character(col)){
            col <- as.factor(col)
            contrasts(col) <- f(nlevels(col))
        }else if(is.factor(col)){
            contrasts(col) <- f(nlevels(col))
        }
        return(col)
    }
    dplyr::bind_cols(lapply(data, setcon, f))
}

# wrapper of contr.sum for setting sum to 0 contrast
# as -0.5 0.5 instead of -1 1
contr.sum2 <- function(n){
    contr.sum(n)/n
}

# internal function to extract the underlying
# numerical variable from a factor with contrasts
.num_from_contrast <- function(data){
    fct <- data[, sapply(data, is.factor)]
    fct_num <- lapply(fct, function(col) contrasts(col)[col])
    names(fct_num) <- paste0(names(fct_num), "_c")
    cbind(data, fct_num)
}

# generate counts data given the mean (mu) and the desired
# mean-variance ratio (vmr). If vmr = 1 data will be generated
# from a poisson distribution (mean = var = lambda) otherwise data
# are generated from a negative binomial distribution
# an useful message is printed along returning the vector of values

rnb <- function(n, mu, vmr = NULL, theta = NULL, message = FALSE){
  if(is.null(theta) & is.null(vmr)){
    stop("theta or vmr need to be specified!")
  }
  if(!is.null(vmr)){
    if(vmr == 1){
      msg <- sprintf("y ~ Poisson(mu = %2.f), vmr = %.2f", mu, vmr)
      y <- rpois(n, mu)
    } else{
      res <- theta_from_vmr(mu, vmr)
      y <- MASS::rnegbin(n, mu, res$theta)
      msg <- sprintf("y ~ NegBin(mu = %2.f, theta = %.2f), var = %.2f, vmr = %.2f", mu, res$theta, res$v, vmr)
    }
  } else{
    res <- vmr_from_theta(mu, theta)
    y <- MASS::rnegbin(n, mu, res$theta)
    msg <- sprintf("y ~ NegBin(mu = %2.f, theta = %.2f), var = %.2f, vmr = %.2f", mu, theta, res$v, res$vmr)
  }
  if(message) message(msg)
  return(y)
}

theta_from_vmr <- function(mu, vmr){
    # vmr = v / m
    v <- mu * vmr
    # v = mu + mu^2/phi
    theta <- -(mu^2/(mu - v))
    if(vmr == 1){
      warning("when vmr = 1, theta = -Inf")
    }
    list(theta = theta, v = v, vmr = vmr)
}

vmr_from_theta <- function(mu, theta){
  v <- mu + mu^2/theta
  vmr <- v/mu
  list(theta = theta, v = v, vmr = vmr)
}

# plotting different type of residuals for (g)lm models

# plot_resid <- function(fit,
#                        x = NULL,
#                        type = c("response",
#                                 "pearson",
#                                 "deviance",
#                                 "student"),
#                        standard = FALSE){
#     require(ggplot2)
#     type <- match.arg(type)
#
#     if(type %in% c("deviance", "pearson") & standard){
#         resids <- rstandard(fit, type = type)
#         yl <- paste(type, "standardized residuals")
#     }else if(type == "student"){
#         if(standard) warning("The 'standard' argument is not relevant for studentized residuals")
#         resids <- rstudent(fit)
#         yl <- "studentized residuals"
#     }else{
#         resids <- residuals(fit, type = type)
#         yl <- paste(type, "residuals")
#     }
#
#     res <- data.frame(
#         residuals = resids
#     )
#
#     if(!is.null(x)){
#         res$x <- fit$data[[x]]
#         xl <- x
#     }else{
#         res$x <- fitted(fit)
#         xl <- "fitted(fit)"
#     }
#
#     ggplot(res,
#            aes(x = x, y = residuals)) +
#         geom_hline(yintercept = 0, linetype = "dashed", col = "red") +
#         geom_point(size = 2.5,
#                    alpha = 0.7) +
#         xlab(xl) +
#         ylab(yl) +
#         geom_smooth(formula = y ~ x,
#                     method = "loess",
#                     se = FALSE)
# }


add_numeric_contrast <- function(data, append = TRUE){
    data_fcts <- data[, sapply(data, is.factor)]
    cs <- data.frame(model.matrix(~., data = data_fcts)[, -1])
    lvs <- lapply(data_fcts, levels)
    refs <- sapply(lvs, function(x) x[1])
    lvs <- lapply(lvs, function(x) x[2:length(x)])
    cs_names <- mapply(function(x, y) paste0(x, "_vs_", y), lvs, refs)
    names(cs) <- unlist(cs_names)

    if(append){
        cbind(data, cs)
    }else{
        cs
    }
}

var_from_theta <- function(mu, theta){
    mu + mu^2/theta
}

count_na <- function(data){
    sapply(data, function(x) sum(is.na(x)))
}

linpred <- function(data, formula, coefs, family){
  X <- model.matrix(formula, data = dat)
  data$lp <- as.numeric(X %*% coefs)
  data$lp_inv <- family$linkinv(lp)
  return(data)
}

contr <- function(x){
  if(!is.factor(x)){
    stop("x need to be a factor!")
  }
  contrasts(x)[x]
}

mtidy <- function(x, conf.level = 0.95){
  sx <- broom::tidy(x, conf.int = TRUE, conf.level = conf.level)
  names(sx) <- c("term", "beta", "se", "stat", "p", "ci.low", "ci.high")
  return(sx)
}

sim_sdt <- function(nt, d, c = 0, ps = 0.5, sr = 1){
  ns <- floor(ps * nt)
  nn <- nt - ns
  is_signal <- rep(c(0, 1), nn, ns)
  x <- ifelse(is_signal == 1, rnorm(ns, d/2, sr), rnorm(nn, -d/2, 1))
  say_signal <- ifelse(x > c, 1, 0)
  is_signal <- factor(is_signal, levels = c(1,0))
  say_signal <- factor(say_signal, levels = c(1,0))
  data.frame(say_signal, is_signal, x)
}

sdt <- function(is_signal, say_signal = NULL, x = NULL, c = NULL){

  if(is.null(say_signal)){
    say_signal <- lapply(c, function(ci) ifelse(x > ci, 1, 0))
  }else{
    say_signal <- list(say_signal)
    c <- NA
  }

  hit <- miss <- fa <- cr <- rep(0, length(c))

  for(i in 1:length(c)){
    hit[i] <- sum(is_signal == 1 & say_signal[[i]] == 1)
    miss[i] <- sum(is_signal == 1 & say_signal[[i]] == 0)
    fa[i] <- sum(is_signal == 0 & say_signal[[i]] == 1)
    cr[i] <- sum(is_signal == 0 & say_signal[[i]] == 0)
  }

  list(hit = hit,
       miss = miss,
       fa = fa,
       cr = cr,
       phit = hit / (hit + miss),
       pfa = fa / (fa + cr),
       pmiss = miss / (miss + hit),
       pcr = cr / (cr + fa),
       c = c)
}
