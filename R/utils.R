#' get_weights
#'
#' @param x an rma.uni object from metafor
#'
#' @return a vector of weights
#' @importFrom metafor weights.rma.uni
#' @importFrom stats weights
#' @export
#'
get_weights <- function(x){
  # thanks to
  # https://github.com/cran/metafor/blob/master/R/weights.rma.mv.r
  # http://www.metafor-project.org/doku.php/tips:weights_in_rma.mv_models
  wi <- diag(weights(x, type = "matrix"))
  return(wi)
}

#' get_vi
#'
#' @param x an rma.uni object from metafor
#'
#' @return a vector of variances (inverse of weights)
#' @export
#'
get_vi <- function(x){
  wi <- get_weights(x)
  vi <- 1 / wi
  return(vi)
}


#' get_scores
#'
#' @param x an rma.uni object from metafor
#'
#' @return a vector of scores computed as \code{yi/sqrt(vi)}
#' @export
#'
get_scores <- function(x){
  vi <- get_vi(x)
  yi <- x$yi
  zi <- as.matrix(yi / sqrt(vi))
  colnames(zi) <- rownames(x$b)
  return(zi)
}

#' tidy_flip
#'
#' @param x the result of the \code{flip} function
#'
#' @return a dataframe
#' @export
#'
tidy_flip <- function(x){
  x <- data.frame(x@res)
  x$param <- rownames(x)
  rownames(x) <- NULL
  colnames(x) <- tolower(colnames(x))
  x[, c("param", "test", "stat", "tail", "p.value")]
}


#' prepare_multi
#'
#' @param fit x an rma.uni object from metafor
#' @param p_space the permutation space
#'
#' @return the \code{fit} object with the flipmeta class, the flip result, tspace and scores
#' @importFrom flip flip
#' @export
#'
prepare_multi <- function(fit, p_space){
  zi <- get_scores(fit)
  # TODO fix this for the situation with missing studies
  k <- fit$k # number of rows (study/observations)
  fit_flip <- flip::flip(zi[, 1], perms = p_space)
  fit$flip <- tidy_flip(fit_flip)
  fit$scores <- zi
  fit$Tspace <- fit_flip@permT
  fit <- add_class(fit, "flipmeta")
  return(fit)
}

#' add_class
#'
#' @param x an object
#' @param class character vector for the class to be added
#'
#' @return the object with the new class
#' @export
#'
add_class <- function(x, class){
  class(x) <- c(class, class(x))
  return(x)
}

#' @export
summary.joinmeta <- function(fitl){
  fitl <- lapply(fitl, tidy_rma)
  fitl <- do.call(rbind, fitl)
  rownames(fitl) <- NULL
  return(fitl)
}

# extract tidy version of the rma object
#' @export
tidy_rma <- function(fit){
  tfit <- data.frame(
    # rma
    est = fit$b, se = fit$se, z = fit$zval, p_meta = fit$pval,
    ci_lb = fit$ci.lb, ci_ub = fit$ci.ub,
    # flip
    t_flip = fit$flip$stat,
    tail_flip = fit$flip$tail,
    p_flip = fit$flip$p.value
  )
  tfit$b <- rownames(tfit)
  rownames(tfit) <- NULL
  tfit[, c("b", names(tfit)[-length(names(tfit))])]
}

#' multiverse
#'
#' @param fitl list of models
#' @param perms number of permutations
#' @importFrom flip make.signSpace
#' @export
#'
multiverse <- function(fitl, perms = 5000){
  # same permutation space for each
  k <- fitl[[1]]$k # TODO hard coded for now
  p_space <- flip::make.signSpace(k, perms = perms)
  fitl <- lapply(fitl, prepare_multi, p_space)
  fitl <- add_class(fitl, "joinmeta")
  if(is.null(names(fitl))){
    names(fitl) <- paste0("mod", 1:length(fitl))
  }
  return(fitl)
}


#' flip.rma
#'
#' @param fit a fitted \code{rma.uni} object
#' @param ... other arguments passed to \code{flip::flip()}
#'
#' @importFrom flip flip
#' @export
#'
flip.rma <- function(fit, ...){
  zi <- get_scores(fit)
  flip::flip(zi[, 1], ...)
}
