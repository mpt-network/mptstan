#' emmeans()-wrapper for MPT models
#'
#' A convenient interface to [emmeans::emmeans()] for all MPT model parameters.
#' Per default, results for all MPT model parameters are combined into one
#' `data.frame`, this can be disabled using the `simplify` argument.
#'
#' @param object fitted MPT model object returned by [mpt()]
#' @param specs same as for [emmeans::emmeans()]
#' @param by same as for [emmeans::emmeans()]
#' @param type same as for [emmeans::summary.emmGrid()]. Default for MPT models
#'   is `"response"`
#' @param ... further arguments passed to [emmeans::emmeans()] or corresponding
#'   methods
#' @param simplify logical value indicating whether the different results should
#'   be compiled into one `data.frame` (`TRUE`, the default) or returned as a
#'   `list`
#' @export
mpt_emmeans <- function(object, specs, by = NULL, type = "response", ...,
                        simplify = TRUE) {
  if (!requireNamespace("emmeans")) {
    stop("package emmeans is required for this functionality", call. = FALSE)
  }
  if (!inherits(object, "mpt_fit")) {
    stop("object needs to be of class 'mpt_fit'.", call. = FALSE)
  }
  all_pars <- object$mpt_formula$model$parameters
  all_emmeans <- vector("list", length(all_pars))
  names(all_emmeans) <- all_pars
  for (i in seq_len(length(all_pars))) {
    if (i == 1) {
      all_emmeans[[i]] <- tryCatch(emmeans::emmeans(object = object, specs = specs,
                                           by = by, type = type, ...), error = function(e) NA)
    } else {
      all_emmeans[[i]] <- tryCatch(emmeans::emmeans(object = object, specs = specs,
                                           by = by, type = type,
                                           dpar = all_pars[i], ...),
                                   error = function(e) NA)
    }
    if (simplify && inherits(all_emmeans[[i]], "emmGrid")) {
      all_emmeans[[i]] <- as.data.frame(all_emmeans[[i]])
      #orig_cols <- colnames(all_emmeans[[i]])
      all_emmeans[[i]]$parameter <- all_pars[i]
      #all_emmeans[[i]] <- all_emmeans[[i]][, c("parameter", orig_cols)]
    }
  }
  if (simplify) {
    out <- do.call("rbind", c(all_emmeans[!is.na(all_emmeans)], deparse.level = 0))
    tmp <- attributes(out)
    out <- out[c("parameter", colnames(out)[-length(colnames(out))])]
    tmp$names <- colnames(out)
    mostattributes(out) <- tmp
    attr(out, "digits") <- 3
    out$parameter <- factor(out$parameter, levels = all_pars)
    rownames(out) <- NULL
    return(out)
  } else {
    return(all_emmeans[!is.na(all_emmeans)])
  }
}
