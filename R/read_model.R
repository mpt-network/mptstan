#' Create MPT model object
#'
#' Create an MPT model object from either a model file or model string. The
#' model can be in the popular `eqn` format commonly used by MPT software or the
#' `easy` format introduced by `MPTinR`.
#'
#' @param file file name of model file.
#' @param type model type. In the `"easy"` format, each line is assumed to
#'   correspond to the full equation for one response category. In the `".eqn"`
#'   or `"eqn2"` format, each branch or path through the tree is given in one
#'   line (`eqn` is the default format for many other MPT tools). The default
#'   behaviour is to assume the model is in the `"easy"` format unless `file`
#'   ends with `.eqn`.
#' @param restrictions `list` of parameter restrictions.
#' @param trees For the `"easy"` format, the optional vector of tree names.
#'   Ignored otherwise.
#' @param categories For the `"easy"` format, the optional vector of categories.
#'   Ignored otherwise.
#' @param text alternative specification of model via text instead of a file.
#'   Ignored is `file` is not missing
#' @param link character specifying the link function for transforming from
#'   unconstrained space to MPT model parameter (i.e., 0 to 1) space. Default is
#'   `"probit"`.
#'
#' @returns An object of class `mpt_model` which is a list with the following
#'   elements:
#' 1. `model_df`: A `data.frame` representation of the model (basically the
#'    `.eqn` format representation).
#' 2. `list`: The model in `expression` format, where each tree is one list.
#' 3. `check`: Vector of the summed probabilities in each tree (should each be 1
#'    for a well constructed model).
#' 4. `ns`: Overview of model in numerical form.
#' 5. `parameters`: Vector with names of model parameters.
#' 6. `family`: `brms` custom family specifying the model.
#' 7. `brms_llk`: character string with the MPT model likelihood in `Stan`
#'     format.
#' @example examples/examples_make_mpt.R
#'
#' @export
make_mpt <- function(file, type = c("easy", "eqn", "eqn2"),
                     restrictions,
                     trees, categories,
                     text,
                     link = "probit") {
  model_df <- read_mpt(file = file, text = text, type = type,
                       trees = trees, categories = categories)
  if (!missing(restrictions)) {
    restrictions <- read.MPT.restrictions(restrictions)
    model_df <- apply.restrictions(model_df, restrictions)
  }
  model_list <- parse_model_df(model_df)
  parameters <- find.MPT.params(model_list)
  new_parameters <- ckeck_parameters(parameters)
  if (!is.null(new_parameters)) {
    model_df <- apply.restrictions(model_df, new_parameters$restrictions)
    parameters <- new_parameters$new_pars
  }
  model_list <- parse_model_df(model_df)
  mod_check <- check.MPT.probabilities(model_list = model_list)
  mod_code <- make_llk_function(model_df)
  model_ns <- c(
    trees = length(model_list),
    categories = length(unlist(model_list)),
    ind_categories = NA_integer_,
    parameters = length(parameters)
  )
  model_ns["ind_categories"] <- model_ns["categories"] - model_ns["trees"]

  model_names <- list(
    trees = unique(model_df$Tree)
  )
  model_names$categories <- lapply(
    X = split(model_df$Category, f = factor(model_df$Tree, levels = model_names$trees)),
    FUN = unique)
  mpt_family <- brms::custom_family(
    name = "mpt",
    links = link,
    dpars = c("mu", parameters[-1]),
    lb = rep(0, model_ns["parameters"]),
    ub = rep(1, model_ns["parameters"]),
    type = "int",
    vars = c("item_type[n]", "n_cat[n]"),
    log_lik = make_log_lik(
      model_list = model_list,
      model_names = model_names,
      parameters = parameters),
    posterior_predict = make_posterior_predict(
      model_list = model_list,
      model_names = model_names,
      parameters = parameters),
    posterior_epred = make_posterior_epred(
      model_list = model_list,
      model_names = model_names,
      parameters = parameters))
  out <- list(
    df = model_df,
    list = model_list,
    check = mod_check,
    ns = model_ns,
    parameters = parameters,
    names = model_names,
    family = mpt_family,
    brms_llk = make_llk_function(model_df)
  )
  class(out) <- "mpt_model"
  return(out)
}

#' @export
print.mpt_model <- function(x, eqn = FALSE, ...) {
  cat("\nMPT model with ", x$ns["ind_categories"], " independent categories (from ",
      x$ns["trees"], " trees)", # x$ns["categories"], ")",
      " and ", x$ns["parameters"], " parameters:\n  ",
      sep = "")
  cat(x$parameters, sep = ", ")
  cat("\n\n")
  #browser()
  vars_per_tree <- lapply(x$list, all.vars)
  for (i in seq_len(x$ns["trees"])) {
    cat("Tree ", i, ": ", x$names$trees[i], "\n", sep = "")
    cat("  Categories:", paste(x$names$categories[[i]], collapse = ", "), "\n")
    cat("  Parameters:",
        paste(vars_per_tree[x$names$trees[i]][[1]], collapse = ", "))
    cat("\n")

  }
  if (eqn) {
    cat("\nModel EQN:\n")
    print(x$df)
  }
  cat("\n")
  if (!all(x$check == 1)) {
    warning("Probabilities do not sum to one for all trees: ",
            paste(x$check, collapse = ", "),
            call. = FALSE)
  }
  invisible(x)
}

ckeck_parameters <- function(pars) {
  ends_in_number <- stringr::str_detect(pars, "\\d$")
  if (any(ends_in_number)) {
    warning("parameter names ending with a number amended with 'x'",
            call. = FALSE)
    new_pars <- stringr::str_replace(pars, "(\\d)$", "\\1x")
    #new_pars <- make.unique(new_pars, sep = "_")
    out <- list(
      new_pars = new_pars,
      restrictions = read.MPT.restrictions(
        tmp.restrictions = as.list(paste(pars[ends_in_number],
                                         "=",
                                         new_pars[ends_in_number]))
      )
    )
  } else {
    out <- NULL
  }
  return(out)
}

read_mpt <- function(file, text,
                       trees, categories,
                       type = c("easy", "eqn", "eqn2")) {
  if (!missing(file) && isTRUE(all.equal(type, c("easy", "eqn", "eqn2"))) &&
      (grepl("\\.eqn$", file) || grepl("\\.EQN$", file))) {
    type <- "eqn"
    message("model type auto-detected as 'eqn'")
  } else {
    type <- match.arg(type)
  }
  if (type == "eqn2") {
    ignore_first <- FALSE
    type <- "eqn"
  } else {
    ignore_first <- TRUE
  }
  model_df <- switch(type,
                  easy = .read.MPT.model(file = file, text = text, trees = trees,
                                         categories = categories),
                  eqn = .read.EQN.model(file = file, text = text,
                                        ignore_first = ignore_first)
  )
  # eqn_extra <- switch(type,
  #                 easy = NULL,
  #                 eqn = .get.EQN.model.order(file = file, text = text,
  #                                       ignore_first = ignore_first)
  # )
  #list(model = model, order = eqn_extra)
  return(model_df)
}

.read.MPT.model <- function(file, text, trees, categories) {
	if (!missing(file)) {
	  whole <- readLines(con = file)
	} else if (!missing(text)) {
	  whole <- stringr::str_split_1(text, "\\n")
	} else {
	  stop("no model specified.", call. = FALSE)
	}
	whole <- gsub("#.*", "", whole)
	whole <- stringr::str_replace_all(whole, " ", "")
	max_rows <- (max(stringr::str_count(whole, "\\+"))+1)*length(whole)
	model_df <- data.frame(Tree = rep(NA_character_, max_rows),
	                       Category = NA_character_, Equation = NA_character_)
	if (missing(trees)) {
	  trees <- paste0("t", seq_along(whole))
	}
	if (missing(categories)) {
	  categories <- paste0("c", seq_along(whole))
	}

	row <- 0
	c_tree <- 1
	c_cat <- 1
	s.flag <- FALSE
	for (c1 in seq_along(whole)) {
		if (!(grepl("^[[:space:]]*$", whole[c1]))) {
			s.flag <- TRUE
			path_split <- stringr::str_split_1(whole[c1], stringr::fixed("+"))
			path_seq <- seq_along(path_split)
			model_df[["Tree"]][path_seq + row] <- trees[c_tree]
			model_df[["Category"]][path_seq + row] <- categories[c_cat]
			model_df[path_seq + row, "Equation"] <- path_split
			#model[[c2]][c3] <- parse(text = whole[c1])[1]
			c_cat <- c_cat + 1
			row <- row + max(path_seq)
			fin <- row
		}
		else {
			if (s.flag == TRUE) c_tree <- c_tree + 1
			s.flag <- FALSE
		}
	}
	return (model_df[1:fin,])
}

#' @importFrom utils read.table
.read.EQN.model <- function(file, text, ignore_first) {
	if (!ignore_first) {
	  nrows <- read.table(file = file, nrows = 1, text = text)[1,1]
	} else {
	  nrows <- -1
	}
	tmp.in <- read.table(file = file, skip = 1, stringsAsFactors = FALSE,
	                     nrows = nrows, text = text)
	colnames(tmp.in) <- c("Tree", "Category", "Equation")
	tmp.in
}

#rules for restriction files:
# 1. Use your brain!
# 2. At first all inequality restrictions are applied.
# 3. Then all equality restrictions.
# 4. Finally all fixed parameters are handled.
# If your set of restrictions does not make sens given this procedure, change your set of restrictions.


read.MPT.restrictions <- function(tmp.restrictions) {
  #min.restriction <- c(0, 0.001)
  #max.restriction <- 0.99999

  if (!is.list(tmp.restrictions))
    stop("restrictions must be a list")

  if (!all(vapply(tmp.restrictions, class, "") == "character"))
    stop("The restrictions can only be of class character")

  no.white.restrictions <-
    lapply(tmp.restrictions,
           gsub,
           pattern = " ",
           replacement = "")
  if (sum(grepl("[/\\+\\*\\!-]", unlist(tmp.restrictions))))
    stop(
      "Error getting Restrictions: Non supported operators (+, -, *, /, !) found in restriction file."
    )
  if (any(
    vapply(no.white.restrictions, grepl, TRUE, pattern = "=.*<"),
    vapply(no.white.restrictions, grepl, TRUE, pattern = "<.*="),
    vapply(no.white.restrictions, grepl, TRUE, pattern = "=.*>"),
    vapply(no.white.restrictions, grepl, TRUE, pattern = ">.*="),
    vapply(no.white.restrictions, grepl, TRUE, pattern = ">.*<"),
    vapply(no.white.restrictions, grepl, TRUE, pattern = "<.*>")
  ))
    stop(
      "Error getting restrictions: A line contains more than one of the following operators: =, <, >!"
    )

  #recover()

  fixed.restrictions = list()
  equality.restrictions = list()
  inequality.restrictions = list()

  c.x.all <- 1
  c.restr <- 1
  #for (c.restr in 1:length(no.white.restrictions)) {
  regexp.number <- "^[[:digit:]]*\\.?[[:digit:]]+$"
  while (c.restr  < (length(no.white.restrictions) + 1)) {
    tmp.restr <-
      strsplit(no.white.restrictions[[c.restr]], "[=><]")[[1]]
    if (any(grepl(regexp.number, tmp.restr[-length(tmp.restr)])))
      stop(
        paste(
          "Numerical constant not the rightmost element in restriction:",
          tmp.restrictions[[c.restr]]
        )
      )
    if (grepl("=", no.white.restrictions[[c.restr]])) {
      if (grepl(regexp.number, tmp.restr[length(tmp.restr)])) {
        if (as.numeric(tmp.restr[length(tmp.restr)]) > 1 |
            as.numeric(tmp.restr[length(tmp.restr)]) < 0)
          stop("fixed restriction / numerical constant is not inside [0,1]")
        fixed.restrictions[[length(fixed.restrictions) + 1]] <-
          list(parameter = tmp.restr[length(tmp.restr) - 1],
               value = tmp.restr[length(tmp.restr)])
        tmp.restr <- tmp.restr[-length(tmp.restr)]
      }
      if (length(tmp.restr) > 1) {
        for (c in 1:(length(tmp.restr) - 1)) {
          equality.restrictions[[length(equality.restrictions) + 1]] <-
            list(parameter = tmp.restr[c], value = tmp.restr[length(tmp.restr)])
        }
      }
    }
    if (grepl("<", no.white.restrictions[[c.restr]])) {
      if (grepl(regexp.number, tmp.restr[length(tmp.restr)])) {
        no.white.restrictions[[length(no.white.restrictions) + 1]] <-
          paste("hank", c.x.all, "=", tmp.restr[length(tmp.restr)], sep = "")
        tmp.restr[length(tmp.restr)] <-
          paste("hank", c.x.all, sep = "")
        c.x.all <- c.x.all + 1
      }
      for (c in 1:(length(tmp.restr) - 1)) {
        #the replacement is done via "method a", Knapp & Batcheler (2004)
        inequality.restrictions[[length(inequality.restrictions) + 1]] <-
          list(
            parameter = tmp.restr[c],
            exchange.inverse = list(
              paste(tmp.restr[c + 1], "*(1-hank", c.x.all, ")", sep = ""),
              paste("(1-", tmp.restr[c + 1], ")", sep = "")
            ),
            exchange.parameter = list(paste(
              tmp.restr[c + 1], "*hank", c.x.all, sep = ""
            )),
            compute.as = parse(text = paste(
              "hank", c.x.all, "*", tmp.restr[c + 1], sep = ""
            ))
          )
        c.x.all <- c.x.all + 1
      }
    }
    if (grepl(">", no.white.restrictions[[c.restr]])) {
      if (grepl(regexp.number, tmp.restr[length(tmp.restr)])) {
        no.white.restrictions[[length(no.white.restrictions) + 1]] <-
          paste("hank", c.x.all, "=", tmp.restr[length(tmp.restr)], sep = "")
        tmp.restr[length(tmp.restr)] <-
          paste("hank", c.x.all, sep = "")
        c.x.all <- c.x.all + 1
      }
      for (c in 1:(length(tmp.restr) - 1)) {
        #the replacement is done via "method b", Knapp & Batcheler (2004)
        inequality.restrictions[[length(inequality.restrictions) + 1]] <-
          list(
            parameter = tmp.restr[c],
            exchange.inverse = list(
              paste("(1-", tmp.restr[c + 1], ")*(1-hank", c.x.all, ")",  sep = "")
            ),
            exchange.parameter = list(
              tmp.restr[c + 1],
              paste("(1-", tmp.restr[c + 1], ")*hank", c.x.all, sep = "")
            ),
            compute.as = parse(text = paste(
              "hank", c.x.all, "*", tmp.restr[c + 1], sep = ""
            ))
          )
        c.x.all <- c.x.all + 1
      }
    }
    c.restr <- c.restr + 1
  }
  list(
    fixed = fixed.restrictions,
    equality = equality.restrictions,
    inequality = inequality.restrictions,
    raw = tmp.restrictions
  )
}

apply.restrictions <- function(model_df, restriction_list) {

	# if (length(inequality.restrictions(model)) != 0) {
	# 	for (inequality in inequality.restrictions(model)) {
	# 		inverse <- paste("\\(1-", parameter(inequality), "\\)", sep = "")
	# 		while (any(grepl(inverse, tmp.model[,"branches"]))) {
	# 			if (length(exchange.inverse(inequality)) == 1) {
	# 				tmp.model[,"branches"] <- gsub(inverse, exchange.inverse(inequality)[[1]], tmp.model[,"branches"])
	# 			} else {
	# 				for (branch in 1:dim(tmp.model)[1]) {
	# 					if (grepl(inverse, tmp.model[branch,"branches"])) {
	# 						for (exchange in exchange.inverse(inequality)) {
	# 							tmp1 <- tmp.model[branch,]
	# 							tmp1[,"branches"] <- sub(inverse, exchange, tmp1[,"branches"])
	# 							if (!exists("new.model")) new.model <- tmp1
	# 							else new.model <- rbind(new.model, tmp1)
	# 						}
	# 					} else {
	# 						if (!exists("new.model")) new.model <- tmp.model[branch,]
	# 						else new.model <- rbind(new.model, tmp.model[branch,])
	# 					}
	# 				}
	# 				tmp.model <- new.model
	# 				rm(new.model)
	# 			}
	# 		}
	# 		parameter <- parameter(inequality)
	# 		while (any(grepl(parameter, tmp.model[,"branches"]))) {
	# 			if (length(exchange.parameter(inequality)) == 1) {
	# 				tmp.model[,"branches"] <- gsub(parameter, exchange.parameter(inequality)[[1]], tmp.model[,"branches"])
	# 			} else {
	# 				for (branch in 1:dim(tmp.model)[1]) {
	# 					if (grepl(parameter, tmp.model[branch,"branches"])) {
	# 						for (exchange in exchange.parameter(inequality)) {
	# 							tmp1 <- tmp.model[branch,]
	# 							tmp1[,"branches"] <- sub(parameter, exchange, tmp1[,"branches"])
	# 							if (!exists("new.model")) new.model <- tmp1
	# 							else new.model <- rbind(new.model, tmp1)
	# 						}
	# 					} else {
	# 						if (!exists("new.model")) new.model <- tmp.model[branch,]
	# 						else new.model <- rbind(new.model, tmp.model[branch,])
	# 					}
	# 				}
	# 				tmp.model <- new.model
	# 				rm(new.model)
	# 			}
	# 		}
	# 	}
	# 	row.names(tmp.model) <- NULL
	# }
  list_fixed_equality <- c(restriction_list$fixed, restriction_list$equality)
	if (length(list_fixed_equality) > 0) {
		for (i in seq_len(length(list_fixed_equality))) {
		  model_df[,"Equation"] <- stringr::str_replace_all(
		    string = model_df[,"Equation"],
		    pattern = paste0("\\b", list_fixed_equality[[i]]$parameter, "\\b"),
		    replacement = list_fixed_equality[[i]]$value
		  )
		}
	}
	model_df
}


