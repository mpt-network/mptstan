
parse_model_df <- function(model_df) {
  model_trees <- split(x = model_df,
                       f = factor(model_df$Tree, levels = unique(model_df$Tree)))
  model_branches <- lapply(
    X = model_trees,
    FUN =
      function(x)
        split(x, f = factor(x$Category,
                            levels = unique(x$Category)))
  )
  model_parsed <- vector("list", length(model_trees))
  for (t in seq_along(model_branches)) {
    for (c in seq_along(model_branches[[t]])) {
      model_parsed[[t]][c] <-  parse(text = paste(model_branches[[t]][[c]][["Equation"]], collapse = " + "))
    }
  }
  names(model_parsed) <- names(model_trees)
  attr(model_parsed, "cat_map") <- lapply(model_branches, names)
  model_parsed
}

find.MPT.params <- function(model_list) {
	tmp <- lapply(model_list,all.vars)
	return(unique(sort(unique(unlist(tmp)))))
}


#' @importFrom stats runif
check.MPT.probabilities <- function(model_list){
	tmp.env <- new.env()
	temp.param.names <- find.MPT.params(model_list)
	temp.param.val <- runif(length(temp.param.names))
	temp.branch <- vapply(model_list,length, NA_integer_)
	prob <- rep(NA_real_,length(temp.branch))
	for (i in 1:length(temp.param.val)) {
		assign(temp.param.names[i],temp.param.val[i], envir = tmp.env)
	}
	tmp_res <- vector("list", length(model_list))
	for (i in seq_along(model_list)) {
	  tmp_res[[i]] <- vapply(model_list[[i]], FUN = eval, FUN.VALUE = NA_real_, envir = tmp.env)
	  names(tmp_res[[i]]) <- attr(model_list, "cat_map")[[i]]
	}
	prob <- vapply(tmp_res, sum, NA_real_)
	prob <- round(prob,digits=6)
	return(prob)
}

