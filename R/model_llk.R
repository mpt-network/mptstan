make_llk_function <- function(model_df) {
  model_list <- parse_model_df(model_df)
  model_vars <- find.MPT.params(model_list)
  if (length(model_vars) > 1) {
    out <- paste0("real mpt_lpmf(int y, real mu",
                  paste0(", real ", model_vars[-1], "_tmp", collapse = ""),
                  ", int item_type, int n_cat) {")
  } else {
    out <- paste0("real mpt_lpmf(int y, real mu, int item_type, int n_cat) {")
  }
  out <- paste0(out, "\n  vector[n_cat] prob;")
  out <- paste0(out, "\n  real ", model_vars[1]," = log(mu);")
  if (length(model_vars) > 1) {
    for (i in seq_len(length(model_vars)-1)) {
      out <- paste0(out, "\n  real ", model_vars[i+1],
                    " = log(", model_vars[i+1], "_tmp)", ";")
    }
  }

  model_trees <- split(x = model_df,
                       f = factor(model_df$Tree, levels = unique(model_df$Tree)))
  model_branches <- lapply(
    X = model_trees,
    FUN =
      function(x)
        split(x, f = factor(x$Category,
                            levels = unique(x$Category)))
  )
  #model_out <- vector("character", length(unique(model_df$Category)))
  model_out <- vector("character", length(unlist(model_list)) + length(model_trees))
  i <- 1
  for (t in seq_along(model_branches)) {
    if (t == 1) {
    model_out[1] <- "  if (item_type == 1) {"
    } else {
      model_out[i] <- paste0("  } else if (item_type == ", t, ") {")
    }
    i <- i+1
    for (c in seq_along(model_branches[[t]])) {
      tmp_branch <- strsplit(model_branches[[t]][[c]][["Equation"]], split = "\\*") |>
        lapply(combine_terms, model_vars = model_vars) |>
        unlist()
      if (length(tmp_branch) > 1) {
        tmp_branch <- paste0("log_sum_exp({", paste(tmp_branch, collapse = ", "), "})")
      }
      model_out[i] <- paste0("    prob[", c, "] = ", tmp_branch, ";")
      i <- i + 1
      # if (c == max(seq_along(model_branches[[t]]))) {
      #   model_out[i] <-
      #     paste0('    print("p(", item_type, ") = ", exp(prob), "\\n")', ';')
      #   i <- i + 1
      # }
    }
  }
  out <- paste0(out, "\n",
                paste(model_out, collapse = "\n"),
                "\n  }\n",
                "  return(categorical_logit_lpmf(y | prob ));\n",
                "}")
  #cat(out)
  return(out)
}

combine_terms <- function(x, model_vars) {
  model_vars_regex <- paste0("(", paste(model_vars, collapse = "|"),")")
  out <- x
  sel <- grepl(paste0("(1-", model_vars_regex, ")"), x)
  vars <- regmatches(out, regexpr(pattern = model_vars_regex, out))
  out[sel] <- paste0("log1m_exp(",vars[sel],")")
  paste(out, collapse = " + ")
}
