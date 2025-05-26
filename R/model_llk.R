make_llk_function <- function(model_df, log_p, data_format) {
  model_list <- parse_model_df(model_df)
  model_vars <- find.MPT.params(model_list)
  if (log_p) par_names <- paste0(", real ", model_vars[-1], "_tmp", collapse = "")
  else par_names <- paste0(", real ", model_vars[-1], collapse = "")
  lpmf_name <- "mpt"
  if(log_p) lpmf_name <- paste0(lpmf_name, "_log")

  if (data_format == "long") {
    if (length(model_vars) > 1) {
      out <- paste0("real ", lpmf_name, "_lpmf(int y, real mu",
                    par_names,
                    ", int item_type, int n_cat) {")
    } else {
      out <- paste0("real ", lpmf_name, "_lpmf(int y, real mu, int item_type, int n_cat) {")
    }
  } else {
    lpmf_name <- paste0(lpmf_name, "_agg")
    all_cats <- unique(unlist(lapply(attr(model_list, "cat_map"), function(x) return(paste("cat", x, sep = "_")))))

    # add all categories except for the first one (that is automatically added with y)
    out <- paste0("real ", lpmf_name, "_lpmf(int y, real mu", par_names,
                  ", int item_type, int n_cat, ",
                  paste0("int ", all_cats[2:(length(all_cats) - 1)], ", ", collapse = ""),
                  paste0("int ", all_cats[length(all_cats)]), ") {")
  }

  out <- paste0(out, "\n  vector[n_cat] prob;")

  if (log_p) {
    out <- paste0(out, "\n  real ", model_vars[1]," = log(mu);")
    if (length(model_vars) > 1) {
      for (i in seq_len(length(model_vars)-1)) {
        out <- paste0(out, "\n  real ", model_vars[i+1],
                      " = log(", model_vars[i+1], "_tmp)", ";")
      }
    }
  } else out <- paste0(out, "\n  real ", model_vars[1]," = mu;")
  #else {
  #  out <- paste0(out, "\n  real ", model_vars[1]," = mu;")
  #  if (length(model_vars) > 1) {
  #    for (i in seq_len(length(model_vars)-1)) {
  #      out <- paste0(out, "\n  real ", model_vars[i+1],
  #                    " = ", model_vars[i+1], "_tmp", ";")
  #   }
  #  }
  #}

  if (data_format == "wide") out <- paste0(out, "\n  int ", all_cats[1], " = y;")

  #### Model Probabilities
  model_trees <- split(x = model_df,
                       f = factor(model_df$Tree, levels = unique(model_df$Tree)))
  model_branches <- lapply(
    X = model_trees,
    FUN =
      function(x)
        split(x, f = factor(x$Category,
                            levels = unique(x$Category)))
  )
  if (data_format == "long") {
    model_out <- vector("character", length(unlist(model_list)) +
                          length(model_trees))
  } else model_out <- vector("character", length(unlist(model_list)) +
                               length(model_trees) * 2)
  i <- 1
  for (t in seq_along(model_branches)) {
    if (t == 1) {
      model_out[1] <- "  if (item_type == 1) {"
    } else {
      model_out[i] <- paste0("  } else if (item_type == ", t, ") {")
    }
    i <- i + 1
    # add the correct data for aggregated fitting
    if (data_format == "wide") {
      cats_tmp <- paste0("cat_", attr(model_list, "cat_map")[[names(model_branches)[t]]])
      model_out[i] <- paste0("    array[n_cat] int freq = {", paste0(cats_tmp, collapse = ", "), "};")
      i <- i + 1
    }

    for (c in seq_along(model_branches[[t]])) {
      tmp_branch <- strsplit(model_branches[[t]][[c]][["Equation"]], split = "\\*") |>
        lapply(combine_terms, model_vars = model_vars, log_p = log_p) |>
        unlist()
      if (length(tmp_branch) > 1) {
        if (log_p) tmp_branch <- paste0("log_sum_exp({", paste(tmp_branch, collapse = ", "), "})")
        else tmp_branch <- paste(tmp_branch, collapse = " + ")
      }

      model_out[i] <- paste0("    prob[", c, "] = ", tmp_branch, ";")
      i <- i + 1
      # if (c == max(seq_along(model_branches[[t]]))) {
      #   model_out[i] <-
      #     paste0('    print("p(", item_type, ") = ", exp(prob), "\\n")', ';')
      #   i <- i + 1
      # }
    }
    # for aggregated data, add the lpmf call after every branch because freq
    # is only defined in that block
    if (data_format == "wide") {
      lpmf_str <- "multinomial"
      if (log_p) lmpf_str <- paste0(lpmf_str, "_logit")
      model_out[i] <- paste0("    return(", lpmf_str, "_lpmf(freq | prob));")
      i <- i + 1
    }
  }

  #### Put everything together
  if (data_format == "wide") {
    # return(0) statement because Stan expects a return value for every possible
    # case
    out <- paste0(out, "\n", paste(model_out, collapse = "\n"),
                  "\n  }\n  return(0);\n}")
  } else {
    lpmf_str <- ifelse(log_p, "categorical_logit", "categorical")
    out <- paste0(out, "\n",
                  paste(model_out, collapse = "\n"),
                  "\n  }\n",
                  "  return(", lpmf_str, "_lpmf(y | prob ));\n",
                  "}")
  }
  return(out)
}

combine_terms <- function(x, model_vars, log_p) {
  if (log_p) {
    model_vars_regex <- paste0("(", paste(model_vars, collapse = "|"),")")
    out <- x
    sel <- grepl(paste0("(1-", model_vars_regex, ")"), x)
    vars <- regmatches(out, regexpr(pattern = model_vars_regex, out))
    out[sel] <- paste0("log1m_exp(",vars[sel],")")
    return(paste(out, collapse = " + "))
  } else return(paste(x, collapse = " * "))
}
