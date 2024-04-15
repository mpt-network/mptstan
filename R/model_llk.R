make_llk_function <- function(model_df) {
  model_list <- parse_model_df(model_df)
  model_vars <- find.MPT.params(model_list)
  if (length(model_vars) > 1) {
    out <- paste0("real mpt_lpmf(int y, real mu",
                  paste(", real", model_vars[-1], collapse = ""),
                  ", int item_type, int n_cat) {")
  } else {
    out <- paste0("real mpt_lpmf(int y, real mu, int item_type, int n_cat) {")
  }
  out <- paste0(out, "\n  vector[n_cat] prob;")
  out <- paste0(out, "\n  real ", model_vars[1]," = mu;")

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
      model_out[i] <-  paste0(
        paste0("    prob[", c, "] = "),
        paste(model_branches[[t]][[c]][["Equation"]], collapse = " + "),
        ";"
      )
      i <- i + 1
    }
  }
  out <- paste0(out, "\n",
                paste(model_out, collapse = "\n"),
                "\n  }\n",
                "  return(categorical_lpmf(y | prob));\n",
                "}")
  #cat(out)
  return(out)
}
