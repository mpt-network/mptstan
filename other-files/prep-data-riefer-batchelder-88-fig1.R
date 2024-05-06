data(rb.fig1.data, package = "MPTinR")
drb1 <- rb.fig1.data
colnames(drb1) <- c("BC", "BnC", "nBC", "nBnC")
rownames(drb1) <- c(paste0("test1.", 1:3),
                    paste0("test2.", 1:3))

library("tidyverse")
drb_fig1 <- drb1 %>%
  as.data.frame() %>%
  rownames_to_column(var = "test") %>%
  pivot_longer(cols = BC:nBnC) %>%
  mutate(resp_long = map2(name, value, ~rep(.x, .y))) %>%
  unnest(resp_long) %>%
  as.data.frame()
save(drb_fig1, file = "tests/testthat/riefer-batchelder-1988-fig1-data.rda")
