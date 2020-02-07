## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(SemNeT)

## ----Fig6, fig.cap = "Comparison of low (left) and high (right) openness to experience semantic networks based on the Fruchterman-Reingold algorithm", fig.align = 'center', fig.height = 5, fig.width = 8, warning = FALSE, eval = TRUE, echo = TRUE, message = FALSE----
# Visually compare networks
compare.nets(net.low, net.high,
             title = list("Low Openness", "High Openness"),
             config = "spring", weighted = FALSE)

## ----Compute network measures, echo = TRUE, eval = FALSE, comment = NA, warning = FALSE----
#  # Compute network measures
#  semnetmeas(net.low, meas = c("ASPL", "CC", "Q"), weighted = FALSE)
#  semnetmeas(net.high, meas = c("ASPL", "CC", "Q"), weighted = FALSE)

## ----tab7, echo = FALSE, eval = TRUE, comment = NA, warning = FALSE-----------
output <- matrix(c("Low", "3.25", "0.74", "0.64",
                   "High", "2.78", "0.76", "0.59"),
                 ncol = 4, byrow = TRUE)


htmlTable::htmlTable(output,
          header = c("Group", "ASPL", "CC", "Q"),
          caption = "Table 7. Group Network Measures")

## ----Compute random network analysis, echo = TRUE, eval = FALSE, comment = NA, warning = FALSE----
#  # Compute tests against random networks
#  rand.test <- randnet.test(net.low, net.high, iter = 1000, cores = 4)

## ----tab8, echo = FALSE, eval = TRUE, comment = NA, warning = FALSE-----------
output <- matrix(c("", "ASPL", "< .001", "3.04", "0.03",
                   "Low", "CC", "< .001", "0.04", "0.01",
                   "", "Q", "< .001", "0.38", "0.01",
                   "", "ASPL", "< .001", "3.03", "0.03",
                   "High", "CC", "< .001", "0.04", "0.01",
                   "", "Q", "< .001", "0.38", "0.01"),
                 ncol = 5, byrow = TRUE)


htmlTable::htmlTable(output,
          header = c("Group", "Measures", "p-values", "Random (M)", "Random (SD)"),
          caption = "Table 8. p-values of Low and High Openness to Experience Networks Against Random Networks")

## ----Arguments for partboot, echo = TRUE, eval = FALSE, comment = NA, warning = FALSE----
#  #Arguments for 'partboot' function
#  partboot(..., percent, sim, weighted = FALSE,
#           iter = 1000, cores)

## ----Partial bootstrap analysis, echo = TRUE, eval = FALSE, comment = NA, warning = FALSE----
#  # Compute partial bootstrap network analysis
#  ## 50% of nodes remaining in network
#  boot.fifty <- partboot(equate.low, equate.high,
#                         percent = .50, iter = 1000,
#                         sim = "cosine", cores = 4)
#  ## 60% of nodes remaining in network
#  boot.sixty <- partboot(equate.low, equate.high,
#                         percent = .60, iter = 1000,
#                         sim = "cosine", cores = 4)
#  ## 70% of nodes remaining in network
#  boot.seventy <- partboot(equate.low, equate.high,
#                           percent = .70, iter = 1000,
#                           sim = "cosine", cores = 4)
#  ## 80% of nodes remaining in network
#  boot.eighty <- partboot(equate.low, equate.high,
#                          percent = .80, iter = 1000,
#                          sim = "cosine", cores = 4)
#  ## 90% of nodes remaining in network
#  boot.ninety <- partboot(equate.low, equate.high,
#                          percent = .90, iter = 1000,
#                          sim = "cosine", cores = 4)

## ----Partial bootstrap analysis plots, echo = TRUE, eval = FALSE, comment = NA, warning = FALSE----
#  # Plot bootstrap results
#  plots <- plot(boot.fifty, boot.sixty, boot.seventy,
#                boot.eighty, boot.ninety, groups = c("Low","High"),
#                measures = c("ASPL", "CC", "Q"))

## ----Fig7, fig.cap = "Plots of the boostrapped partial network measures (1000 samples per percentage of nodes remaining. Density plots are above the box plots and scatterplots (individual dots depict a single sample). The black dot in the scatterplots represents the mean for the respective group and percentage.", fig.align = 'center', fig.height = 9, fig.width = 5, warning = FALSE, eval = TRUE, echo = FALSE, message = FALSE----
gridExtra::grid.arrange(vignette.plots$aspl, vignette.plots$cc, vignette.plots$q)

## ----Partial bootstrap analysis tests, echo = TRUE, eval = FALSE, comment = NA, warning = FALSE----
#  # Perform t-tests on bootstrap results
#  tests <- partboot.test(boot.fifty, boot.sixty, boot.seventy,
#                         boot.eighty, boot.ninety)

## ----tab9, echo = FALSE, eval = TRUE, comment = NA, warning = FALSE-----------
output <- matrix(c("1998", "-82.59", "3.69", "50.73", "2.27", "-66.76", "2.99",
                   "1998", "-54.64", "2.44", "39.75", "1.78", "-46.93", "2.10",
                   "1998", "-34.86", "1.56", "32.24", "1.44", "-33.61", "1.50",
                   "1998", "-23.31", "1.04", "22.50", "1.01", "-24.58", "1.10",
                   "1998", "-17.19", "0.77", "18.05", "0.81", "-18.11", "0.81"),
                 ncol = 7, byrow = TRUE)


htmlTable::htmlTable(output,
          header = c("df", "t", "d", "t", "d", "t", "d"),
          rnames = paste(c(90, 80, 70, 60, 50), "%", sep = ""),
          tfoot = "Note: 1000 samples were generated for each percentage of nodes remaining. t-statistics and Cohen’s d values are presented (Cohen, 1992). Negative t-statistics denote the high openness to experience group having lower values than the low openness to experience group. All p’s < 0.001. Cohen’s d effect sizes: 0.50, moderate; 0.80, large; 1.10, very large. ASPL, average shortest path length; CC, clustering coefficient; Q, modularity.",
          caption = "Table 9. Partial bootstrapped networks results")

