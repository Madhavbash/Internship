####### Function agcatfisherposthoc Chi square analysis and plot ###############
### Created: 200715 Author: Alfonso Garmendia  algarsal at upvnet dot upv dot es
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agcatfisherposthoc <-
  function(tbl, test = c("fisher.test"), popsInRows = TRUE,
            control = c("holm","fdr", "BH", "BY", "bonferroni", 
                        "hochberg", "hommel"),
            alpha0 = 0.05, digits = 4, ...)
  {
    #### check variables ####
    control <- match.arg(control)
    test = match.fun(test)
    if (!popsInRows)
      tbl <- t(tbl)
    popsNames <- rownames(tbl)
    prs <- combn(1:nrow(tbl), 2)
    tests <- ncol(prs)
    pvals <- numeric(tests)
    # lbls <-
      var1 <- var2 <- character(tests)
    # i = 1
    for (i in 1:tests) {
      pvals[i] <- test(tbl[prs[, i], ], ...)$p.value
      # lbls[i] <- paste(popsNames[prs[, i]], collapse = " vs. ")
      var1[i] <- popsNames[prs[, i]][1]
      var2[i] <- popsNames[prs[, i]][2]
    }
    adj.pvals <- p.adjust(pvals, method = control)
    Significant <- adj.pvals < alpha0

    # cat("Adjusted p-values used the", control, "method.\n\n")
    data.frame(var1 = var1, var2 = var2,
               raw.p = format(pvals, digits = digits),
               adj.p = format(adj.pvals, digits = digits),
               Significant = Significant)
  }

