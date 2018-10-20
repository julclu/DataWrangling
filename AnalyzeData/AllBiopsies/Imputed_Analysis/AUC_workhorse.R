auc <- function(y, prob) {
  if (is.factor(y)) {
    y.uniq <- levels(y)
  }
  else {
    y.uniq <- sort(unique(y))
  }
  nclass <- length(y.uniq)
  AUC <- NULL
  for (i in 1:(nclass - 1)) {
    for (j in (i + 1):nclass) {
      pt.ij <- (y == y.uniq[i] | y == y.uniq[j])
      if (sum(pt.ij) > 1) {
        y.ij <- y[pt.ij]
        pij <- prob[pt.ij, j]
        pji <- prob[pt.ij, i]
        Aij <-  auc.workhorse(cbind(pij, 1 * (y.ij == y.uniq[j])))
        Aji <-  auc.workhorse(cbind(pji, 1 * (y.ij == y.uniq[i])))
        AUC <- c(AUC, (Aij + Aji)/2)
      }
    } 
  }
  if (is.null(AUC)) {
    NA
  }
  else {
    mean(AUC, na.rm = TRUE)
  }
}

auc.workhorse <- function(roc.data) {
  x <- roc.data[, 1][roc.data[, 2] == 1]
  y <- roc.data[, 1][roc.data[, 2] == 0]
  if (length(x) > 1 & length(y) > 1) {
    AUC  <- tryCatch({wilcox.test(x, y, exact=F)$stat/(length(x)*length(y))}, error=function(ex){NA})
  }
  else {
    AUC <- NA
  }
  AUC
}
