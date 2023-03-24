#' @importFrom stats as.formula
#' @importFrom PSweight PStrim
PSweight.modified <- function(ps.formula = NULL, ps.estimate = NULL, trtgrp = NULL, zname = NULL, yname, data, weight = "overlap", delta = 0, augmentation = FALSE, bootstrap = FALSE, R = 50, out.formula = NULL, out.estimate = NULL, family = "gaussian", ps.method = "glm", ps.control = list(), out.method = "glm", out.control = list(), weight.external = NULL) {

  # extract zname
  if (!is.null(ps.formula)) {
    ps.formula <- as.formula(ps.formula)
    zname <- all.vars(ps.formula)[1]
  }

  data[zname] <- as.character(unlist(data[zname]))
  categoryz1 <- unique(unlist(data[zname]))
  z1 <- as.numeric(factor(unlist(data[zname])))
  oldlevel1 <- categoryz1[order(unique(z1))]
  ncate <- length(categoryz1)

  # trim the data
  if (delta > 0) {
    trimobj <- do.call(PStrim, list(data = data, ps.formula = ps.formula, zname = zname, ps.estimate = ps.estimate, delta = delta, optimal = FALSE, out.estimate = out.estimate, method = ps.method, ps.control = ps.control))
    data <- trimobj$data
    ps.estimate <- trimobj$ps.estimate
    out.estimate <- trimobj$out.estimate
  }


  if (ncate == 2) {
    do.call(binest.modified, list(ps.formula = ps.formula, ps.estimate = ps.estimate, zname = zname, yname = yname, data = data, trtgrp = trtgrp, augmentation = augmentation, bootstrap = bootstrap, R = R, out.formula = out.formula, out.estimate = out.estimate, family = family, weight = weight, ps.method = ps.method, ps.control = ps.control, out.method = out.method, out.control = out.control, weight.external = weight.external))
  } # else {
  #   do.call(mulest, list(ps.formula = ps.formula, ps.estimate = ps.estimate, zname = zname, yname = yname, data = data, trtgrp = trtgrp, augmentation = augmentation, bootstrap = bootstrap, R = R, out.formula = out.formula, out.estimate = out.estimate, family = family, weight = weight, ps.method = ps.method, ps.control = ps.control, out.method = out.method, out.control = out.control, weight.external = weight.external))
  # }
}
