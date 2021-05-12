compare_ms <- function(ms1, ms2, error = 5, minerror = 0.002, m = 1, n = 0.5) {
  ms1 <- data.frame(mz = ms1[,"mz"], int = ms1[,"int"]/sum(ms1[,"int"]))
  ms2 <- data.frame(mz = ms2[,"mz"], int = ms2[,"int"]/sum(ms2[,"int"]))
  m1 <- ms1[,"mz"]
  i1 <- ms1[,"int"]
  m2 <- sapply(m1, function(x) mean(ms2[which(ms2[,"mz"] >= x - max(x*error*1E-6,minerror) & ms2[,"mz"] <= x + max(x*error*1E-6,minerror)),"mz"]))
  m2[which(is.nan(m2))] <- 0
  i2 <- sapply(m1, function(x) sum(ms2[which(ms2[,"mz"] >= x - max(x*error*1E-6,minerror) & ms2[,"mz"] <= x + max(x*error*1E-6,minerror)),"int"]))
  i2[which(is.nan(i2))] <- 0
  dp <- dotprod(m1, i1, m2, i2, m, n)
  m1 <- ms2[,"mz"]
  i1 <- ms2[,"int"]
  m2 <- sapply(m1, function(x) mean(ms1[which(ms1[,"mz"] >= x - max(x*error*1E-6,minerror) & ms1[,"mz"] <= x + max(x*error*1E-6,minerror)),"mz"]))
  m2[which(is.nan(m2))] <- 0
  i2 <- sapply(m1, function(x) sum(ms1[which(ms1[,"mz"] >= x - max(x*error*1E-6,minerror) & ms1[,"mz"] <= x + max(x*error*1E-6,minerror)),"int"]))
  i2[which(is.nan(i2))] <- 0
  rdp <- dotprod(m1, i1, m2, i2, m, n)
  data.frame(dp = dp, rdp = rdp)
}

dotprod <- function(m1, i1, m2, i2, m = 1, n = 0.5) {
  W1 <- (m1^m)*(i1^n)
  W2 <- (m2^m)*(i2^n)
  nom <- sum(W1*W2)^2
  denom <- sum(W1^2)*sum(W2^2)
  nom/denom
}

plot_compare_ms <- function(ms1, ms2, main = "Comparison Mass Spectrum", size = 1, c1 = "black", c2 = "red", ylim.exp = 1) {
  ms1 <- data.frame(mz = ms1[,"mz"], mz.u = ms1[,"mz.u"], int = ms1[,"int"]/sum(ms1[,"int"]), int.u = ms1[,"int.u"]/sum(ms1[,"int"]))
  ms2 <- data.frame(mz = ms2[,"mz"], mz.u = ms2[,"mz.u"], int = ms2[,"int"]/sum(ms2[,"int"]), int.u = ms2[,"int.u"]/sum(ms2[,"int"]))
  xlim <- c(min(c(ms1[,"mz"] - 1, ms2[,"mz"] - 1)), max(c(ms1[,"mz"] + 1, ms2[,"mz"] + 1)))
  ylim <- c(min(c(-ms1[,"int"] - ylim.exp, -ms2[,"int"] - ylim.exp)), max(c(ms1[,"int"] + ylim.exp, ms2[,"int"] + ylim.exp)))
  df1 <- data.frame(x1 = ms1[,"mz"], y1 = ms1[,"int"], x1u = ms1[,"mz.u"], y1u = ms1[,"int.u"])
  df2 <- data.frame(x2 = ms2[,"mz"], y2 = -ms2[,"int"], x2u = ms2[,"mz.u"], y2u = ms2[,"int.u"])
  ggplot(data = df1) + geom_linerange(aes(x = x1,  ymin = 0, ymax = y1), color = c1, size = size) + 
    geom_errorbar(aes(x = x1, ymin = y1 - y1u, ymax = y1 + y1u, width = 0.01), color = c2, na.rm = TRUE, linetype = 5) + 
    geom_errorbarh(aes(y = y1, xmin = x1 - x1u, xmax = x1 + x1u, height = 0.01), color = c2, na.rm = TRUE, linetype = 5) + 
    geom_linerange(data = df2, aes(x = x2, ymin = 0, ymax = y2), color = c2, size = size) +
    geom_errorbar(data = df2, aes(x = x2, ymin = y2 - y2u, ymax = y2 + y2u, width = 0.01), color = c1, na.rm = TRUE, linetype = 5) + 
    geom_errorbarh(data = df2, aes(y = y2, xmin = x2 - x2u, xmax = x2 + x2u, height = 0.01), color = c1, na.rm = TRUE, linetype = 5) + 
    ggtitle(main) + xlab("m/z") + ylab("Relative Intensity") + coord_cartesian(xlim = xlim, ylim = ylim)+ theme_bw() 
}