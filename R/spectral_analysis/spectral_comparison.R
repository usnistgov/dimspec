# Spectral Comparison Functions ----------------------------------------------------------

#' Pairwise data.frame of two uncertainty mass spectra
#'
#' The function stacks two uncertainty mass spectra together based on binned m/z values
#'
#' @param ums1 uncertainty mass spectrum from `get_ums` function
#' @param ums2 uncertainty mass spectrum from `get_ums` function
#' @param masserror the mass accuracy (in ppm) of the instrument data
#' @param minerror the minimum mass error (in Da) of the instrument data
#'
#' @export
#'


pair_ums <- function(ums1, ums2, error = 5, minerror = 0.002) {
  ums1 <- as.matrix(ums1)
  ums2 <- as.matrix(ums2)
  colnames(ums1) <- NULL
  colnames(ums2) <- NULL
  newums <- cbind(ums1, matrix(NA, ncol = 5, nrow = nrow(ums1)))
  for (i in 1:nrow(ums1)) {
    ind <- which(ums2[,1] >= ums1[i,1] - max(minerror, ums1[i,1]*error*1E-6) & ums2[,1] <= ums1[i,1] + max(minerror, ums1[i,1]*error*1E-6))
    if (length(ind) > 0) {
      newums[i,6:10] <- ums2[ind,1:5]
      ums2 <- ums2[-ind,]
      ind <- integer(0)
    }
  }
  if (nrow(ums2) > 0) {
    addms <- matrix(NA, ncol = 5, nrow = nrow(ums2))
    newums <- rbind(newums, cbind(addms, ums2))
  }
  colnames(newums) <- c("mz_1", "mz.u_1", "int_1", "int.u_1", "n_1", "mz_2", "mz.u_2", "int_2", "int.u_2", "n_2")
  newums
}

#' Significance testing function
#'
#' Internal function: enables significance testing between two values
#'
#' @param x1,x2 mean values to be compared
#' @param s1,s2 standard deviation of their respective values
#' @param n1,n2 number of observations of the respective values
#' @param sig significance level to test (0.95 = 95\%)
#'

sigtest <- function(x1, x2, s1, s2, n1, n2, sig = 0.95) {
  tmax <- qt(c(1-sig, sig), min(c(n1,n2)))
  t <- (x1 - x2)/sqrt(((s1^2)/n1)+((s2^2)/n2))
  t >= tmax[1] & t <= tmax[2]
}

#' Pool standard deviations
#'
#' Internal function: calculates a pooled standard deviation
#'
#' @param sd A vector containing numeric values of standard deviations
#' @param n A vector containing integers for the number of observations respective to the sd values
#'

pool.sd <- function(sd, n) {
  sqrt(sum((n-1)*(sd^2))/sum(n-1))
}

#' Pool uncertainty mass spectra
#'
#' Calculates a pooled uncertainty mass spectrum that is a result of data from multiple 
#' uncertainty mass spectra.
#'
#' @param umslist A list where each item is a uncertainty mass spectrum from function `get_ums`
#' @param masserror the mass accuracy (in ppm) of the instrument data
#' @param minerror the minimum mass error (in Da) of the instrument data
#' 


pool.ums <- function(umslist, error = 5, minerror = 0.002) {
  umslist <- lapply(umslist, function(x) {colnames(x) <- c("mz", "mz.u", "int", "int.u", "n"); x})
  ums <- lapply(1:nrow(umslist[[1]]), function(x) data.frame(mz = umslist[[1]][x,"mz"], mz.u = umslist[[1]][x,"mz.u"], int = umslist[[1]][x,"int"], int.u = umslist[[1]][x,"int.u"], n = umslist[[1]][x,"n"]))
  if (length(umslist) > 1) {
    for (i in 2:length(umslist)) {
      lowmasses <- sapply(ums, function(x) min(x[,"mz"]))
      highmasses <- sapply(ums, function(x) max(x[,"mz"]))
      lowmasses <- lowmasses - sapply(lowmasses, function(x) max(x*error*1E-6,minerror))
      highmasses <- highmasses + sapply(highmasses, function(x) max(x*error*1E-6,minerror))
      for (j in 1:nrow(umslist[[i]])) {
        ind <- intersect(which(lowmasses <= umslist[[i]][j,"mz"]), which(highmasses >= umslist[[i]][j,"mz"]))
        if (length(ind) > 0) {
          ums[[ind]] <- rbind(ums[[ind]], c(umslist[[i]][j,c("mz", "mz.u", "int", "int.u", "n")]))
        }
        if (length(ind) == 0) {
          ums[[length(ums)+1]] <- data.frame(mz = umslist[[i]][j,"mz"], mz.u = umslist[[i]][j,"mz.u"], int = umslist[[i]][j,"int"], int.u = umslist[[i]][j,"int.u"], n = umslist[[i]][j,"n"])
        }
      }
    }
  }
  out <- do.call(rbind, lapply(ums, function(x) c(mean(x[,"mz"]), pool.sd(x[,"mz.u"], x[,"n"]), mean(x[,"int"]), pool.sd(x[,"int.u"], x[,"n"]), sum(x[,"n"]))))
  colnames(out) <- c("mz", "mz.u", "int", "int.u", "n")
  out
}

#' Calculate dot product match score
#'
#' Calculates a the match score (based on dot product) of the two uncertainty mass spectra.
#' Note: this is a static match score and does not include associated uncertainties.
#'
#' @param ms1,ms2 the uncertainty mass spectra from function `get_ums`
#' @param error a vector of the respective mass error (in ppm) for each mass spectrum or a single vector representing the mass error for all m/z values
#' @param minerror a two component vector of the respective minimum mass error (in Da) for each mass spectrum or a single value representing the minimum mass error of all m/z values
#' @param m,n weighting values for mass (m) and intensity (n)
#' 

compare_ms <- function(ms1, ms2, error = c(5,5), minerror = c(0.002,0.002), m = 1, n = 0.5) {
  if (length(error) == 1) {error <- rep(error, 2)}
  if (length(minerror) == 1) {minerror <- rep(minerror, 2)}
  if (nrow(ms1) == 0 | nrow(ms2) == 0) {return(data.frame(dp = NA, rdp = NA))}
  ms1 <- data.frame(mz = ms1[,"mz"], int = ms1[,"int"]/sum(ms1[,"int"]))
  ms2 <- data.frame(mz = ms2[,"mz"], int = ms2[,"int"]/sum(ms2[,"int"]))
  m1 <- ms1[,"mz"]
  i1 <- ms1[,"int"]
  m2 <- sapply(m1, function(x) mean(ms2[sapply(ms2, function(y) overlap(x, max(x*error[1]*1E-6,minerror[1]), y, max(y*error[2]*1E-6,minerror[2]))),"mz"]))
  m2[which(is.nan(m2))] <- 0
  i2 <- sapply(m1, function(x) sum(ms2[sapply(ms2, function(y) overlap(x, max(x*error[1]*1E-6,minerror[1]), y, max(y*error[2]*1E-6,minerror[2]))),"int"]))
  i2[which(is.nan(i2))] <- 0
  dp <- dotprod(m1, i1, m2, i2, m, n)
  m1 <- ms2[,"mz"]
  i1 <- ms2[,"int"]
  m2 <- sapply(m1, function(x) mean(ms1[sapply(ms1, function(y) overlap(x, max(x*error[2]*1E-6,minerror[2]), y, max(y*error[1]*1E-6,minerror[1]))),"mz"]))
  m2[which(is.nan(m2))] <- 0
  i2 <- sapply(m1, function(x) sum(ms1[sapply(ms1, function(y) overlap(x, max(x*error[2]*1E-6,minerror[2]), y, max(y*error[1]*1E-6,minerror[1]))),"int"]))
  i2[which(is.nan(i2))] <- 0
  rdp <- dotprod(m1, i1, m2, i2, m, n)
  data.frame(dp = dp, rdp = rdp)
}

#' Calculate dot product match score using bootstrap data
#'
#' Calculates a the match score (based on dot product) of the two uncertainty mass spectra.
#' To generate a distribution of match scores using the uncertainty of the two mass spectra,
#' bootstrapped data (using `rnorm` for now)
#'
#' @param ms1,ms2 the uncertainty mass spectra from function `get_ums`
#' @param error a vector of the respective mass error (in ppm) for each mass spectrum or a single vector representing the mass error for all m/z values
#' @param minerror a two component vector of the respective minimum mass error (in Da) for each mass spectrum or a single value representing the minimum mass error of all m/z values
#' @param m,n weighting values for mass (m) and intensity (n)
#' @param runs
#' 

bootstrap_compare_ms <- function(ms1, ms2, error = c(5,5), minerror = c(0.002,0.002), m = 1, n = 0.5, runs = 10000) {
  ms1[which(is.na(ms1), arr.ind = TRUE)] <- 0
  ms2[which(is.na(ms2), arr.ind = TRUE)] <- 0
  ms1.rms <- lapply(1:runs, function(x) data.frame(
    mz = sapply(1:nrow(ms1), function(m) {max(0, rnorm(1, mean = ms1[m,"mz"], sd = ms1[m,"mz.u"]))}),
    int = sapply(1:nrow(ms1), function(m) {max(0, rnorm(1, mean = ms1[m,"int"], sd = ms1[m,"int.u"]))})
  ))
  ms2.rms <- lapply(1:runs, function(x) data.frame(
    mz = sapply(1:nrow(ms2), function(m) {max(0, rnorm(1, mean = ms2[m,"mz"], sd = ms2[m,"mz.u"]))}),
    int = sapply(1:nrow(ms2), function(m) {max(0, rnorm(1, mean = ms2[m,"int"], sd = ms2[m,"int.u"]))})
  ))
  results <- do.call(rbind, lapply(1:runs, function(x) compare_ms(ms1.rms[[x]], ms2.rms[[x]], error, minerror)))
  dp_summary <- data.frame(
    dp = quantile(results$dp, probs = seq(0,1,0.25), na.rm = T), 
    rdp = quantile(results$rdp, probs = seq(0,1,0.25), na.rm = T)
  )
  list(results = results, dp_summary = dp_summary)
}

boxplot_quant <- function(means, quants, labels = 1:nrow(quants), xlab = "Number", ylab = "Value", main = "Boxplot", xlim = NULL, ylim = NULL, x.rotate = 0) {
  require(ggplot2)
  df <- data.frame(x = labels, y = means, ymin = quants[,1], lower = quants[,2], middle = quants[,3], upper = quants[,4], ymax = quants[,5])
  ggplot(data = df, aes(x)) + geom_boxplot(aes(x = x, min = ymin, ymax = ymax, lower = lower, middle = middle, upper = upper, group = x), stat = "identity") + geom_point(aes(x = x, y = y)) + 
    xlab(xlab) + ylab(ylab) + ggtitle(main) + coord_cartesian(xlim = xlim, ylim = ylim) +
    theme_bw() + theme(axis.text.x = element_text(angle = x.rotate))
}

#' Calculate overlap ranges
#'
#' Internal function: determines if two ranges (x1-e1 to x1+e1) and (x2-e2 to x2+e2) overlap (nonstatistical evaluation)
#'
#' @param x1,x2 values containing mean values
#' @param e1,e2 values containing respective error values

overlap <- function(x1, e1, x2, e2) {
  (x1 + e1) >= (x2 - e2) & (x1 - e1) <= (x2 + e2)
}



#' Calculate dot product
#'
#' Internal function: calculates the dot product between paired m/z and intensity values
#'
#' @param m1,m2 paired vectors containing measured m/z values
#' @param i1,i2 paired vectors containing measured intensity values
#' @param m,n weighting values for mass (m) and intensity (n)
#'

dotprod <- function(m1, i1, m2, i2, m = 1, n = 0.5) {
  W1 <- (m1^m)*(i1^n)
  W2 <- (m2^m)*(i2^n)
  nom <- sum(W1*W2)^2
  denom <- sum(W1^2)*sum(W2^2)
  nom/denom
}

#' Plot MS Comparison
#'
#' Plots a butterfly plot for the comparison of two uncertainty mass spectra
#'
#' @param ums1,ums2 uncertainty mass spectrum from `get_ums` function
#' @param main Main Title of the Plot
#' @param size line width of the mass spectra lines
#' @param c1 Color of the top (ums1) mass spectral lines
#' @param c2 Color of the bottom (ums2) mass spectral lines
#' @param ylim.exp Expansion unit for the y-axis

plot_compare_ms <- function(ums1, ums2, main = "Comparison Mass Spectrum", size = 1, c1 = "black", c2 = "red", ylim.exp = 1) {
  require(ggplot2)
  ums1 <- data.frame(mz = ums1[,"mz"], mz.u = ums1[,"mz.u"], int = ums1[,"int"]/sum(ums1[,"int"]), int.u = ums1[,"int.u"]/sum(ums1[,"int"]))
  ums2 <- data.frame(mz = ums2[,"mz"], mz.u = ums2[,"mz.u"], int = ums2[,"int"]/sum(ums2[,"int"]), int.u = ums2[,"int.u"]/sum(ums2[,"int"]))
  xlim <- c(min(c(ums1[,"mz"] - 1, ums2[,"mz"] - 1)), max(c(ums1[,"mz"] + 1, ums2[,"mz"] + 1)))
  ylim <- c(min(c(-ums1[,"int"] - ylim.exp, -ums2[,"int"] - ylim.exp)), max(c(ums1[,"int"] + ylim.exp, ums2[,"int"] + ylim.exp)))
  df1 <- data.frame(x1 = ums1[,"mz"], y1 = ums1[,"int"], x1u = ums1[,"mz.u"], y1u = ums1[,"int.u"])
  df2 <- data.frame(x2 = ums2[,"mz"], y2 = -ums2[,"int"], x2u = ums2[,"mz.u"], y2u = ums2[,"int.u"])
  # Breaking change to line geometries in arguments from `size` to `linewidth` starting in ggplot2 v3.4.0
  if (packageVersion("ggplot2") >= '3.4.0') {
    ggplot(data = df1) +
      geom_linerange(aes(x = x1,  ymin = 0, ymax = y1), color = c1, linewidth = size) + 
      geom_errorbar(aes(x = x1, ymin = y1 - y1u, ymax = y1 + y1u, width = 0.01), color = c2, na.rm = TRUE, linetype = 5) + 
      geom_errorbarh(aes(y = y1, xmin = x1 - x1u, xmax = x1 + x1u, height = 0.01), color = c2, na.rm = TRUE, linetype = 5) + 
      geom_linerange(data = df2, aes(x = x2, ymin = 0, ymax = y2), color = c2, linewidth = size) +
      geom_errorbar(data = df2, aes(x = x2, ymin = y2 - y2u, ymax = y2 + y2u, width = 0.01), color = c1, na.rm = TRUE, linetype = 5) + 
      geom_errorbarh(data = df2, aes(y = y2, xmin = x2 - x2u, xmax = x2 + x2u, height = 0.01), color = c1, na.rm = TRUE, linetype = 5) + 
      ggtitle(main) + xlab("m/z") + ylab("Relative Intensity") +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      theme_bw() 
  } else {
    ggplot(data = df1) +
      geom_linerange(aes(x = x1,  ymin = 0, ymax = y1), color = c1, size = size) + 
      geom_errorbar(aes(x = x1, ymin = y1 - y1u, ymax = y1 + y1u, width = 0.01), color = c2, na.rm = TRUE, linetype = 5) + 
      geom_errorbarh(aes(y = y1, xmin = x1 - x1u, xmax = x1 + x1u, height = 0.01), color = c2, na.rm = TRUE, linetype = 5) + 
      geom_linerange(data = df2, aes(x = x2, ymin = 0, ymax = y2), color = c2, size = size) +
      geom_errorbar(data = df2, aes(x = x2, ymin = y2 - y2u, ymax = y2 + y2u, width = 0.01), color = c1, na.rm = TRUE, linetype = 5) + 
      geom_errorbarh(data = df2, aes(y = y2, xmin = x2 - x2u, xmax = x2 + x2u, height = 0.01), color = c1, na.rm = TRUE, linetype = 5) + 
      ggtitle(main) + xlab("m/z") + ylab("Relative Intensity") +
      coord_cartesian(xlim = xlim, ylim = ylim) +
      theme_bw() 
  }
}