rm(list = ls())

library(tidyr)
library(dplyr)
library(readr)

complexity_measures2 <- function(balassa_index, method = "fitness", iterations = 20, extremality = 1) {
  # sanity checks ----
  if (!(any(class(balassa_index) %in% "dgCMatrix") == TRUE)) {
    stop("'balassa_index' must be a dgCMatrix")
  }

  if (!(any(method %in% c("fitness", "reflections", "eigenvalues")) == TRUE)) {
    stop("'method' must be 'fitness', 'reflections' or 'eigenvalues'")
  }

  if (!is.integer(iterations)) {
    iterations <- as.integer(iterations)

    if (iterations < 2L) {
      stop("'iterations' must be integer and >= 2")
    }
  }

  if (iterations %% 2 != 0) {
    iterations <- iterations + 1
    warning("'iterations' was changed to 'iterations + 1' to work with an even number of iterations")
  }

  # compute complexity measures ----
  if (method == "fitness") {
    fitness_output <- fitness_method(balassa_index, iterations, extremality)
    xci <- fitness_output$xci
    yci <- fitness_output$yci
  }

  if (method == "reflections") {
    reflections_output <- reflections_method(balassa_index, iterations)
    xci <- reflections_output$xci
    yci <- reflections_output$yci
  }

  if (method == "eigenvalues") {
    # to check if a sign correction is needed
    reflections_output <- reflections_method(balassa_index, iterations)
    xci_r <- reflections_output$xci
    yci_r <- reflections_output$yci

    eigenvalues_output <- eigenvalues_method(balassa_index, iterations)
    xci <- eigenvalues_output$xci
    yci <- eigenvalues_output$yci

    # correct xci sign when required
    if (isTRUE(cor(xci, xci_r, use = "pairwise.complete.obs") < 0)) {
      xci <- (-1) * xci
    }

    # correct yci sign when required
    if (isTRUE(cor(yci, yci_r, use = "pairwise.complete.obs") < 0)) {
      yci <- (-1) * yci
    }
  }

  return(
    list(
      complexity_index_country = xci,
      complexity_index_product = yci
    )
  )
}

reflections_method <- function(balassa_index, iterations) {
  # create empty matrices
  kx <- Matrix(0,
               nrow = nrow(balassa_index), ncol = iterations,
               sparse = TRUE
  )

  ky <- Matrix(0,
               nrow = ncol(balassa_index), ncol = iterations,
               sparse = TRUE
  )

  # fill the first columns with rowSums(balassa_index) and colSums(balassa_index) to start iterating
  kx[, 1] <- rowSums(balassa_index)
  ky[, 1] <- colSums(balassa_index)

  # compute cols 2 to "no. of iterations" by iterating from col 1
  for (j in 2:ncol(kx)) {
    kx[, j] <- (balassa_index %*% ky[, (j - 1)]) / rowSums(balassa_index)
    ky[, j] <- (crossprod(balassa_index, kx[, (j - 1)])) / colSums(balassa_index)
  }

  # xci is of odd order and normalized
  # yci is of even order and normalized
  return(
    list(
      xci = setNames(
        (kx[, iterations - 1] - mean(kx[, iterations - 1])) / sd(kx[, iterations - 1]),
        rownames(balassa_index)
      ),
      yci = setNames(
        (ky[, iterations] - mean(ky[, iterations])) / sd(ky[, iterations]),
        colnames(balassa_index)
      )
    )
  )
}

eigenvalues_method <- function(balassa_index, iterations) {
  # compute eigenvalues for xci
  xci <- eigen((balassa_index / rowSums(balassa_index)) %*% (t(balassa_index) / colSums(balassa_index)))
  xci <- Re(xci$vectors[, 2])

  # compute eigenvalues for yci
  yci <- eigen((t(balassa_index) / colSums(balassa_index)) %*% (balassa_index / rowSums(balassa_index)))
  yci <- Re(yci$vectors[, 2])

  return(
    list(
      xci = setNames(
        (xci - mean(xci)) / sd(xci),
        rownames(balassa_index)
      ),
      yci = setNames(
        (yci - mean(yci)) / sd(yci),
        colnames(balassa_index)
      )
    )
  )
}

world_trade <- read_csv("./data/WtoData_20211018200240.csv") %>%
  filter(`Indicator Code` == "ITS_MTV_AX") %>% # Annual exports ITS_MTV_AX
  rename(country = `Reporting Economy ISO3A Code`,
         product = `Product/Sector Code`,
         value = Value) %>%
  group_by(country, product) %>%
  summarize(export_val = mean(value)) %>%
  na.omit() %>%
  select(export_val, country, product)

#-----------------------------#
#### Using ecxplor library ####
#-----------------------------#

source("./R/model.R")

rca_panel <- computeRCA(world_trade)
complexity_panel <- computeComplexity(rca_panel)
m_matrix <- computeM(complexity_panel)
M <- m_matrix
diversity_0 <- computeDiversity0(m_matrix)
ubiquity_0 <- computeUbiquity0(m_matrix)
M_tilde_PCI <- computeMTilde(m_matrix,
                         diversity_0,
                         ubiquity_0,
                         flag = "PCI")
pci <- computeRanks(M_tilde_PCI)
pci
M_tilde_ECI <- computeMTilde(m_matrix,
                             diversity_0,
                             ubiquity_0,
                             flag = "ECI")
eci <- computeRanks(M_tilde_ECI)
eci

phi <- computeProximity(M, ubiquity_0)
phi
d <- computeDistance(M, phi)
