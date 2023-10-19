# pair.ia


function (gid, sample = 0L, quiet = FALSE, plot = TRUE, low = "blue", 
          high = "red", limits = NULL, index = "rbarD", method = 1L) 
{
  N <- nInd(gid)
  numLoci <- nLoc(gid)
  lnames <- locNames(gid)
  np <- choose(N, 2)
  nploci <- choose(numLoci, 2)
  shuffle <- sample > 0L
  if (quiet) {
    oh <- progressr::handlers()
    on.exit(progressr::handlers(oh))
    progressr::handlers("void")
  }
  progressr::with_progress({
    p <- make_progress((1 + sample) * nploci, 50)
    res <- pair_ia_internal(gid, N, numLoci, lnames, np, 
                            nploci, p, sample = 0)
    if (shuffle) {
      counts <- matrix(1L, nrow = nrow(res), ncol = ncol(res))
      for (i in seq_len(sample)) {
        tmp <- shufflepop(gid, method = method)
        tmpres <- pair_ia_internal(tmp, N, numLoci, lnames, 
                                   np, nploci, p, i)
        counts <- counts + as.integer(tmpres >= res)
      }
      p <- counts/(sample + 1)
      res <- cbind(Ia = res[, 1], p.Ia = p[, 1], rbarD = res[, 
                                                             2], p.rD = p[, 2])
    }
  })
  class(res) <- c("pairia", "matrix")
  if (plot) {
    tryCatch(plot(res, index = index, low = low, high = high, 
                  limits = limits), error = function(e) e)
  }
  res
}






#```{r Parameters, tidy = TRUE}
filtered_data <-
  read.csv("../data/data-2023-09-11 (2).csv", header = TRUE)

selected_stats <-
  c("Ho", "Hs", "Ht", "Fis (W&C)", "Fst (W&C)", "Fis (Nei)", "Fst (Nei)")

n_rep = 10000
n_marker = 6
n_pop = 8

sequence_length <- length(6:11) 

num_cores <- 16
#```



## data filtering convertion

#```{r data filtering, tidy = TRUE, results='hide', message=FALSE}
library(hierfstat)
library(adegenet)
filtered_data <-
  data.frame(indv = paste(
    substr(filtered_data$Population, 1, 3),
    row.names(filtered_data),
    sep = "."
  ), filtered_data)
# Create mydata_genind
population <- filtered_data$Population
mydata_genind <- df2genind(
  X = filtered_data[, 6:11],
  sep = "/",
  ncode = 6,
  ind.names = filtered_data$indv,
  pop = filtered_data$Population,
  NA.char = "0/0",
  ploidy = 2,
  type = "codom",
  strata = NULL,
  hierarchy = NULL
)
mydata_hierfstat <- genind2hierfstat(mydata_genind)
#```


pair_ia_function_serial <- function(gid, sample = 0L, quiet = FALSE, plot = FALSE, low = "blue", high = "red", limits = NULL, index = "rbarD", method = 1L) {
  N <- nInd(gid)
  numLoci <- nLoc(gid)
  lnames <- locNames(gid)
  
  progressr::with_progress({
    p <- make_progress((1 + sample) * choose(N, 2) * choose(numLoci, 2), 50)
    res <- pair_ia_internal(gid, N, numLoci, lnames, choose(N, 2), choose(numLoci, 2), p, sample = 0)
    
    counts <- matrix(1L, nrow = nrow(res), ncol = ncol(res))
    for (i in seq_len(sample)) {
      tmp <- shufflepop(gid, method = method)
      tmpres <- pair_ia_internal(tmp, N, numLoci, lnames, choose(N, 2), choose(numLoci, 2), p, i)
      counts <- counts + as.integer(tmpres >= res)
    }
    p <- counts / (sample + 1)
    res <- cbind(Ia = res[, 1], p.Ia = p[, 1], rbarD = res[, 2], p.rD = p[, 2])
  })
  
  class(res) <- c("pairia", "matrix")
  return(res)
}


library(pegas)
library(foreach)
library(doParallel)
library(poppr)

pair_ia_function_parallel <- function(gid, sample = 0L, quiet = FALSE, plot = FALSE, low = "blue", high = "red", limits = NULL, index = "rbarD", method = 1L) {
  N <- nInd(gid)
  numLoci <- nLoc(gid)
  lnames <- locNames(gid)
  progressr::with_progress({
  p <- make_progress((1 + sample) * choose(N, 2) * choose(numLoci, 2), 50)
  res <- pair_ia_internal(gid, N, numLoci, lnames, choose(N, 2), choose(numLoci, 2), p, sample = 0)
  
  counts <- matrix(1L, nrow = nrow(res), ncol = ncol(res))
  
  # No sink here in the parallel block
  
  # Parallelize the loop
  foreach(i = seq_len(sample), .combine = 'c') %dopar% {
    tmp <- shufflepop(gid, method = method)
    tmpres <- pair_ia_internal(tmp, N, numLoci, lnames, choose(N, 2), choose(numLoci, 2), p, i)
    counts <- counts + as.integer(tmpres >= res)
  }
  
  # Cleanly stop the parallel backend
  on.exit(stopCluster(cl), add = TRUE)
  
  p <- counts / (sample + 1)
  res <- cbind(Ia = res[, 1], p.Ia = p[, 1], rbarD = res[, 2], p.rD = p[, 2])
  })
  
  class(res) <- c("pairia", "matrix")
  return(res)
}









#==============================================================================#
# Get the index of association from sums of distances over loci and samples
# 
# This will take in a list called that contains 3 vectors:
# 1. d.vector : a vector containing sums of distances per locus
# 2. d2.vector : like d.vector, but containing sums of squares
# 3. D.vector : a vector containing the distance over all samples.
#
# Public functions utilizing this function:
# ## none
#
# Internal functions utilizing this function:
# ## ia_pair_loc
#==============================================================================#
ia_from_d_and_D <- function(V, np){
  varD <- ((sum(V$D.vector^2) - ((sum(V$D.vector))^2)/np))/np
  vard.vector <- ((V$d2.vector - ((V$d.vector^2)/np))/np)
  vardpair.vector <- .Call("pairwise_covar", vard.vector, PACKAGE = "poppr")
  sigVarj <- sum(vard.vector)
  rm(vard.vector)
  Ia <- (varD/sigVarj) - 1
  rbarD <- (varD - sigVarj)/(2 * sum(vardpair.vector))
  return(c(Ia, rbarD))
}

pair_ia_internal <- function(gid, N, numLoci, lnames, np, nploci, p, sample = NULL) {
  # Calculate pairwise distances for each locus. This will be a matrix of 
  # np rows and numLoci columns.
  if (gid@type == "codom") {
    V <- pair_matrix(seploc(gid), numLoci, np)
  } else { # P/A case
    V <- apply(tab(gid), 2, function(x) as.vector(dist(x)))
    # checking for missing data and imputing the comparison to zero.
    if (any(is.na(V))) {
      V[which(is.na(V))] <- 0
    }
  }
  
  colnames(V) <- lnames
  
  # calculate I_A and \bar{r}_d for each combination of loci
  loci_pairs  <- combn(lnames, 2)
  ia_pairs    <- matrix(NA_real_, nrow = 2, ncol = nploci)
  for (i in seq(nploci)) {
    if ((nploci * sample + i) %% p$step == 0) p$rog() 
    the_pair <- loci_pairs[, i, drop = TRUE]
    newV <- V[, the_pair, drop = FALSE]
    ia_pairs[, i] <- ia_from_d_and_D(
      V = list(
        d.vector  = colSums(newV), 
        d2.vector = colSums(newV * newV), 
        D.vector  = rowSums(newV)
      ),
      np = np
    )
  }
  colnames(ia_pairs) <- apply(loci_pairs, 2, paste, collapse = ":")
  rownames(ia_pairs) <- c("Ia", "rbarD")
  ia_pairs           <- t(ia_pairs)
  ia_pairs
}

pair_ia_internal <- function(gid, N, numLoci, lnames, np, nploci, p, sample = NULL) {
  # Calculate pairwise distances for each locus. This will be a matrix of 
  # np rows and numLoci columns.
  if (gid@type == "codom") {
    V <- pair_matrix(seploc(gid), numLoci, np)
  } else { # P/A case
    V <- apply(tab(gid), 2, function(x) as.vector(dist(x)))
    # checking for missing data and imputing the comparison to zero.
    if (any(is.na(V))) {
      V[which(is.na(V))] <- 0
    }
  }
}

#==============================================================================#
# This creates a pairwise difference matrix via the C function pairdiffs in
# src/poppr_distance.c
# 
# Public functions utilizing this function:
# # none
#
# Internal functions utilizing this function:
# # pair_diffs, pair.ia
#
#==============================================================================#
pair_matrix <- function(pop, numLoci, np)
{
  temp.d.vector <- matrix(nrow = np, ncol = numLoci, data = as.numeric(NA))
  temp.d.vector <- vapply(pop, function(x) .Call("pairdiffs", tab(x), 
                                                 PACKAGE = "poppr")/2, 
                          FUN.VALUE = temp.d.vector[, 1])
  temp.d.vector <- ceiling(temp.d.vector)
  return(temp.d.vector)
}

make_progress <- function(reps, steps = 50) {
  step  <- reps/steps
  scale <- if (step < 1) step else 1
  p     <- progressr::progressor(steps, scale = scale)
  list(rog = p, step = if (step < 1) step else round(step))
}



# Usage example:
library(tictoc)

# Load required libraries on the workers
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(pegas)
  library(poppr)
})

tic(msg = "parallel")
result_parallel <- pair_ia_function_parallel(mydata_genind, sample = n_rep, quiet = FALSE, plot = FALSE)
toc()
tic(msg = "parallel")
result_serial <-   pair_ia_function_serial(mydata_genind, sample = n_rep, quiet = FALSE, plot = FALSE)
toc()

# Stop the cluster
stopCluster(cl)
