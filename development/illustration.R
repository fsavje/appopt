library("appopt")

n_large <- 1e+6L
n_larger <- 1e+7L
n_largest <- 3e+7L
n_massive <- 2e+8L

### 1,000,000 data points
data <- data.frame(x1 = rnorm(n_large), x2 = rnorm(n_large))
ptm <- proc.time()
blocking <- get_blocking(data, 2, algorithm = "directed", MIS_method = "lexical")
proc.time() - ptm
rm(data, blocking)
gc()

### 10,000,000 data points
data <- data.frame(x1 = rnorm(n_larger), x2 = rnorm(n_larger))
ptm <- proc.time()
blocking <- get_blocking(data, 2, algorithm = "directed", MIS_method = "lexical")
proc.time() - ptm
rm(data, blocking)
gc()

### 30,000,000 data points
data <- data.frame(x1 = rnorm(n_largest), x2 = rnorm(n_largest))
ptm <- proc.time()
blocking <- get_blocking(data, 2, algorithm = "directed", MIS_method = "lexical")
proc.time() - ptm
rm(data, blocking)
gc()

### 200,000,000 data points, excluding NN search
nn_indices <- c(1:(n_massive - 1), 0L)
ptm <- proc.time()
blocking <- .Call("cpp_get_blocking",
                  n_massive,
                  2L,
                  nn_indices,
                  1L,
                  1L,
                  PACKAGE = "appopt")
proc.time() - ptm
rm(nn_indices, blocking)
gc()
