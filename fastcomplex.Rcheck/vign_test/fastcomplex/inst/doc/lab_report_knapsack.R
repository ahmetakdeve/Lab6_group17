## ----eval=TRUE,results='hide',message=FALSE------------------------------
library(devtools)
install_github("https://github.com/ahmetakdeve/Lab6_group17",
               subdir = "fastcomplex",build_vignettes = TRUE)
library(fastcomplex)

## ----eval=TRUE,results='hide'--------------------------------------------
suppressWarnings(RNGversion("3.5.9"))
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(
  w=sample(1:4000, size = n, replace = TRUE),
  v=runif(n = n, 0, 10000))
knapsack_objects16 <- data.frame(
  w=sample(1:4000, size = 16, replace = TRUE),
  v=runif(n = n, 0, 10000))
knapsack_objects500 <- data.frame(
  w=sample(1:4000, size = 500, replace = TRUE),
  v=runif(n = n, 0, 10000))
knapsack_objects1000000 <- data.frame(
  w=sample(1:4000, size = 1000000, replace = TRUE),
  v=runif(n = n, 0, 10000)
)


## ----eval=TRUE-----------------------------------------------------------
brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500,par=FALSE)

## ----eval=TRUE-----------------------------------------------------------
system.time(brute_force_knapsack(x = knapsack_objects16[1:8,], W = 3500))

## ----eval=TRUE-----------------------------------------------------------
knapsack_dynamic(x = knapsack_objects[1:8,], W = 3500)

## ----eval=TRUE-----------------------------------------------------------
system.time(knapsack_dynamic(x = knapsack_objects500[1:8,], W = 3500))

## ----eval=TRUE-----------------------------------------------------------
greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

## ----eval=TRUE-----------------------------------------------------------
system.time(greedy_knapsack(x = knapsack_objects1000000[1:8,], W = 3500))

