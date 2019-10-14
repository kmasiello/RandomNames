#' Random name generator using adjetive-noun pairs
#'
#' @param n The number of names desired
#' @param seed Optional input to set seed for reproducibility.
#'
#' @return A character vector.
#' @export
#'
#'
random_name <- local({
  adjectives <- readLines("adjectives.txt")
  animals <- readLines("animals.txt")


  function(n = 1, seed = Sys.time()) {
    withr::with_seed(as.integer(seed), {
      x <- sample(length(adjectives) * length(animals), n * 2, replace = TRUE)
      adj <- x[seq_along(x) %% 2 == 1] %% length(adjectives) + 1
      ani <- x[seq_along(x) %% 2 == 0] %% length(animals) + 1
      paste0(adjectives[adj], "_", animals[ani])
    })
  }
})


