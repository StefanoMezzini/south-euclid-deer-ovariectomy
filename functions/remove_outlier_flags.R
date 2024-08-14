remove_outlier_flags <- function(id) {
  d$tel[[which(d$animal == id)]]$outlier <<- 0
}
