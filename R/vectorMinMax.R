get_min_max <- function(a_vector) {
  vector_max <- max(a_vector)
  vector_min <- min(a_vector)
  max_min_list <- list(max=vector_max, min=vector_min)
  return(max_min_list)
}