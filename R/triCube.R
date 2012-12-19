#'   Tukey's Tricube weight function
#'
#'      Computes the tricube weight function on a vector of distance values (d),
#'      based on a half-window width of h,
#'      and returns a vector of weights that range from zero to 1.
#'
#' @param d numeric vector of distances from the point of estimation to the given sample value
#' @param h numeric value, the half-window width, measured in the same units as d
#' @keywords statistics weighting
#' @return w numeric vector of weights, all 0<=w<=1
#' @export
#' @examples
#'  h<-10
#'  d<-c(-11,-10,-5,-1,-0.01,0,5,9.9,10,20)
#'  triCube(d,h)
triCube<-function(d,h) {
  #  triCube is Tukey tricubed weight function
  #    first argument, d, is a vector of the distances between the observations and the estimation point
  #    second argument, h, is the half window width
  #    it returns a vector of weights (w) for the observations in the vector, d
  n<-length(d)
  zero<-rep(0,n)
  ad<-abs(d)
  w<-(1-(ad/h)^3)^3
  w<-pmax(zero,w)
  return(w)
}