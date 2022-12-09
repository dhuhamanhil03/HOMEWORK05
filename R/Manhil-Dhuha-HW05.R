#' Pythagorean Theorem
#'
#' @param a A number
#' @param b A number
#' @param c A number
#' @return Three sides
#' @examples
#' pythagoreanTheorem(25, 34, NA)
#' pythagoreanTheorem(NA, 18, 13)

pythagoreanTheorem <- function(a, b, c) {

  # Checking to see which value is missing
  whichMissing <- is.na(c(a, b, c));

  # If more than one value is missing, throw an error
  if(sum(whichMissing) > 1) {
    stop("Can't have more than one missing value.")
  }

  # If all values are missing, throw an error
  else if(sum(whichMissing) == 0) {
    stop("Can't have all three values.")
  }
  else {
    # If all values aren't numeric, throw an error
    valuesNeeded = c(a,b,c)[!whichMissing];
    if (any(is.na(as.numeric(valuesNeeded)))){
      stop("All values are not numeric.");
    }
  }

  # Calculate C
  if(is.na(c)) {
    c = sqrt(a**2 + b**2);
  }

  # Calculate B
  else if(is.na(b)) {
    b = sqrt(abs(c**2 - a**2));
  }

  # Calculate A
  else if(is.na(a)) {
    a = sqrt(abs(c**2 - b**2));
  }

  return(c(a, b, c))
}

#' Trimmed Mean
#'
#' @param x A vector
#' @param s A number
#' @param l A number
#' @return A number
#' @examples
#' trimmedMean(x, 2, 1)

trimmedMean <- function(x, s, l) {

  # If x doesn't have at least s + l + 1 values, throw an error
  if(length(x) < s+l+1)
    return (stop("X does not have at least s + l + 1 values."))

  # For loop to check if value is smallest
  for(i in 1:s){
    x = x[x != min(x)]
  }

  # For loop to check if value is largest
  for(i in 1:l){
    x = x[x != max(x)]
  }

  # Calculate trimmed mean
  meanValue = sum(x) / length(x)

  # Return value
  return(meanValue)
}
