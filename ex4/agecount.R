agecount <- function(age = NULL) {
  if (is.null(age)){
    stop("age couldn't be NULL")
  }
  homicides <- readLines("homicides.txt")
  r <- regexec("(\\d+) years old", homicides)
  m <- regmatches(homicides, r)
  causes <- sapply(m, function(x) as.integer(x[2]))
  return(length(which(causes==age)))
}