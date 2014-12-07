count <- function(cause = NULL) {
  if (is.null(cause)){
    stop("cause couldn't be NULL")
  }
  alloved <- c("asphyxiation", "blunt force", "other", "shooting",
               "stabbing", "unknown")
  if (cause %in% alloved) {
    homicides <- readLines("homicides.txt")
    r <- regexec("Cause: ([^<]+)", homicides)
    m <- regmatches(homicides, r)
    causes <- sapply(m, function(x) tolower(x[2]))
    return(length(which(causes==cause)))
  }
  else {
    stop("cause isn't in the list of alloved")
  }
}