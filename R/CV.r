CV <- function(...)
{
  x <- as.numeric(...)
  return(sd(x)/mean(x))
}
