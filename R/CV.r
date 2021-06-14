CV <- function(...,n=2)
{
  n <- as.numeric(n)
  x <- as.numeric(...)
  return(round(sd(x)/mean(x),digits=n))
}
