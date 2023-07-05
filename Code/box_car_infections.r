#function that uses the box cars model to implement delay in death for various gamma shapes

box_car_infections <- function(S, I_vec,lambda, dRt,pRt) {
  n <- length(I_vec)
  dI_vec <- numeric(n)
  dI_vec[1] <- (lambda)*S - pRt * I_vec[1] - dRt * I_vec[1]
  if (n > 1 ) {
    for (i in 2:n) {
      dI_vec[i] <- pRt * I_vec[i-1] - pRt * I_vec[i] - dRt * I_vec[i] 
    }
  }
  return(dI_vec)
}