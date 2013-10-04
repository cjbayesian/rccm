gen_lagged_vec <-
function(X, tau=1, E=3)
{
    L <- length(X)
    x <- array(dim=c(L-(E-1)*tau,E*tau))
    tmap <- (1+(E-1)*tau):L
    for(i in tmap)
        x[i-((E-1)*tau),] <- X[seq(i,(i-(E-1)*tau),-tau)]

    return(list(x=x,tmap=tmap))
}
