get_nn <-
function(x,dists)
{
    E <- ncol(x)
    nn <- t(apply(dists,1,order))
    nn <- nn[,2:(E+2)]
    return(nn)
}
