calc_weights <-
function(x,dists,index,nn)
{
    E <- ncol(x)
    u <- exp(-dists[index,nn[index,]]/dists[index,nn[index,1]])
    w <- u / sum(u)
    ## Return weights and time indicies ##
    return(list(w=w,nn=nn[index,]))
}
