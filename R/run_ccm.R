#' @title Run Convergent Cross Mapping
#' @description Uses a leave one out cross validation to calculate the predictability of X|Y and Y|X.
#' @param X a vector containing series of observations
#' @param Y a vector containing series of observations
#' @param tau the lag a which to generate each lagging vector
#' @param E the number of lags to include
#' @param randomWindow logical indicating how to select the window on which to compute shadow manifolds. If set to FALSE, will use the window beginning at (E-1)tau.
#' @param reps number of repeated random window samples on which to average the cross correlation
#' @return the predicted value of y
#' @keywords predict
#' @seealso \code{\link{calc_weights}} 
#' @examples
#' #str_length(letters)
#' #str_length(c("i", "like", "programming", NA))
run_ccm <-
function(X,Y,tau=1,E=2,randomWindow=FALSE,reps=1)
{
    laggedX<-gen_lagged_vec(X=X,E=E,tau=tau)
    laggedY<-gen_lagged_vec(X=Y,E=E,tau=tau)
    dists_x_all <- as.matrix(dist(laggedX$x))
    dists_y_all <- as.matrix(dist(laggedY$x))
    max_shadow <- dim(dists_x_all)[1]
    Time <- length(X)
    cor_x <- numeric(Time)
    cor_y <- numeric(Time)

    for(L in (5+(E-1)*tau):Time)
    {   
        n_shadow <- L-(E-1)*tau
        pred_x <- numeric(n_shadow)
        pred_y <- numeric(n_shadow)
        actual_x <- numeric(n_shadow)
        actual_y <- numeric(n_shadow) 
        cor_x_tmp <- numeric(reps)
        cor_y_tmp <- numeric(reps)
        for(rep in 1:reps)
        {
            start <- 1
            if(randomWindow)
                start <- sample(1:(max_shadow-n_shadow+1),1)
            window <- start:(start+n_shadow-1)

            dists_x <- dists_x_all[window,window]
            dists_y <- dists_y_all[window,window]

            x=laggedX$x[window,]
            y=laggedY$x[window,]
            nn_x <- get_nn(x,dists_x)
            nn_y <- get_nn(y,dists_y)


            for(i in 1:n_shadow)
            {
                ww <- calc_weights(x=x,dists=dists_x,index=i,nn=nn_x)
                pred_y[i] <- predict_y(y=y,wt=ww)
                actual_y[i] <- y[i,1]

                ww <- calc_weights(x=y,dists=dists_y,index=i,nn=nn_y)
                pred_x[i] <- predict_y(y=x,wt=ww)
                actual_x[i] <- x[i,1] 
            }      
            cor_x_tmp[rep] <- cor(pred_x,actual_x)
            cor_y_tmp[rep] <- cor(pred_y,actual_y)
        }
        cor_x[L] <- mean(cor_x_tmp)
        cor_y[L] <- mean(cor_y_tmp)

        cat(L,'\t',cor_x[L],'\t',cor_y[L],'\n')

    }
    #Truncate at 0
    cor_x[cor_x<0] <- 0
    cor_y[cor_y<0] <- 0
 
    return(list(cor_x=cor_x,cor_y=cor_y))
}
