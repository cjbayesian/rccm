predict_y <-
function(y,wt)
{
    sum(wt$w * y[wt$nn])
}
