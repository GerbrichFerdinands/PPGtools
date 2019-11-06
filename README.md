# PPGtools

## Example

```r
# prepare data
data <- prepInput(rec, channel = "Green", tstart = 20, tstop = 40)

# smooth series z 
lambda <-10^(seq(1, 7, 2))
zdata <- smoothWE(data, lambda, d=2, uni = TRUE)

# plot results
title <- "Uniform sampling assumed, d=2, lambda = 10, 10^3, 10^5"
plotLambda(zdata, title)

```


## Package structure
![](flowchart.png)
