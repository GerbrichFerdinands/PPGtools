# PPGtools

## Example

```{r}
# prepare data
data <- prepInput(rec, channel = "Green", tstart = 20, tstop = 40)

# smooth series z 
lambda <- matrix(10^(seq(1, 7, 2)), dimnames = list(paste0('lambda_', 1:4), NULL))
                
z <- smoothWE(raw_signal = data, lambda = lambda)

# plot result
plotLambda(raw_signal = data, z = z, title = "test")

```


## Package structure
![](flowchart.png)
