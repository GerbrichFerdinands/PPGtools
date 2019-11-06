# PPGtools

## Example

This is still a private GitHub repository.
To install ` PPGtools` on your computer, a token needs to be generated. 

To do this, follow the instructions from the `devtools` package: 

```
# To install from a private repo, use auth_token with a token
# from https://github.com/settings/tokens. You only need the
# repo scope. Best practice is to save your PAT in env var called
# GITHUB_PAT.
```


```r
# generated token 
token <- 

# install package from private GitHub repository 
devtools::install_github(GerbrichFerdinands/PPGtools, auth_token = token, build = TRUE)

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
