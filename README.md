# Cooperative phenotype predicts political ideology eighteen months later

## Getting Started

### Installing

To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:

```
install.packages(c("car","cowplot","fastDummies","ggforce","ggraph",
                   "haven","igraph","kableExtra","lavaan",
                   "mice","papaja","semTools","targets",
                   "tarchetypes","tidyverse"))
```

Since the data underlying this project are not publicly available, you will also need to contact me to access the data file (scott.claessens@gmail.com). Without it, it will not be possible to run the pipeline or generate the manuscript.

### Executing code

1. Add the data file to the `data` folder
2. Set the working directory to this code repository `setwd("myPath")`
3. Run the pipeline using `targets::tar_make()`
4. To load individual targets into your environment, run `targets::tar_load(targetName)`

## Help

Any issues, please email scott.claessens@gmail.com.

## Authors

Scott Claessens, scott.claessens@gmail.com
