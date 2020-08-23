# Cleaning of ONS data

I cleaned ONS data to have a tidy dataset of LAD-wise features regarding the following information:
__life expectancy__
__population__
__suicides__
__wellbeing__
__work__
__gdp__
__unemployment__

You can find an app to explore the app in [this link](https://lucha6.shinyapps.io/ONS2019dash/)

All the code to make the app are available in this repo, so you could also clone this repo then run:

```R
shiny::runApp('ONS2019dash/ONS2019dash.R')
```

To reproduce the data cleaning you can run:

```R
source('01_tidyingOriginalData.R')
```
 
or step through the `01_tidyingOriginalData.Rmd` rmarkdown file.

The rendered rmarkdown file is also available in [this link](https://rpubs.com/lucha6/ONS2019cleaning)
