### ModelMap for Ananta
setwd("/Users/madanksuwal/Desktop/From Prakash/Data")
dir()
# load library
library(ModelMap); library(raster); library(quantregForest)
# Creating object names only
read.csv("slide.csv")->slide.df
head(slide.df)


qdatafn <- "slide.csv"  #data file with extracted value "VModelMapData.csv"= slide
qdata.trainfn <- "slide_TRAIN.csv"
qdata.testfn <- "slide_TEST.csv"
#Setting working directory as folder
folder <- getwd()

#Split test (=20%) and traing data 
get.test( proportion.test=0.2,  # 20% for test
          qdatafn=qdatafn, 
          seed=42, folder=folder,
          qdata.trainfn=qdata.trainfn,
          qdata.testfn=qdata.testfn)
#Predictor variables
##Numeric (continous or discrete) predictors name list
predList <- c( "drain",  "elev", "ense", "relief", "plcurv",
               "prcurv", "slide","slope", "spi", "sti", "stype", "twi")
##Factor predictors name list
predFactor <- c("aspect", "depth", "diameter", "fage", "fdens", 
                "ftype", "geolo", "sdc")

# Unique ID
unique.rowname <- "ID"
#Define raster look up table.
rastLUTfn <- "numeric variables.csv" # file with list of numerical variables  "VModelMapData_LUT.csv"
rastLUTfn <- read.table( rastLUTfn,
                         header=FALSE,
                         sep=",",
                         stringsAsFactors=FALSE)
rastLUTfn[,1] <- paste(folder,rastLUTfn[,1],sep="/")

================
### After preparing files, now we go for Quantile Regression Forest
# Quantile Regression Forest
# ModelMap will prepare QRF and RF at once

MODELfn.pinyon <- "VQuantile_QRF_Pinyon" # assigning parameter names
MODELfn.sage <- "VQuantile_QRF_Sage"
response.name.pinyon <- "PINYON"
response.name.sage <- "SAGE"
response.type <- "continuous"
