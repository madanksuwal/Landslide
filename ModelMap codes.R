### ModelMap
setwd("/Users/madanksuwal/Desktop/From Prakash/ModelMapData")
dir()
# load library
library(ModelMap); library(raster); library(quantregForest)
# Creating object names only
qdatafn <- "VModelMapData.csv"
qdata.trainfn <- "VModelMapData_TRAIN.csv"
qdata.testfn <- "VModelMapData_TEST.csv"
#Setting working directory as folder
folder <- getwd()
#Split test (=20%) and traing data 
get.test( proportion.test=0.2,  # 20% for test
          qdatafn=qdatafn, 
          seed=42, folder=folder,
          qdata.trainfn=qdata.trainfn,
          qdata.testfn=qdata.testfn)
#Predictor variables
#Numeric (continous or discrete) predictors name list
predList <- c( "ELEV250",  "NLCD01_250", "EVI2005097",
                  "NDV2005097", "NIR2005097", "RED2005097")
#Factor predictors name list
predFactor <- c("NLCD01_250")
# Unique ID
unique.rowname <- "ID"
#Define raster look up table.
rastLUTfn <- "VModelMapData_LUT.csv"
rastLUTfn <- read.table( rastLUTfn,
                            header=FALSE,
                            sep=",",
                            stringsAsFactors=FALSE)
rastLUTfn[,1] <- paste(folder,rastLUTfn[,1],sep="/")

### After preparing about files, now we go for Quantile Regression Forest
# Quantile Regression Forest
# ModelMap will prepare QRF and RF at once

MODELfn.pinyon <- "VQuantile_QRF_Pinyon" # assigning parameter names
MODELfn.sage <- "VQuantile_QRF_Sage"
response.name.pinyon <- "PINYON"
response.name.sage <- "SAGE"
response.type <- "continuous"

# Build model
QRF.pinyon <- model.build( model.type="QRF",       # quantile RF model
                           qdata.trainfn=qdata.trainfn, # training data
                           folder=folder,          # working directory
                           unique.rowname=unique.rowname, #Row ID set above
                           MODELfn=MODELfn.pinyon, #The file name to use to save files related to the model object
                           predList=predList,      #list of numeric/discrete predictors
                           predFactor=predFactor,  #list of factorial predictors
                           response.name=response.name.pinyon, #Y or response variable
                           response.type=response.type,
                           importance=TRUE,         #prepare variable importance, BUT not available now
                           quantiles=c(0.1,0.2,0.5,0.8,0.9)) #different quantiles

QRF.sage <- model.build( model.type="QRF",
                         qdata.trainfn=qdata.trainfn,
                         folder=folder,
                         unique.rowname=unique.rowname,
                         MODELfn=MODELfn.sage, 
                         predList=predList,
                         predFactor=predFactor,
                         response.name=response.name.sage,  #Y or response variable
                         response.type=response.type,
                         importance=TRUE,
                         quantiles=c(0.1,0.2,0.5,0.8,0.9))
# to extract QRF or RF; not necessory here
QRF.pinyon$QRF
QRF.pinyon$RF

# Diagnostics
QRF.pinyon.pred <- model.diagnostics( model.obj=QRF.pinyon,
                                         qdata.testfn=qdata.testfn,
                                         folder=folder,
                                         MODELfn=MODELfn.pinyon,
                                         unique.rowname=unique.rowname,
                                         quantiles=c(0.1,0.5,0.9),
                                         # Model Validation Arguments
                                         prediction.type="TEST",
                                         device.type=c("pdf","png"),
                                         cex=1.2)

QRF.sage.pred <- model.diagnostics( model.obj=QRF.sage,
                                       qdata.testfn=qdata.testfn,
                                       folder=folder,
                                       MODELfn=MODELfn.sage,
                                       unique.rowname=unique.rowname,
                                       quantiles=c(0.1,0.5,0.9),
                                       # Model Validation Arguments
                                       prediction.type="TEST",
                                       device.type=c("pdf","png"),
                                       cex=1.2)
#Importance Plots
opar <- par(mfrow=c(2,1),mar=c(3,3,3,3),oma=c(0,0,3,0))
Imp.pinyon<-model.importance.plot( model.obj.1=QRF.pinyon$RF,
                                      model.obj.2=QRF.pinyon$QRF,
                                      model.name.1="RFmean",
                                      model.name.2="QRF median",
                                      quantile.2=0.5,
                                      sort.by="predList",
                                      predList=predList,
                                      scale.by="sum",
                                      main="Pinyon Percent Cover",
                                      device.type="none",
                                      cex=0.9)

Imp.sage<-model.importance.plot( model.obj.1=QRF.sage$RF,
                                    model.obj.2=QRF.sage$QRF,
                                    model.name.1="RFmean",
                                    model.name.2="QRF median",
                                    quantile.2=0.5,
                                    sort.by="predList",
                                    predList=predList,
                                    scale.by="sum",
                                    main="Sage Percent Cover",
                                    device.type="none",
                                    cex=0.9)
mtext("Variable Importance Comparison",side=3,line=0,cex=1.8,outer=TRUE)
par(opar)

opar <- par(mfrow=c(2,1),mar=c(3,3,3,3),oma=c(0,0,3,0))
Imp.pinyon<-model.importance.plot( model.obj.1=QRF.pinyon$QRF,
                                      model.obj.2=QRF.pinyon$QRF,
                                      model.name.1="Lower Quantile",
                                      model.name.2="Upper Quantile",
                                      quantile.1=0.1,
                                      quantile.2=0.9,
                                      sort.by="predList",
                                      predList=predList,
                                      scale.by="sum",
                                      main="Pinyon Percent Cover",
                                      device.type="none",
                                      cex=0.9)

Imp.sage<-model.importance.plot( model.obj.1=QRF.sage$QRF,
                                 model.obj.2=QRF.sage$QRF,
                                 model.name.1="Lower Quantile",
                                 model.name.2="Upper Quantile",
                                 quantile.1=0.2,
                                 quantile.2=0.8,
                                 sort.by="predList",
                                 predList=predList,
                                 scale.by="sum",
                                 main="Sage Percent Cover",
                                 device.type="none",
                                 cex=0.9)
mtext("Variable Importance Comparison",side=3,line=0,cex=1.8,outer=TRUE)
par(opar)

# Interaction plot 3D plot
pred.means <- c( ELEV250 = 2300,
                    NLCD01_250 = 42,
                    EVI2005097 = 1800,
                    NDV2005097 = 3100,
                    NIR2005097 = 2000,
                    RED2005097 = 1000)
opar <- par(mfrow=c(3,1),mar=c(2,3,0,2),oma=c(0,0,3,0))
for(quantile in c(0.1,0.5,0.9)){
  model.interaction.plot( QRF.pinyon$QRF,
                          x="ELEV250",
                          y="EVI2005097",
                          main="",
                          plot.type="persp",
                          device.type=c("none"),
                          MODELfn=MODELfn.pinyon,
                          folder=paste(folder,"/interaction",sep=""),
                          quantile=quantile,
                          pred.means=pred.means,
                          zlim=c(0,60))
  mtext(paste( quantile*100,"% Quantile",sep=""),side=2,line=1,font=2,cex=1)
}
mtext("Pinyon Cover by Quantile",side=3,line=1,cex=1.8,outer=TRUE)
par(opar)

#Raster Lookup Table
rastLUTfn <- "VModelMapData_LUT.csv"
rastLUTfn <- read.table( rastLUTfn,
                            header=FALSE,
                            sep=",",
                            stringsAsFactors=FALSE)
rastLUTfn[,1] <- paste(folder,rastLUTfn[,1],sep="/")

#Predict Over the Rasters
model.mapmake( model.obj=QRF.pinyon,
                  folder=folder,
                  MODELfn=MODELfn.pinyon,
                  rastLUTfn=rastLUTfn,
                  na.action="na.omit",
                  # Mapping arguments
                  map.sd=TRUE,
                  quantiles=c(0.1,0.5,0.9))

model.mapmake( model.obj=QRF.sage,
                  folder=folder,
                  MODELfn=MODELfn.sage,
                  rastLUTfn=rastLUTfn,
                  na.action="na.omit",
                  # Mapping arguments
                  map.sd=TRUE,
                  quantiles=c(0.1,0.5,0.9))


#RF Map
l <- seq(100,0,length.out=101)
c <- seq(0,100,length.out=101)
col.ramp <- hcl(h = 120, c = c, l = l)
opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
mapgrid.pinyon <- raster(paste(MODELfn.pinyon,"_map_RF.img",sep=""))
mapgrid.sage <- raster(paste(MODELfn.sage,"_map_RF.img",sep=""))
zlim <- c(0,60)
legend.label<-rev(pretty(zlim,n=5))
legend.colors<-col.ramp[trunc((legend.label/max(legend.label))*100)+1]
legend.label<-paste(legend.label,"%",sep="")
image( mapgrid.pinyon,
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.pinyon,side=3,line=1,cex=1.2)
image( mapgrid.sage,
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.sage,side=3,line=1,cex=1.2)
legend( x=xmax(mapgrid.sage),y=ymax(mapgrid.sage),
           legend=legend.label,
           fill=legend.colors,
           bty="n",
           cex=1.2)
mtext("RF - Mean Percent Cover",side=3,line=1,cex=1.5,outer=T)
par(opar)
opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
mapgrid.pinyon <- brick(paste(MODELfn.pinyon,"_map_QRF.img",sep=""))
mapgrid.sage <- brick(paste(MODELfn.sage,"_map_QRF.img",sep=""))
image( mapgrid.pinyon[[2]],
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.pinyon,side=3,line=1,cex=1.2)
image( mapgrid.sage[[2]],
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.sage,side=3,line=1,cex=1.2)
legend( x=xmax(mapgrid.sage),y=ymax(mapgrid.sage),
           legend=legend.label,
           fill=legend.colors,
           bty="n",
           cex=1.2)
mtext("QRF - Median (50% quantile) Percent Cover",side=3,line=1,cex=1.5,outer=T)
par(opar)
opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
image( mapgrid.pinyon[[1]],
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.pinyon,side=3,line=1,cex=1.2)
image( mapgrid.sage[[1]],
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.sage,side=3,line=1,cex=1.2)
legend( x=xmax(mapgrid.sage),y=ymax(mapgrid.sage),
        legend=legend.label,
        fill=legend.colors,
        bty="n",
        cex=1.2)
mtext("QRF - Lower bound (10% quantile)",side=3,line=1,cex=1.5,outer=T)
par(opar)
opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
image( mapgrid.pinyon[[3]],
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.pinyon,side=3,line=1,cex=1.2)
image( mapgrid.sage[[3]],
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.sage,side=3,line=1,cex=1.2)
legend( x=xmax(mapgrid.sage),y=ymax(mapgrid.sage),
           legend=legend.label,
           fill=legend.colors,
            bty="n",
            cex=1.2)
mtext("QRF model - Upper - 90% quantile",side=3,line=1,cex=1.5,outer=T)
par(opar)

#Conditional Inference Forest
# buidling model
MODELfn.pinyon <- "VQuantile_CF_Pinyon"
MODELfn.sage <- "VQuantile_CF_Sage"
response.name.pinyon <- "PINYON"
response.name.sage <- "SAGE"
response.type <- "continuous"
qdata<-read.csv(qdata.trainfn)
IS.NUM<-sapply(qdata,is.numeric)
qdata[,IS.NUM]<-sapply(qdata[,IS.NUM],as.numeric)
CF.pinyon <- model.build( model.type="CF",
                             qdata.trainfn=qdata,
                             folder=folder,
                             unique.rowname=unique.rowname,
                             MODELfn=MODELfn.pinyon,
                             predList=predList,
                             predFactor=predFactor,
                             response.name=response.name.pinyon,
                             response.type=response.type)
CF.sage <- model.build( model.type="CF",
                        qdata.trainfn=qdata,
                        folder=folder,
                        unique.rowname=unique.rowname,
                        MODELfn=MODELfn.sage,
                        predList=predList,
                        predFactor=predFactor,
                        response.name=response.name.sage,
                        response.type=response.type)
opar <- par(mfrow=c(2,1),mar=c(3,3,3,3),oma=c(0,0,3,0))
#Importance Plots
Imp.pinyon<-model.importance.plot( model.obj.1=CF.pinyon,
                                      model.obj.2=QRF.pinyon$RF,
                                      model.name.1="conditional (CF)",
                                      model.name.2="unconditional (RF)",
                                      cf.conditional.1=FALSE,
                                      sort.by="predList",
                                      predList=predList,
                                      scale.by="sum",
                                      main="Pinyon Percent Cover",
                                      device.type="none",
                                      cex=0.9)
Imp.sage<-model.importance.plot( model.obj.1=CF.sage,
                                    model.obj.2=QRF.sage$RF,
                                    model.name.1="conditional (CF)",
                                    model.name.2="unconditional (RF)",
                                    cf.conditional.1=FALSE,
                                    sort.by="predList",
                                    predList=predList,
                                    scale.by="sum",
                                    main="Sage Percent Cover",
                                    device.type="none",
                                    cex=0.9)
mtext("CF versus RF Variable Importance",side=3,line=0,cex=1.8,outer=TRUE)
par(opar)

#Map production
##Predict Over the Rasters
model.mapmake( model.obj=CF.pinyon,
                  folder=folder,
                  MODELfn=MODELfn.pinyon,
                  rastLUTfn=rastLUTfn,
                  na.action="na.omit"
              )
model.mapmake( model.obj=CF.sage,
                  folder=folder,
               MODELfn=MODELfn.sage,
               rastLUTfn=rastLUTfn,
               na.action="na.omit"
             )
#CF Map
opar <- par(mfrow=c(1,2),mar=c(3,3,2,1),oma=c(0,0,3,4),xpd=NA)
mapgrid.pinyon <- raster(paste(MODELfn.pinyon,"_map.img",sep=""))
mapgrid.sage <- raster(paste(MODELfn.sage,"_map.img",sep=""))
zlim <- c(0,60)
image( mapgrid.pinyon,
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.pinyon,side=3,line=1,cex=1.2)
image( mapgrid.sage,
          col=col.ramp,
          xlab="",ylab="",xaxt="n",yaxt="n",
          zlim=zlim,
          asp=1,bty="n",main="")
mtext(response.name.sage,side=3,line=1,cex=1.2)
legend( x=xmax(mapgrid.sage),y=ymax(mapgrid.sage),
           legend=legend.label,
           fill=legend.colors,
           bty="n",
           cex=1.2)
mtext("CF - Mean Percent Cover",side=3,line=1,cex=1.5,outer=T)
par(opar)
        