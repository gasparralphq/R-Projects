## Load required packages
rm(list = ls())
rm(list = ls())

library(maptools)
library(raster)
library(sp)
library(rgeos)
library(gridExtra)
require(rgdal)
library(jpeg)
library(ncdf4)
library(RColorBrewer)
require(fields)
library(png)
library(repr)

shape<-readOGR("C:/R_/Basemap","phil_byprov_2013") #BASEMAP
muni<- readOGR("C:/R_/Basemap","_minicip_edited_labels_V2") #LABELS



SEASON<-c("DJF", "MAM", "JJA", "SON")
Pert<-c("p10","p50","p90") #Percentile | p10 - Low, p50 - Mid, p90 - High
period = c("mid", "lte") # period mid = 2036 - 2065, late = 2070 - 2099
period2 = c("Mid-21st Century(2036-2065)", "Late-21st Century(2070-2099)")
RCP <- c("RCP45","RCP85")
RCP2 <- c("RCP 4.5","RCP 8.5") #Scenarios
VAR <- c("rr","ta","tx","tn") #rr - Rainfall, ta - Mean Temperature, tx - Maximum Temperature, tn - Minimum Temperature
VARout <-c("Seasonal Rainfall","Seasonal Mean Temperature","Seasonal Maximum Temperature","Seasonal Minimum Temperature")
REGNAME <- c("Region XII - Province of Sarangani") # Region Name
TITLENAMING <- c("Moderate","High")
Rgn = c("R12") #Region Roman?

cbar = brewer.pal(10,"BrBG") #Color Bar
cbar3 = brewer.pal(5,"BrBG")
cbar2 = brewer.pal(9,"OrRd")
Brks <- c(-500,-80,-40,-20,-10,0,10,20,40,80,500)# rainfall - Color Bar ylabel
#Brks3<-c(-100,-80,-60,-40,-20,0,20,40,60,80,100)
Brks2 <- c(0,0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,6) # temperature - Color Bar ylabel

#Rgn=c("NCR","R1","R2","R3","R5","R6","R7","R8","R9","R10","R11","R12","R13","CAR")





################LOOP FOR EVERY NC FILES BASE ON THE FILE NAME #####################################################

for (j in 1:length(RCP)) {
  for (m in 1:length(Pert)) {
    for (n in 1:length(period)) {
      for (l in 1:length(VAR)) {
        
          
          path1 <- paste("G:/CLIRAM new/change_new",RCP[j], sep = "/") #PATH OF NCFILES.
          
          fname1 <-paste("1km", "_", RCP[j], "_", VAR[l], "_", Pert[m] , "_", "DJF", "_", period[n], ".nc", sep = "")
          fname2 <-paste("1km", "_", RCP[j], "_", VAR[l], "_", Pert[m]  , "_", "MAM", "_", period[n], ".nc", sep = "")
          fname3 <-paste("1km", "_", RCP[j], "_", VAR[l], "_", Pert[m]  , "_", "JJA", "_", period[n], ".nc", sep = "")
          fname4 <-paste("1km", "_", RCP[j], "_", VAR[l], "_", Pert[m]  , "_", "SON", "_", period[n], ".nc", sep = "")
          
          
          infile <- paste(path1,fname1, sep = "/")
          infile2 <- paste(path1,fname2, sep = "/")
          infile3 <- paste(path1,fname3, sep = "/")
          infile4 <- paste(path1,fname4, sep = "/")
          
          
          raster1 <- raster(infile)
          raster2 <- raster(infile2)
          raster3 <- raster(infile3)
          raster4 <- raster(infile4)
          
          
          print(infile)
          print(infile2)
          print(infile3)
          print(infile4)
          
          
        
          prov = as.character("SARANGANI") #CHANGE FOR EACH PROVINCE                                                    
          for (o in 1:length(Rgn)) {
            for (p in 1:length(prov)) {             
          
              toplot <- shape[shape$FIRST_REGI == Rgn[o],]
              muniprov<- toplot[which(toplot$PROVINCE == prov[p]),]
              #muniprov<- toplot[toplot$PROVINCE == prov[p],]
              Prov = as.character(muniprov$PROVINCE)
              munic <- muni[muni$PROVINCE %in% Prov,]

        }
        
        ##PATH AND FILE NAME AFTER THE PROCESS/RUN ####################################
        if(RCP[j] =="RCP45"){
         pathout<-paste("C:/Users/IAAS_Leon/Desktop/FINAL OUTPUTS/4seasons-new/Region12/SARANGANI/RCP45/",Rgn[o],prov,RCP[j], VAR[l],Pert[m], sep="_")
        }else{
          pathout<-paste("C:/Users/IAAS_Leon/Desktop/FINAL OUTPUTS/4seasons-new/Region12/SARANGANI/RCP85/",Rgn[o],prov,RCP[j], VAR[l],Pert[m], sep="_")
        }
        
        fnameout<-paste("_",period2[n],".png", sep="")  
        outfile<-paste(pathout,fnameout, sep="")  
        print(outfile)
        png(outfile,height= 10, width= 17, res= 300, units = "in")  
        ##############################################################################
          ##LOOP FOR REGION read the file and crop th region
          
         # pathout<-paste("C:/Users/IAAS_Leon/Desktop/FINAL OUTPUTS/4seasons/NCR/METRO MANILA/",Rgn[o],prov,RCP[j],VAR[l],Pert[m],period[n],sep=" ")
         # fnameout<-paste(" ",period2[n],".png", sep="")  
         # outfile<-paste(pathout,fnameout, sep="")  
         # print(outfile)
         # png(outfile,height= 10, width= 16, res= 200, units = "in")     
          
          
          ##### Start Croping the selected Region #####################################
          
          #Crop 1
          
          bbNCR1 <- crop(raster1, extent(munic))
          ccNCR1 <- mask(bbNCR1,munic ,updateNA = TRUE)
          
          #Crop 2
          bbNCR2 <- crop(raster2, extent(munic))
          ccNCR2 <- mask(bbNCR2,munic ,updateNA = TRUE)
          
          #Crop 3
          bbNCR3 <- crop(raster3, extent(munic))
          ccNCR3 <- mask(bbNCR3,munic ,updateNA = TRUE)
          
          #Crop 4
          bbNCR4 <- crop(raster4, extent(munic))
          ccNCR4 <- mask(bbNCR4,munic ,updateNA = TRUE)
          
          ######################################################################


          #for size paper auto land/portrait adjustments
          
        
          
          
          ################ create 1 row with 4 column ############################
          par(mfrow = c(1,4),oma = c(8, 4, 8, 2))
          
          
          #Plotting of the Cropped Selected Region
          if (VAR[l] == "rr") {
            par(mar = c(1.5,0,8,0)) #Size of plot
            plot(ccNCR1,interpolate = T,box = F,col = cbar, legend = F, axes = F, breaks = Brks, xaxs="r",yaxs="r")
            plot(munic,add = T,lwd = 1.5,border = "black")
            mtext(side = 3, line = 1.5, "Dec-Jan-Feb", cex = 1.1) #label Position and Text
            invisible(text(getSpPPolygonsLabptSlots(munic), labels = as.character(munic$MUNICIPALI), cex =0.8, col = "black", font = 1)) # Labels
            
            par(mar = c(2,0,8,0))
            plot(ccNCR2,interpolate = T,box = F,col = cbar,legend = F,axes = F,breaks = Brks,xaxs="r",yaxs="r")
            plot(munic,add = T,lwd = 1.5,border = "black")
            mtext(side = 3, line = 1.5, "Mar-Apr-May", cex = 1.1)
            invisible(text(getSpPPolygonsLabptSlots(munic), labels = as.character(munic$MUNICIPALI), cex =0.8, col = "black", font = 1))
            
            par(mar = c(2,0,8,0))
            par(ccNCR3,interpolate = T,box = F,col = cbar,legend = F,axes = F,breaks = Brks,xaxs="r",yaxs="r")
            plot(munic,add = T,lwd = 1.5,border = "black")
            mtext(side = 3, line = 1.5, "Jun-Jul-Aug", cex = 1.1)
            invisible(text(getSpPPolygonsLabptSlots(munic), labels = as.character(munic$MUNICIPALI), cex =0.8, col = "black", font = 1))
            par(mar = c(2,0,8,0))
            plot(ccNCR4,interpolate = T,box = F,col = cbar,legend = F,axes = F,breaks = Brks,xaxs="r",yaxs="r")
            plot(munic,add = T,lwd = 1.5,border = "black")
            mtext(side = 3, line = 1.5, "Sep-Oct-Nov", cex = 1.1)
            invisible(text(getSpPPolygonsLabptSlots(munic), labels = as.character(munic$MUNICIPALI), cex =0.8, col = "black", font = 1))
            
            
          }else{
            par(mar = c(1.5,0,8,0)) #Size of Plot
            plot(ccNCR1,interpolate = T,box = F,col = cbar2,legend = F,axes = F,breaks =Brks2,xaxs="r",yaxs="r")
            plot(munic,add = T,lwd = 1.5,border = "black")
            mtext(side = 3, line = 1.5, "Dec-Jan-Feb", cex = 1.1)
            invisible(text(getSpPPolygonsLabptSlots(munic), labels = as.character(munic$MUNICIPALI), cex =0.8, col = "black", font = 1))
            
            par(mar = c(2,0,8,0))
            plot(ccNCR2,interpolate = T,box = F,col = cbar2,legend = F,axes = F,breaks =Brks2,xaxs="r",yaxs="r")
            plot(munic,add = T,lwd = 1.5,border = "black")
            mtext(side = 3, line = 1.5, "Mar-Apr-May", cex = 1.1)
            invisible(text(getSpPPolygonsLabptSlots(munic), labels = as.character(munic$MUNICIPALI), cex =0.8, col = "black", font = 1))
            
            par(mar = c(2,0,8,0))
            plot(ccNCR3,interpolate = T,box = F,col = cbar2,legend = F,axes = F,breaks = Brks2,xaxs="r",yaxs="r")
            plot(munic,add = T,lwd = 1.5,border = "black")
            mtext(side = 3, line = 1.5, "Jun-Jul-Aug", cex = 1.1)
            invisible(text(getSpPPolygonsLabptSlots(munic), labels = as.character(munic$MUNICIPALI), cex =0.8, col = "black", font = 1))
            
            par(mar = c(2,0,8,0))
            plot(ccNCR4,interpolate = T,box = F,col = cbar2,legend = F,axes = F,breaks =Brks2,xaxs="r",yaxs="r")
            plot(munic,add = T,lwd = 1.5,border = "black")
            mtext(side = 3, line = 1.5, "Sep-Oct-Nov", cex = 1.1)
            invisible(text(getSpPPolygonsLabptSlots(munic), labels = as.character(munic$MUNICIPALI), cex =0.8, col = "black", font = 1))
            
          }

          #Legend Color Assigning
          par(
            fig = c(0.05,1,0,0.5),new = TRUE,oma = c(0, 0, 3, 7)
          )
          #if Var = Rainfall(rr) use 
          if (VAR[l] == "rr") {
            #  plot(c(-3:10),c(-3:10),type="n",axes=FALSE,xlab="",ylab="")
            
            plot(
              c(0:10),c(0:10),type = "n",axes = FALSE,xlab = "",ylab = ""
            )
            rect(0,1,1,2,col = cbar[1],border = TRUE)
            jj = 2
            while (jj <= 10) {
              kk <- jj - 1
              mm <- jj - 0.1
              rect(kk,1,jj,2,col = cbar[jj],border = TRUE)
              text(jj - 0.9,1,Brks[jj],cex = 1.5,pos = 1)
              jj <- jj + 1
            }
            
            text(
              4.8,2,paste("Rainfall Change in percent(%)",sep = '      '),cex = 1.5,pos =
                3
            )
            
          }else
          {
            plot(
              c(0:10),c(0:10),type = "n",axes = FALSE,xlab = "",ylab = ""
            )
            
            rect(0,1,1,2,col = cbar2[1],border = TRUE)
            
            jj = 2
            while (jj <= 9) {
              kk <- jj - 1
              mm <- jj - 0.1
              rect(kk,1,jj,2,col = cbar2[jj],border = TRUE)
              
              text(jj - 0.9,1,Brks2[jj],cex = 1.5,pos = 1)
              
              jj <- jj + 1
            }
            
            Unit <- expression(~degree~C)
            text(4.6,2,paste("Temperature Change [  " , "  ]",sep='    '),cex=1.5,pos=3)
            text(5.17,2,~degree~C,cex=1.5,pos=3)
          }
          
          
          if (Pert[m] == "p10") {
            naming <-
              paste(
                "Projected Changes in",VARout[l],"in the",period2[n],"\n relative to 1971-2000 Under", TITLENAMING[j], "Emission Scenario","(", RCP2[j], ")",
                "\n", REGNAME, "\n","Lower Bound (10th Percentile)" ,sep = " "
              )
            
          }else if (Pert[m] == "p50") {
            naming <-
              paste(
                "Projected Changes in",VARout[l],"in the",period2[n],"\n relative to 1971-2000 Under", TITLENAMING[j], "Emission Scenario","(", RCP2[j], ")",
                "\n", REGNAME, "\n", "Median (50th Percentile)" ,sep = " "
              )
            
          }else {
            naming <-
              paste(
                "Projected Changes in",VARout[l],"in the",period2[n],"\n relative to 1971-2000 Under", TITLENAMING[j], "Emission Scenario","(", RCP2[j], ")",
                "\n", REGNAME, "\n","Upper Bound (90th Percentile)", sep = " "
              )
            
          }
          
          print(naming)
          
          mtext(
            naming,line = -7,side = 3,cex = 1.5,outer = TRUE
          )
          
          #### LOGO ##################################################
          #  images = list(source = "C:/workspace/pagasalogo",xref = "paper",yref = "paper",x= 0, y= 1,sizex = 0.2,sizey = 0.2,opacity = 0.8)
          par(oma = c(1, 4, 4, 0))
          img <-
            readPNG("C:/Users/IAAS_Leon/Desktop/trash/logo_for CLIRAM_w source_v3.png") #, native=TRUE, info=TRUE)
          rasterImage(img, 9.7, 0, 10.4, 3.0, interpolate=FALSE)
          #############################################################
          
          #par(oma = c(0, 105, 0, 0))
          #naming2 <- paste("Source:","\n", "CAD-IAAS,", "\n","DOST-PAGASA,","\n", "2017",sep = "")
          #mtext(
          #naming2,side = 1,line = 2, adj = 0.5 ,cex = 0.9, font = 3
          #)
          # mtext(side=2, "Latitude", line=2.5, cex=1)
          
          # grid()
          
          #  img <-readJPEG("C:/workspace/pagasa.png")
          #   rasterImage(img,0,1,0,0.4)
          #  text(125.7,9.9,"By: CAD \n PAGASA 2016 ",adj=c(0,0),cex=0.7,col="gray20")
          
          dev.off()
          
        }
        
      } # end loop period (n) 5
      
    } # end loop season (m) 4
    
  } # end loop var (l) 3
  
} # end loop model(k) 2

# end loop RCP (j) 1


#To close The Graphics and Save the Image
graphics.off()
graphics.off()

