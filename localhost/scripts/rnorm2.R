# MoFuSS
# Version 3
# Date: Mar 2024

# 2dolist

# Internal parameters

# Load libraries ----
library(msm)
library(raster)
library(tidyverse)
library(readxl)

# Get list of directories starting with 'debugging_'
debugging_to_remove <- list.files(path = ".", pattern = "^debugging_", full.names = TRUE, recursive = FALSE)
# Remove each found directory
sapply(debugging_to_remove, function(folder) {
  unlink(folder, recursive = TRUE)
})

unlink("Debugging", recursive = TRUE,force=TRUE) # BORRAR
unlink("Temp", recursive = TRUE,force=TRUE) # BORRAR
if (!dir.exists("Debugging")) {
  dir.create("Debugging")
}
if (!dir.exists("Temp")) {
  dir.create("Temp")
}

unlink("Summary_Report//Mofuss_Summary_Report.pdf", force=TRUE)
unlink("LaTeX//Mofuss_Summary_Report.pdf", force=TRUE)
unlink("Mofuss_Summary_Report.pdf", force=TRUE)
unlink("LaTeX//InputPara.csv", force=TRUE)
unlink("LaTeX//NRBTable.csv", force=TRUE)
unlink("LaTeX//fNRBTable.csv", force=TRUE)
unlink("LaTeX//SumTable.csv", force=TRUE)

# Read in the arguments listed at the command line in Dinamica EGO'S "Run external process"
args=(commandArgs(TRUE))

# "args" is now a list of character vectors.
# First check to see if arguments are passed.
# Then cycle through each element of the list and evaluate the expressions.
if(length(args)==0){
  print("No arguments supplied by DINAMICA.")
  ##Supply default values here (to be used when running the script through R directly)
  MC=2# MonteCarlo runs
  IT=2010 # Initial year
  K_MC=1
  TOF_MC=1
  Ini_st_MC=1
  Ini_st.factor.percentage=75
  COVER_MAP=1
  rmax_MC=1
  DEF_FW=1
  IL=48 # Iteration length in week - each year = 48 weeks
  # STdyn=20 # Simulation length set by dinamica, but cycles in the repeat functor is STdyn+1 as 2 cycles are needed for 1 year: 1jan->31dec
  Harv.Pix.W=25400
  Prune.W=1
  Harv.Pix.V=25400
  Prune.V=1
  Harv.Pix_MC=10000
  Prune_MC=1
  # Subset_locs=0
  Harvestable_W="Not supplied" 
  Harvestable_V="Not supplied"
  Histograms.per.Fig_FOR=50
  Histograms.per.Fig_TOF=50
  AGBmap=1
  OSType=64
  BaUvsICS="BaU"
  LUCmap_v = 1 
  AGBmap_v = 1
  
}else{
  for(i in 1:length(args)){
    eval(parse(text=args[[i]]))
  }
}

if (BaUvsICS == "ICS") {
  unlink("OutICS//*", recursive = TRUE,force=TRUE)
  unlink("LaTeX//Growth_Harvest_AniOutICS.mp4", force=TRUE)
  unlink("HTML_animation_OutICS//*", recursive = TRUE,force=TRUE)	
  unlink("LaTeX//SumTableICS.csv", force=TRUE)
  unlink("DebuggingICS//*", recursive = TRUE,force=TRUE)
  unlink("TempICS//*", recursive = TRUE,force=TRUE)
  OutDir<-"OutICS"
  DebugDir<-"DebuggingICS"
  TempDir<-"TempICS"
} else {
  unlink("OutBaU//*", recursive = TRUE,force=TRUE)
  unlink("LaTeX//Growth_Harvest_AniOutBaU.mp4", force=TRUE)
  unlink("HTML_animation_OutBaU//*", recursive = TRUE,force=TRUE)	
  unlink("LaTeX//SumTableBaU.csv", force=TRUE)
  unlink("DebuggingBaU//*", recursive = TRUE,force=TRUE)
  unlink("TempBaU//*", recursive = TRUE,force=TRUE)
  OutDir<-"OutBaU"
  DebugDir<-"DebuggingBaU"
  TempDir<-"TempBaU"
  
  for (i in 1:MC) {
    print(i)
    unlink(paste0("debugging_",i), recursive = TRUE,force=TRUE)
  }
  Sys.sleep(15)
  for (i in 1:MC) {
    print(i)
    dir.create(paste0("debugging_",i))
  }
  
  # # To save two digits 
  # for (i in sprintf("%02d", 1:MC)) { 
  #   print(i)
  #   unlink(paste0("debugging_",i), recursive = TRUE,force=TRUE)
  # }
  # Sys.sleep(15)
  # for (i in sprintf("%02d", 1:MC)) {
  #   print(i)
  #   dir.create(paste0("debugging_",i))
  # }
  
}

# Read parameters table ----
read.csv("LULCC/TempTables/Country.csv") %>%
  dplyr::filter(Key. == "1") %>%
  pull(Country) -> country_name

# Specify the directory where the file is located
parameters_directory <- paste0(getwd(),"/LULCC/DownloadedDatasets/SourceData",country_name)

# Use list.files() to find the file that matches the pattern
parameters_name <- list.files(path = parameters_directory, pattern = "^parameters.*\\.csv$", full.names = TRUE)

# Read the Excel file
country_parameters <- read_csv(parameters_name)
print(tibble::as_tibble(country_parameters), n=30)

# if (webmofuss == 1) {
#   # Read parameters table in webmofuss
#   country_parameters <- read_csv(parameters_file_path)
# } else if(webmofuss == 0) {
#   # Read parameters table (recognizing the delimiter)
#   detect_delimiter <- function(file_path) {
#     # Read the first line of the file
#     first_line <- readLines(file_path, n = 1)
#     # Check if the first line contains ',' or ';'
#     if (grepl(";", first_line)) {
#       return(";")
#     } else {
#       return(",")
#     }
#   }
#   # Detect the delimiter
#   delimiter <- detect_delimiter(parameters_file_path)
#   # Read the CSV file with the detected delimiter
#   country_parameters <- read_delim(parameters_file_path, delim = delimiter)
#   print(tibble::as_tibble(country_parameters), n=100)
# }

# country_parameters %>%
#   dplyr::filter(Var == "LULCt1map") %>%
#   pull(ParCHR) -> LULCt1map
# 
# country_parameters %>%
#   dplyr::filter(Var == "LULCt2map") %>%
#   pull(ParCHR) -> LULCt2map
# 
# country_parameters %>%
#   dplyr::filter(Var == "LULCt3map") %>%
#   pull(ParCHR) -> LULCt3map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB1map") %>%
#   pull(ParCHR) -> AGB1map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB2map") %>%
#   pull(ParCHR) -> AGB2map
# 
# country_parameters %>%
#   dplyr::filter(Var == "AGB3map") %>%
#   pull(ParCHR) -> AGB3map

country_parameters %>%
  dplyr::filter(Var == "end_year") %>%
  pull(ParCHR) %>%
  as.integer(.) -> end_year
STdyn <- end_year - IT

if (LUCmap_v == 1) {

    if (file.exists("LULCC/TempTables/growth_parameters1.csv") == TRUE) {
    # Check the first line of the file to determine the delimiter
    first_linegp1 <- readLines("LULCC/TempTables/growth_parameters1.csv", n = 1)
    # Determine the delimiter based on the first line
    delimitergp1 <- ifelse(grepl(";", first_linegp1), ";", ",")
    # Read the CSV file with the appropriate delimiter
    data_all1 <- read.csv("LULCC/TempTables/growth_parameters1.csv", sep = delimitergp1)
    # %>%
    #   mutate(
    #     `Key*` = as.numeric(`Key*`),
    #     rmax = as.numeric(rmax),
    #     rmaxSD = as.numeric(rmaxSD),
    #     K = as.numeric(K),
    #     KSD = as.numeric(KSD),
    #     TOF = as.numeric(TOF)
    #   )
    data_FOR1<-subset(data_all1, data_all1$TOF==0)
    data_TOF1<-subset(data_all1, data_all1$TOF==1)
    #Adjusts for raster resolution - May2023
    data_TOF1r <- data_TOF1
    rasters_res<-xres(raster("LULCC/TempRaster//Mask_c.tif"))
    max_tot1<-nrow(data_all1)
    
    LULC_Categories1<-as.data.frame(data_all1[ ,1])
    colnames(LULC_Categories1)<-("x")
    LULC_Categories1=data.frame(Key=c(1:max_tot1),LULC_Categories1)
    write.csv(LULC_Categories1,"Temp/LULC_Categories1.csv",row.names = FALSE)
  }
  
  # dataTOFvsFOR_1 <- read.csv ("LULCC/TempTables/TOFvsFOR_Categories1.csv")
  # write.csv(dataTOFvsFOR_1,"LULCC/TempTables/TOFvsFOR_Categories.csv",row.names = FALSE)
  
  # TOFvsFOR_mask1 <- raster("LULCC/TempRaster/TOFvsFOR_mask1.tif")
  # writeRaster(TOFvsFOR_mask1, "LULCC/TempRaster/TOFvsFOR_mask.tif", datatype="INT2S", overwrite=TRUE)
  
} else if (LUCmap_v == 2) {
  
  if (file.exists("LULCC/TempTables/growth_parameters2.csv") == TRUE) {
    # Check the first line of the file to determine the delimiter
    first_linegp2 <- readLines("LULCC/TempTables/growth_parameters2.csv", n = 1)
    # Determine the delimiter based on the first line
    delimitergp2 <- ifelse(grepl(";", first_linegp2), ";", ",")
    # Read the CSV file with the appropriate delimiter
    data_all2 <- read.csv("LULCC/TempTables/growth_parameters2.csv", sep = delimitergp2)
    # %>%
    #   mutate(
    #     `Key*` = as.numeric(`Key*`),
    #     rmax = as.numeric(rmax),
    #     rmaxSD = as.numeric(rmaxSD),
    #     K = as.numeric(K),
    #     KSD = as.numeric(KSD),
    #     TOF = as.numeric(TOF)
    #   )
    
    data_FOR2<-subset(data_all2, data_all2$TOF==0)
    data_TOF2<-subset(data_all2, data_all2$TOF==1)
    #Adjusts for raster resolution - May2023
    data_TOF2r <- data_TOF2
    rasters_res<-xres(raster("LULCC/TempRaster//Mask_c.tif"))
    max_tot2<-nrow(data_all2)
    
    LULC_Categories2<-as.data.frame(data_all2[ ,1])
    colnames(LULC_Categories2)<-("x")
    LULC_Categories2=data.frame(Key=c(1:max_tot2),LULC_Categories2)
    write.csv(LULC_Categories2,"Temp/LULC_Categories2.csv",row.names = FALSE)
  }
  
  # dataTOFvsFOR_2 <- read.csv ("LULCC/TempTables/TOFvsFOR_Categories2.csv")
  # write.csv(dataTOFvsFOR_2,"LULCC/TempTables/TOFvsFOR_Categories.csv",row.names = FALSE)
  
  # TOFvsFOR_mask2 <- raster("LULCC/TempRaster/TOFvsFOR_mask2.tif")
  # writeRaster(TOFvsFOR_mask2, "LULCC/TempRaster/TOFvsFOR_mask.tif", datatype="INT2S", overwrite=TRUE)
  
} else if (LUCmap_v == 3) {
  
  # if (file.exists("LULCC/TempTables/growth_parameters3.csv") == TRUE) {
  #   data_semicolon<-read.csv("LULCC/TempTables/growth_parameters3.csv", sep=";", header=T)
  #   data_comma<-read.csv("LULCC/TempTables/growth_parameters3.csv", sep=",", header=T)
  #   if (is.null(data_semicolon$TOF[1])) { 
  #     data_all3<-data_comma
  #   } else {
  #     data_all3<-data_semicolon
  #   }
  #   data_FOR3<-subset(data_all3, data_all3$TOF==0)
  #   data_TOF3<-subset(data_all3, data_all3$TOF==1)
  #   max_tot3<-nrow(data_all3)
  #   
  #   LULC_Categories3<-as.data.frame(data_all3[ ,1])
  #   colnames(LULC_Categories3)<-("x")
  #   LULC_Categories3=data.frame(Key=c(1:max_tot3),LULC_Categories3)
  #   write.csv(LULC_Categories3,"Temp/LULC_Categories3.csv",row.names = FALSE)
  # }
  
  # dataTOFvsFOR_3 <- read.csv ("LULCC/TempTables/TOFvsFOR_Categories3.csv")
  # write.csv(dataTOFvsFOR_3,"LULCC/TempTables/TOFvsFOR_Categories.csv",row.names = FALSE)
  # 
  # TOFvsFOR_mask3 <- raster("LULCC/TempRaster/TOFvsFOR_mask3.tif")
  # writeRaster(TOFvsFOR_mask3, "LULCC/TempRaster/TOFvsFOR_mask.tif", datatype="INT2S", overwrite=TRUE)
  
}

# Rename AGB maps following the version being used
if (AGBmap_v == 1){
  agb_c1 <- raster("LULCC/TempRaster/agb_c1.tif")
  writeRaster(agb_c1, "LULCC/TempRaster/agb_c.tif", datatype="INT4S", overwrite=TRUE)
  
} else if (AGBmap_v == 2){
  agb_c2 <- raster("LULCC/TempRaster/agb_c2.tif")
  writeRaster(agb_c2, "LULCC/TempRaster/agb_c.tif", datatype="INT4S", overwrite=TRUE)
  
} else if (AGBmap_v == 3){
  agb_c3<- raster("LULCC/TempRaster/agb_c3.tif")
  writeRaster(agb_c3, "LULCC/TempRaster/agb_c.tif", datatype="INT4S", overwrite=TRUE)
  
}

if (OSType == 64) {
  ffmpeg_path<-file.path(getwd(),"ffmpeg64/bin/ffmpeg.exe")
} else {
  ffmpeg_path<-file.path(getwd(),"ffmpeg32/bin/ffmpeg.exe")
}

ST = (48/IL*STdyn)+1

print(MC)
print(IT)
print(K_MC)
print(TOF_MC)
print(Ini_st_MC)
print(Ini_st.factor.percentage)
Ini_st.factor = Ini_st.factor.percentage/100
print(COVER_MAP)
print(rmax_MC)
print (DEF_FW)
print(IL)
print(STdyn)
print(ST)
print(Harv.Pix.W)
print(Prune.W)
print(Harv.Pix.V)
print(Prune.V)
print(Harv.Pix_MC)
print(Prune_MC)
print(ffmpeg_path)
# print(Subset_locs)
# print(MaxAGB)
print(AGBmap)

if (K_MC == 1) {
  K_MC_yesno = "Yes"
} else {
  K_MC_yesno = "No"
}

if (TOF_MC == 1) {
  TOF_MC_yesno = "Yes"
} else {
  TOF_MC_yesno = "No"
}

if (Ini_st_MC == 1) {
  Ini_st_MC_yesno = "Yes"
} else {
  Ini_st_MC_yesno = "No"
}

if (COVER_MAP == 1) {
  COVER_MAP_yesno = "Yes"
  Ini_st_MC = 0
  Ini_st_MC_yesno = "Not applicable"
  Ini_st.factor.percentage = "Tree cover as a % of K"
} else {
  COVER_MAP_yesno = "No"
  Ini_st.factor.percentage = paste(Ini_st.factor.percentage,"% of K",sep="")
}

if (rmax_MC == 1) {
  rmax_MC_yesno = "Yes"
} else {
  rmax_MC_yesno = "No"
}

if (DEF_FW == 1) {
  DEF_FW_yesno = "Yes"
} else {
  DEF_FW_yesno = "No"
}

if (AGBmap == 1) {
  AGBmap_yesno = "Yes"
  COVER_MAP_yesno = "Not applicable"
  Ini_st_MC_yesno = "Not applicable"
  Ini_st.factor.percentage = "Using AGB map"
} else {
  AGBmap_yesno = "No"
}

if (OSType == 32) {
  res1000<-100
  res600<-100
  res300<-100
} else {
  res1000<-1000
  res600<-600
  res300<-300
}


# FORESTS ----
# Rename depending on LUCmap

if (LUCmap_v == 1) {
  data_all <- data_all1
  data_FOR <- data_FOR1
  data_TOF <- data_TOF1
  max_tot <- max_tot1
  LULC_Categories <- LULC_Categories1
} else if (LUCmap_v == 2) {
  data_all <- data_all2
  data_FOR <- data_FOR2
  data_TOF <- data_TOF2
  max_tot <- max_tot2
  LULC_Categories <- LULC_Categories2
} else if (LUCmap_v == 3) {
  data_all <- data_all3
  data_FOR <- data_FOR3
  data_TOF <- data_TOF3
  max_tot <- max_tot3
  LULC_Categories <- LULC_Categories3
}

max_FOR<-nrow(data_FOR)

## Histograms 4 forests ----

if (max_FOR!=0) {
  histograms_per_figure_FOR<-Histograms.per.Fig_FOR
  tpaso_FOR<-histograms_per_figure_FOR-1
  height_figure_FOR<-75*histograms_per_figure_FOR/5
  for(j in 1:(ceiling(max_FOR/histograms_per_figure_FOR))) { 
    if (j==1) {
      min5_FOR<-1
      max5_FOR<-j+tpaso_FOR
    } else {
      min5_FOR<-max5_FOR+1
      max5_FOR<-(tpaso_FOR*j)+j
    }
    if (max5_FOR>max_FOR) {
      max5_FOR<-max_FOR
    } else { 
      max5_FOR<-max5_FOR
    }
    
    # rmax values ----
    
    tiff(filename=paste(OutDir,"//Histogram_rmax",j,".tif",sep=""),width=290,height=height_figure_FOR,units="mm",res=res1000,bg="white",
         compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
    n<-layout(matrix(1:histograms_per_figure_FOR, (histograms_per_figure_FOR/5),5, byrow=TRUE))
    par(oma=c(0,0,5,0))  # top has 5 lines of space
    # Set margins to accommodate larger subtitles
    par(mar = c(5, 4, 6, 2))  # bottom, left, top, right
    
    
    for(i in min5_FOR:max5_FOR) {
      if (data_FOR[i,3]==0) {
        textmsg<-"WARNING ERROR IN SUPPLY PARAMETERS csv DATASET"
        write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
      } else {
        lulc1_name<-data_FOR[i,2]
        r1max<-data_FOR[i,3]
        r1maxsd<-data_FOR[i,4]*rmax_MC
        LULC_ID_FOR<-data_FOR[i,1]
        r1<-rtnorm(MC,mean=r1max,sd=r1maxsd,lower=r1max/10)
        r1[1]<-r1max
        
        # Adjust the font size for the histogram titles
        cex_main1 <- 0.3 + (15 / max(nchar(lulc1_name), 15))
        
        hist((r1*100),nclass=15,xlab="rmax",ylab="Frequency",main=lulc1_name,sub=expression("% yr"^{-1}*""),col="grey",cex.main = cex_main1)
        
        r1<-as.data.frame(r1)
        colnames(r1)<-paste("LULC_rmax",LULC_ID_FOR,sep="")
        r1=data.frame(Key=c(1:MC),r1) 
        write.csv(r1,paste("Temp//rmax",LULC_ID_FOR,".csv",sep=""),row.names = FALSE)
      }
    }
    
    Main_Title<-paste("Parameters set by user (Forests and Woodlands):
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno,"   Carrying capacity (K) w/MC?",K_MC_yesno,"  Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno," \n Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"     \n Iteration length =",IL,"weeks (",IL*0.25,"months )     Tree cover map provided?",COVER_MAP_yesno,"     AGB map provided?",AGBmap_yesno,"     Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
    title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
          cex.main = 1,   font.main= 1, col.main= "blue")
    dev.off()
    
    #K values ----
    
    tiff(filename=paste(OutDir,"//Histogram_K",j,".tif",sep=""),width=290,height=height_figure_FOR,units="mm",res=res1000,bg="white",
         compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
    n<-layout(matrix(1:histograms_per_figure_FOR, (histograms_per_figure_FOR/5),5, byrow=TRUE))
    par(oma=c(0,0,5,0))  # top has 5 lines of space
    # Set margins to accommodate larger subtitles
    par(mar = c(5, 4, 6, 2))  # bottom, left, top, right
    
    for(i in min5_FOR:max5_FOR) {
      if (data_FOR[i,3]==0) {
        textmsg<-"WARNING ERROR IN SUPPLY PARAMETERS csv DATASET"
        write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
      } else {
        lulc2_name<-data_FOR[i,2]
        k1max<-data_FOR[i,5]
        k1maxsd<-data_FOR[i,6]*K_MC
        LULC_ID_FOR<-data_FOR[i,1]
        k1<-rtnorm(MC,mean=k1max,sd=k1maxsd,lower=0)
        k1[1]<-k1max
        
        # Adjust the font size for the histogram titles
        cex_main2 <- 0.3 + (15 / max(nchar(lulc2_name), 15))
        
        hist(k1,nclass=15,xlab="K",ylab="Frequency",main=lulc2_name,sub=expression("tDM ha"^{-1}*""),col="grey",cex.main = cex_main2)
        k1<-as.data.frame(k1)
        colnames(k1)<-paste("LULC_K",LULC_ID_FOR,sep="")
        k1=data.frame(Key=c(1:MC),k1) 
        write.csv(k1,paste("Temp//k",LULC_ID_FOR,".csv",sep=""),row.names = FALSE)
      }
    }
    
    Main_Title<-paste("Parameters set by user (Forests and Woodlands):
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno,"   Carrying capacity (K) w/MC?",K_MC_yesno,"  Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno," \n Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"     \n Iteration length =",IL,"weeks (",IL*0.25,"months )     Tree cover map provided?",COVER_MAP_yesno,"     AGB map provided?",AGBmap_yesno,"     Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
    title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
          cex.main = 1,   font.main= 1, col.main= "blue")
    dev.off()
    
    # Initial Stock values----
    
    tiff(filename=paste(OutDir,"//Histogram_ini_stock",j,".tif",sep=""),width=290,height=height_figure_FOR,units="mm",
         res=res1000,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
    n<-layout(matrix(1:histograms_per_figure_FOR, (histograms_per_figure_FOR/5),5, byrow=TRUE))
    par(oma=c(0,0,5,0))  # top has 5 lines of space
    # Set margins to accommodate larger subtitles
    par(mar = c(5, 4, 6, 2))  # bottom, left, top, right
    
    for(i in min5_FOR:max5_FOR) {
      if (data_FOR[i,3]==0) {
        textmsg<-"WARNING ERROR IN SUPPLY PARAMETERS csv DATASET"
        write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
      } else {
        lulc3_name<-data_FOR[i,2]
        inst1<-data_FOR[i,5]*Ini_st.factor
        inst1sd<-data_FOR[i,6]*Ini_st.factor*Ini_st_MC
        LULC_ID_FOR<-data_FOR[i,1]
        st1<-rtnorm(MC,mean=inst1,sd=inst1sd,lower=0)
        st1[1]<-inst1
        
        # Adjust the font size for the histogram titles
        cex_main3 <- 0.3 + (15 / max(nchar(lulc3_name), 15))
        
        hist(st1,nclass=15,xlab="Initial Stock",ylab="Frequency",main=lulc3_name,sub=expression("tDM ha"^{-1}*""),col="grey", cex.main = cex_main3)
        st1<-as.data.frame(st1)
        colnames(st1)<-paste("LULC_IniSt",LULC_ID_FOR,sep="")
        st1=data.frame(Key=c(1:MC),st1) 
        write.csv(st1,paste("Temp//i_st",LULC_ID_FOR,".csv",sep=""),row.names = FALSE)
      }
    }
    
    Main_Title<-paste("Parameters set by user (Forests and Woodlands):
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno,"   Carrying capacity (K) w/MC?",K_MC_yesno,"  Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno," \n Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"     \n Iteration length =",IL,"weeks (",IL*0.25,"months )     Tree cover map provided?",COVER_MAP_yesno,"     AGB map provided?",AGBmap_yesno,"     Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
    title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
          cex.main = 1,   font.main= 1, col.main= "blue")
    dev.off()
    
  } # j hasta ceiling(max_FOR/5)
  
} else {
  textmsg<-"No Forest or Woodland class provided"
  write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
}


#TREES OUTSIDE FORESTS (TOF)----

max_TOF<-nrow(data_TOF)

if (max_TOF!=0) {
  histograms_per_figure_TOF<-Histograms.per.Fig_TOF
  tpaso_TOF<-histograms_per_figure_TOF-1
  height_figure_TOF<-75*histograms_per_figure_TOF/5
  for(j in 1:(ceiling(max_TOF/histograms_per_figure_TOF))) { 
    if (j==1) {
      min5_TOF<-1
      max5_TOF<-j+tpaso_TOF
    } else {
      min5_TOF<-max5_TOF+1
      max5_TOF<-(tpaso_TOF*j)+j
    }
    if (max5_TOF>max_TOF) {
      max5_TOF<-max_TOF
    } else { 
      max5_TOF<-max5_TOF
    }
    
    tiff(filename=paste(OutDir,"//Histogram_TOF",j,".tif",sep=""),width=290,height=height_figure_TOF,units="mm",res=res1000,bg="white",
         compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
    n<-layout(matrix(1:histograms_per_figure_TOF, (histograms_per_figure_TOF/5),5, byrow=TRUE))
    par(oma=c(0,0,5,0))  # top has 5 lines of space
    # Set margins to accommodate larger subtitles
    par(mar = c(5, 4, 6, 2))  # bottom, left, top, right
    
    for(i in min5_TOF:max5_TOF) {
      lulc4_name<-data_TOF[i,2]
      kTOFmax<-data_TOF[i,5]
      kTOFmaxsd<-data_TOF[i,6]*TOF_MC
      LULC_ID_TOF<-data_TOF[i,1]
      kTOF<-rtnorm(MC,mean=kTOFmax,sd=kTOFmaxsd,lower=0)
      kTOF[1]<-kTOFmax
      
      # Adjust the font size for the histogram titles
      cex_main4 <- 0.3 + (15 / max(nchar(lulc4_name), 15))
      
      hist(kTOF,nclass=15,xlab="Available fuelwood from pruning",ylab="Frequency",main=lulc4_name,sub=expression("tDM ha"^{-1}*" yr"^{-1}*""),col="grey", cex.main = cex_main4)
      kTOF<-as.data.frame(kTOF)
      colnames(kTOF)<-paste("LULC_TOF",LULC_ID_TOF,sep="")
      kTOF=data.frame(Key=c(1:MC),kTOF)  
      write.csv(kTOF,paste("Temp//i_st",LULC_ID_TOF,".csv",sep=""),row.names = FALSE)
      write.csv(kTOF,paste("Temp//k",LULC_ID_TOF,".csv",sep=""),row.names = FALSE)
      write.csv(kTOF,paste("Temp//rmax",LULC_ID_TOF,".csv",sep=""),row.names = FALSE)
      print(i)
    }
    
    Main_Title<-paste("Parameters set by user (Trees Outside Forests (TOF)):
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno,"   Carrying capacity (K) w/MC?",K_MC_yesno,"  Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno," \n Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"     \n Iteration length =",IL,"weeks (",IL*0.25,"months )     Tree cover map provided?",COVER_MAP_yesno,"     AGB map provided?",AGBmap_yesno,"     Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
    title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
          cex.main = 1,   font.main= 1, col.main= "blue")
    
    dev.off()
    
  } # j hasta ceiling(max_TOF/5)
  
} else {
  textmsg<-"No Trees Outside Forest class provided"
  write.csv(textmsg,paste(OutDir,"//",textmsg,".csv",sep=""),row.names = FALSE)
}

#TABLES TO BE READ BY DINAMICA ----

k_files1dig <- dir("Temp", pattern = glob2rx("k?.csv"), full.names = TRUE)
k_files2dig <- dir("Temp", pattern = glob2rx("k??.csv"), full.names = TRUE)
k_files3dig <- dir("Temp", pattern = glob2rx("k???.csv"), full.names = TRUE)
k_files<-c(k_files1dig,k_files2dig,k_files3dig)
k_tables <- lapply(k_files, read.csv)

K_all_1 <- do.call(cbind, k_tables)
# Adjust K for pixel resolution (separating FOR from TOF); starting from "by hectare" data
rasters_res <- xres(raster("LULCC/TempRaster//Mask_c.tif"))
res_factor_to_ha <-(rasters_res*rasters_res)/(100*100)
namesFOR <- names(K_all_1)[grep("LULC_K", names(K_all_1))]
K_all_1[,c(namesFOR)]<-K_all_1[,c(namesFOR)]*res_factor_to_ha
namesTOR <- names(K_all_1)[grep("LULC_TOF", names(K_all_1))]
K_all_1[,c(namesTOR)]<-K_all_1[,c(namesTOR)]*res_factor_to_ha 
K_all_2 <- K_all_1[ , grepl( "LULC" , names( K_all_1 ) ) ]
K_all_3 <- data.frame(Key=c(1:MC),K_all_2) %>%
  replace(is.na(.), 0.11111)
write.csv(K_all_3,"Temp//k_all.csv",row.names = FALSE)

rmax_files1dig <- dir("Temp", pattern = glob2rx("rmax?.csv"), full.names = TRUE)
rmax_files2dig <- dir("Temp", pattern = glob2rx("rmax??.csv"), full.names = TRUE)
rmax_files3dig <- dir("Temp", pattern = glob2rx("rmax???.csv"), full.names = TRUE)
rmax_files<-c(rmax_files1dig,rmax_files2dig,rmax_files3dig)
rmax_tables <- lapply(rmax_files, read.csv)

rmax_all_1<-do.call(cbind, rmax_tables)
rmax_all_2<-rmax_all_1[ , grepl( "LULC" , names( rmax_all_1 ) ) ]
#Lo que hay que hacer es multiplicar por resolucion aquellas columnas cuyo nombre tene TOF y listo
rmax_all_3ha=data.frame(Key=c(1:MC),rmax_all_2) 
rmax_all_3 <- rmax_all_3ha %>% 
  mutate_at(vars(matches("LULC_TOF")), function(x){res_factor_to_ha*x}) %>%
  replace(is.na(.), 0.11111)
write.csv(rmax_all_3,"Temp//rmax_all.csv",row.names = FALSE)

inist_files1dig <- dir("Temp", pattern = glob2rx("i_st?.csv"), full.names = TRUE)
inist_files2dig <- dir("Temp", pattern = glob2rx("i_st??.csv"), full.names = TRUE)
inist_files3dig <- dir("Temp", pattern = glob2rx("i_st???.csv"), full.names = TRUE)
inist_files<-c(inist_files1dig,inist_files2dig,inist_files3dig)
inist_tables <- lapply(inist_files, read.csv)

inist_all_1 <- do.call(cbind, inist_tables)
###Adjust Initial Stock for pixel resolution (separating FOR from TOF); starting from "by hectare" data
namesFOR_st <- names(inist_all_1)[grep("LULC_IniSt", names(inist_all_1))]
inist_all_1[,c(namesFOR_st)]<-inist_all_1[,c(namesFOR_st)]*res_factor_to_ha
namesTOR_st <- names(inist_all_1)[grep("LULC_TOF", names(inist_all_1))]
inist_all_1[,c(namesTOR_st)]<-inist_all_1[,c(namesTOR_st)]*res_factor_to_ha
inist_all_2 <- inist_all_1[ , grepl( "LULC" , names( inist_all_1 ) ) ]
inist_all_3 <- data.frame(Key=c(1:MC),inist_all_2) %>%
  replace(is.na(.), 0.11111)
write.csv(inist_all_3,"Temp//i_st_all.csv",row.names = FALSE)

# HARVESTED PIXELS MC----
if (LUCmap_v == 1) {
  LULCMap<-raster("LULCC/TempRaster/LULCt1_c.tif")
} else if (LUCmap_v == 2) {
  LULCMap<-raster("LULCC/TempRaster/LULCt2_c.tif")
} else if (LUCmap_v == 3) {
  LULCMap<-raster("LULCC/TempRaster/LULCt3_c.tif")
}

LULCMap_hist<-freq(LULCMap, digits=0, value=NULL, useNA="ifany", merge=FALSE, progress="window")
LULCMap_hist[ ,2]<-LULCMap_hist[ ,2]*res_factor_to_ha

df1<-as.data.frame(LULCMap_hist)
tot_area<-df1[complete.cases(df1),]
sum(tot_area$count)

df2_TOF<-as.data.frame(data_TOF[ ,1])
colnames(df2_TOF)<-"value"
df3_TOF<-merge(df1, df2_TOF)
df4_TOF<-df3_TOF[complete.cases(df3_TOF),]
TOF_total_area<-sum(df4_TOF$count)

df2_FOR<-as.data.frame(data_FOR[ ,1])
colnames(df2_FOR)<-"value"
df3_FOR<-merge(df1, df2_FOR)
df4_FOR<-df3_FOR[complete.cases(df3_FOR),]
FOR_total_area<-sum(df4_FOR$count)

Harvestable_total_W<-TOF_total_area+FOR_total_area
Harvestable_total_V<-FOR_total_area
# Harvestable_total_V<-TOF_total_area+FOR_total_area

tiff(filename=paste(OutDir,"//Harvested_pixels.tif",sep=""),width=300,height=175,units="mm",res=res1000,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
n<-layout(matrix(1:2, 1,2))
par(oma=c(0,0,5,0))  # top has 5 lines of space


# Harvest Pixels Walking #####

Harv.Pix.W
Harv.Pix.Wsd<-Harv.Pix.W*1*Harv.Pix_MC

if (Harv.Pix.W == 0) {
  Harv.Pix.W
  value1<-c(1:MC)*0
  HP1_0<-data.frame(value1)
  hist(0,nclass=15,xlab=paste("1pixel = ",res_factor_to_ha,"ha (",res_factor_to_ha/100,"km2)",sep=""),ylab="Frequency",main=paste("Harvested pixels every ",IL*0.25," months (walking)",sep=""),
       sub=(paste("Total harvestable area at initial time: ",Harvestable_total_W," ha (",Harvestable_total_W/100," km2)",sep="")),col="grey")
  write.csv(HP1_0,"Temp//Harvest_pixels_W.csv")
} else {
  if (Harv.Pix.W >= Harvestable_total_W) {
    Harv.Pix.W<-Harvestable_total_W-1
  } else {
    Harv.Pix.W
  }
  HP1<-round(rtnorm(MC,mean=Harv.Pix.W,Harv.Pix.Wsd,lower=(Harv.Pix.W/10),upper=Harvestable_total_W),digits=0)
  HP1[1]<-Harv.Pix.W
  hist((HP1),nclass=15,xlab=paste("1pixel = ",res_factor_to_ha,"ha (",res_factor_to_ha/100,"km2)",sep=""),ylab="Frequency",main=paste("Harvested pixels every ",IL*0.25," months (walking)",sep=""),
       sub=(paste("Total harvestable area at initial time: ",Harvestable_total_W," ha (",Harvestable_total_W/100," km2)",sep="")),col="grey")
  HP1<-as.data.frame(HP1)
  colnames(HP1)<-"Value"
  write.csv(HP1,"Temp//Harvest_pixels_W.csv")
}

##### Harvest Pixels Vehicle #####

Harv.Pix.V
Harv.Pix.Vsd<-Harv.Pix.V*1*Harv.Pix_MC

if (Harv.Pix.V == 0) {
  Harv.Pix.V
  value2<-c(1:MC)*0
  HP2_0<-data.frame(value2)
  hist(0,nclass=15,xlab=paste("1pixel = ",res_factor_to_ha,"ha (",res_factor_to_ha/100,"km2)",sep=""),ylab="Frequency",main=paste("Harvested pixels every ",IL*0.25," months (vehicle)",sep=""),
       sub=(paste("Total harvestable area at initial time: ",Harvestable_total_V," ha (",Harvestable_total_V/100," km2)",sep="")),col="grey")
  write.csv(HP2_0,"Temp//Harvest_pixels_V.csv")
} else {
  if (Harv.Pix.V >= Harvestable_total_V) {
    Harv.Pix.V<-Harvestable_total_V-1
  } else {
    Harv.Pix.V
  }
  HP2<-round(rtnorm(MC,mean=Harv.Pix.V,Harv.Pix.Vsd,lower=(Harv.Pix.V/10),upper=Harvestable_total_V),digits=0)
  HP2[1]<-Harv.Pix.V
  hist((HP2),nclass=15,xlab=paste("1pixel = ",res_factor_to_ha,"ha (",res_factor_to_ha/100,"km2)",sep=""),ylab="Frequency",main=paste("Harvested pixels every ",IL*0.25," months (vehicle)",sep=""),
       sub=(paste("Total harvestable area at initial time: ",Harvestable_total_V," ha (",Harvestable_total_V/100," km2)",sep="")),col="grey")
  HP2<-as.data.frame(HP2)
  colnames(HP2)<-"Value"
  write.csv(HP2,"Temp//Harvest_pixels_V.csv")
}


Main_Title<-paste("Parameters set by user:
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno, "   Carrying capacity (K) w/MC?",K_MC_yesno,
                  " \n Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno,"   Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"   Iteration length =",IL,"weeks (",IL*0.25,"months )",
                  " \n Tree cover map provided?",COVER_MAP_yesno,"   AGB map provided?",AGBmap_yesno,"   Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
      cex.main = 1,   font.main= 1, col.main= "blue")
dev.off()


# PRUNE FACTORS MC ----

tiff(filename=paste(OutDir,"//Prune_factors.tif",sep=""),width=300,height=175,units="mm",res=res1000,bg="white",compression=c("lzw"),type=c("windows"),pointsize=12,family="",restoreConsole=TRUE)
n<-layout(matrix(1:2, 1,2))
par(oma=c(0,0,5,0))  # top has 5 lines of space

##### Prune Factor Walking #####

Prune.W
Prune.Wsd<-Prune.W*1*Prune_MC

PF1<-round(rtnorm(MC,mean=Prune.W,Prune.Wsd,lower=0.9), digits=0)
PF1[1]<-Prune.W
hist((PF1),nclass=15,xlab="",ylab="Frequency",main="Prune Factor (walking)",sub=expression(""),col="grey")
PF1<-as.data.frame(PF1)
colnames(PF1)<-"Value"
write.csv(PF1,"Temp//Prune_factor_W.csv")

##### Prune Factor Vehicle #####

Prune.V
Prune.Vsd<-Prune.V*1*Prune_MC

PF2<-round(rtnorm(MC,mean=Prune.V,Prune.Vsd,lower=0.9), digits=0)
PF2[1]<-Prune.V
hist((PF2),nclass=15,xlab="",ylab="Frequency",main="Prune Factor (vehicle)",sub=expression(""),col="grey")
PF2<-as.data.frame(PF2)
colnames(PF2)<-"Value"
write.csv(PF2,"Temp//Prune_factor_V.csv")

Main_Title<-paste("Parameters set by user:
			StartUp year =",IT,"    Sim. length =",ST-1,"yr     MC =",MC,"runs    rmax w/MC?",rmax_MC_yesno, "   Carrying capacity (K) w/MC?",K_MC_yesno,
                  " \n Trees Outside Forests (TOF) w/MC?",TOF_MC_yesno,"   Initial Stock =",Ini_st.factor.percentage,"     Initial Stock w/MC =",Ini_st_MC_yesno,"   Iteration length =",IL,"weeks (",IL*0.25,"months )",
                  " \n Tree cover map provided?",COVER_MAP_yesno,"   AGB map provided?",AGBmap_yesno,"   Accounting for fuelwood from deforestation?",DEF_FW_yesno) 
title(main=Main_Title,line=NA,outer=TRUE,adj=0.5,
      cex.main = 1,   font.main= 1, col.main= "blue")
dev.off()


###############################
### Max AGB value for graphs and animation showing last MC run
###############################

maxAGB_all<-read.csv("Temp//k_all.csv")
MaxAGB<-(max(maxAGB_all, na.rm=TRUE))
write.csv(MaxAGB,"Temp//MaxAGB.csv")
MaxAGB_lastMC<-(max(maxAGB_all[MC, ], na.rm=TRUE))
write.csv(MaxAGB_lastMC,"Temp//MaxAGB_lastMC.csv")
MaxAGB_firstMC<-(max(maxAGB_all[1, ], na.rm=TRUE))
write.csv(MaxAGB_firstMC,"Temp//MaxAGB_firstMC.csv")


# END ----

