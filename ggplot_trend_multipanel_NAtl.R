# source("/home/ben/research/NOC/projects/MCCIP_2022/ggplot_trend_multipanel_NAtl.R")
# Code for MCCIP 2022 report. Multipanel Hs trends, UK close up.

# Librarys.
   library(ggplot2)
   library(grid)
   library(gridExtra)
   library(RColorBrewer)
   library(colorspace)
   library(fBasics)
   library(abind)
# Regression.
   require(zyp,quietly = TRUE)

#-------------------------------------------------------#
# Missions.
   mission_idx <- 3:8
   mission_idx <- 6:10
   mission_idx <- c(2:10)

# Resolution.
   res <- 2

# Data type.
   data_type <- "KU-all"

# Years.
   anal_years <- 1992:2017
   lab_years <- paste(anal_years[1],"-",anal_years[length(anal_years)],sep="")

# Season.
   lab_months <- "JAS"
   vec_months <- 7:9
   lab_months <- "annual"
   vec_months <- 1:12
   lab_months <- "JFM"
   vec_months <- 1:3

# Flag for regression.
   #flag_reg <- "ONI"
   flag_reg <- "none"

# Read analysis data (amtrix of lists).
   vec_datasets <- c("GEOSAT","ERS-1","TOPEX","ERS-2","GFO","JASON-1","ENVISAT","JASON-2","CRYOSAT-2","HY-2","SARAL","JASON-3","SENTINEL-3A")

# Analysis data (CCI).
# 1: mean
# 2: NA
# 3: NA
   stat_idx_1 <- 1
   stat_idx <- 1

# Analysis data (Young).
# 1: Q50
# 2: Q90
# 3: Q95
# 4: Q99
# 5: mean
   stat_idx_2 <- 5

# Analysis data (ERA5).
# 1: mean
   stat_idx_3 <- 1

# Analysis data (NOC).
# 1: mean
   stat_idx_4 <- 1

# Analysis data (GOW).
# 1: mean
   stat_idx_5 <- 1

# Analysis data (ECMWF).
# 1: mean
   stat_idx_6 <- 1

# Flag for year centering (CCI).
   #lab_y_centre <- "n_winter"
   lab_y_centre <- "n_summer"

# Analysis data.
# 1: mean
   stat_idx <- 1
# 1: LM trend
# 7: SEN trend
   sub_stat_idx <- 1

   if ( sub_stat_idx == 1 ) {
      reg_lab <- "LM"
   } else if ( sub_stat_idx == 7 ) {
      reg_lab <- "SEN"
   }

# CI level.
   CI_thresh <- 0.01
   lab_CI_thresh <- 100 * (1 - CI_thresh)

# Normalise difference.
   flag_norm <- TRUE

   if (flag_norm) {
      lab_norm <- "norm"
   } else {
      lab_norm <- "abs"
   }

#=====================================================================================#
   list_data_path <- list(6)
# Data path CCI.
   list_data_path[[1]] <- paste("/home/ben/research/NOC/SRS_wave_analysis/CCI/analysis/output/",res,"deg/list_trend_",lab_years,"_",lab_months,"_",lab_y_centre,"_",flag_reg,".Robj",sep="")

# Data path Young.
# Modifications for monthly data format.
   #list_data_path[[2]] <- paste("/home/ben/research/NOC/SRS_wave_analysis/analysis_NOC/output/",res,"deg/list_trend_",data_type,"_",paste(vec_datasets[mission_idx],collapse='_'),"_",lab_years,"_",lab_months,"_global_",flag_reg,".Robj",sep="")
   #list_data_path[[2]] <- paste("/home/ben/research/NOC/SRS_wave_analysis/analysis_NOC/output/",res,"deg/list_monthly_stats_",data_type,"_",paste(vec_datasets[mission_idx],collapse='_'),"_",lab_years,"_annual_global_meanMED2.Robj",sep="")
   list_data_path[[2]] <- paste("/home/ben/research/NOC/SRS_wave_analysis/analysis_NOC/output/",res,"deg/list_monthly_stats_",data_type,"_",paste(vec_datasets[mission_idx],collapse='_'),"_",lab_years,"_annual_global_meanALL2.Robj",sep="")

# Data path ERA5.
   list_data_path[[3]] <- paste("/home/ben/research/NOC/SRS_wave_analysis/ERA5/analysis/output/",res,"deg/list_trend_1992-2017_",lab_months,"_n_summer_none_mean.Robj",sep="")

# Data path NOC_WW3.
   #list_data_path[[4]] <- "/home/ben/research/NOC/SRS_wave_analysis/NOC_WW3/analysis/output/4deg/list_trend_1992-2015_annual_n_summer_none_1deg.Robj"
   list_data_path[[4]] <- paste("/home/ben/research/NOC/SRS_wave_analysis/NOC_WW3/analysis/output/",res,"deg/list_trend_1992-2015_annual_n_summer_none_1deg.Robj",sep="")

# Data path GOW.
   list_data_path[[5]] <- paste("/home/ben/research/NOC/SRS_wave_analysis/GOW/analysis/output/",res,"deg/list_trend_1992-2017_annual_n_summer_none.Robj",sep="")

# Data path ECMWF.
   list_data_path[[6]] <- paste("/home/ben/research/NOC/SRS_wave_analysis/ECMWF/output/",res,"deg/list_trend_1992-2017_",lab_months,"_n_summer_none_mean.Robj",sep="")

#=====================================================================================#
# Dataset 1 (CCI).

# Matrix to hold datasets.
   attach(list_data_path[[1]])
   attached_data <- list_CCI_trend
   detach(pos=2)

# Set up data structures.
   mat_plot_data_1 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   mat_plot_CI_1 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   lab_dataset_1 <- attached_data[[1]]$dataset_name
   lat_mid_1 <- attached_data[[1]]$lat_mid
   lon_mid_1 <- attached_data[[1]]$lon_mid
   lab_stats_1 <- attached_data[[1]]$trend_stats

# Assign data.
   list_data <- attached_data[[2]]
   rm(attached_data)

# Extract data.
   for (lat_idx in 1:dim(mat_plot_data_1)[1]) {
      for (lon_idx in 1:dim(mat_plot_data_1)[2]) {
# Temporal trend.
         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_data_1[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_1,sub_stat_idx]] }
# Trend CI.
         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_CI_1[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_1,2]] }
      }
   }
# Fix for CI info.
   mat_plot_CI_1[mat_plot_CI_1 > CI_thresh] <- NA
   mat_plot_CI_1[!is.na(mat_plot_CI_1)] <- 1

##-------------------------------------------------------------------------------------#
## Dataset 2 (Young).
#
## Matrix to hold datasets.
#   attach(list_data_path[[2]])
#   attached_data <- list_SRS_trend
#   detach(pos=2)
#
## Set up data structures.
#   mat_plot_data_2 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
#   mat_plot_CI_2 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
#   lab_dataset_2 <- attached_data[[1]]$mission_name
#   lat_mid_2 <- attached_data[[1]]$lat_mid
#   lon_mid_2 <- attached_data[[1]]$lon_mid - 180
#   #lab_stats <- attached_data[[1]]$trend_stats
#   lab_stats_2 <- c(paste("Q",100*c(0.5,0.9,0.95,0.99),sep=""),"mean")
#   lab_period_2 <- attached_data[[1]]$time_period
#
#   list_data <- attached_data[[2]]
#   rm(attached_data)
#
## list_SRS_trend dimensions:
## 1,2: lat, lon
## Matrix.
## 3: Q50,Q90,Q95,Q99,mean
## 4: year_slope, P-val
#   for (lat_idx in 1:dim(mat_plot_data_2)[1]) {
#     for (lon_idx in 1:dim(mat_plot_data_2)[2]) {
## Temporal trend.
#         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_data_2[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_2,sub_stat_idx]] }
## Trend CI.
#         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_CI_2[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_2,2]] }
#      }
#   }
## Fix for CI info.
#   mat_plot_CI_2[mat_plot_CI_2 > CI_thresh] <- NA
#   mat_plot_CI_2[!is.na(mat_plot_CI_2)] <- 1
#
## Flip longitude to centre on Lon = 0.
#   mat_plot_data_2 <- cbind(mat_plot_data_2[,((((360/res)/2)+1):(360/res))],mat_plot_data_2[,1:((360/res)/2)])
#   mat_plot_CI_2 <- cbind(mat_plot_CI_2[,((((360/res)/2)+1):(360/res))],mat_plot_CI_2[,1:((360/res)/2)])
## Flip latitude.
#   mat_plot_data_2 <- mat_plot_data_2[(144/res):1,]
#   mat_plot_CI_2 <- mat_plot_CI_2[(144/res):1,]
#
#-------------------------------------------------------------------------------------#
# Dataset 2 (Young, monthly means)).

# Matrix to hold datasets.
   attach(list_data_path[[2]])
   attached_data <- list_SRS_stats
   detach(pos=2)

# Set up data structures.
   mat_plot_data_2 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   mat_plot_CI_2 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   lab_dataset_2 <- attached_data[[1]]$mission_name
   lat_mid_2 <- attached_data[[1]]$lat_mid
   lon_mid_2 <- attached_data[[1]]$lon_mid - 180

   list_data <- attached_data[[2]]
   rm(attached_data)

# list_SRS_trend dimensions:
# 1,2: lat, lon
# Matrix.
# 3: Q50,Q90,Q95,Q99,mean
# 4: year_slope, P-val
   for (lat_idx in 1:dim(mat_plot_data_2)[1]) {
     for (lon_idx in 1:dim(mat_plot_data_2)[2]) {
         if ( sum(is.na(list_data[[lat_idx,lon_idx]][,vec_months,2])) < 20 )  {
            vec_annual_mean <- apply(X=list_data[[lat_idx,lon_idx]][,vec_months,2],MAR=1,FUN=mean)
# Temporal trend.
            df_Q <- data.frame(cbind(year=anal_years,swh_mean=apply(X=list_data[[lat_idx,lon_idx]][,vec_months,2],MAR=1,FUN=mean)))
            lm_Q <- lm(swh_mean ~ year,data=df_Q)
            sum_lm_Q <- summary(lm_Q)
# Save the regression slope and p-value.
            mat_plot_data_2[lat_idx,lon_idx] <- sum_lm_Q$coefficients[2,1]
            mat_plot_CI_2[lat_idx,lon_idx] <- sum_lm_Q$coefficients[2,4]
            #sen_Q <- eval(parse(text=paste("zyp.sen(",lab_trend_var," ~ year,data=df_Q)",sep='')))
            #mat_trend[qq,7:9] <- c(sen_Q$coefficients[2],confint.zyp(sen_Q,level=0.95)[2,])
         }
      }
   }
# Fix for CI info.
   mat_plot_CI_2[mat_plot_CI_2 > CI_thresh] <- NA
   mat_plot_CI_2[!is.na(mat_plot_CI_2)] <- 1

# Flip longitude to centre on Lon = 0.
   mat_plot_data_2 <- cbind(mat_plot_data_2[,((((360/res)/2)+1):(360/res))],mat_plot_data_2[,1:((360/res)/2)])
   mat_plot_CI_2 <- cbind(mat_plot_CI_2[,((((360/res)/2)+1):(360/res))],mat_plot_CI_2[,1:((360/res)/2)])
# Flip latitude.
   mat_plot_data_2 <- mat_plot_data_2[(144/res):1,]
   mat_plot_CI_2 <- mat_plot_CI_2[(144/res):1,]

#-------------------------------------------------------------------------------------#
# Dataset 3 (ERA5).

# Matrix to hold datasets.
   attach(list_data_path[[3]])
   attached_data <- list_ERA_trend
   detach(pos=2)

# Set up data structures.
   mat_plot_data_3 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   #mat_plot_data_3 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=90)
   mat_plot_CI_3 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   lab_dataset_3 <- attached_data[[1]]$dataset_name
   lat_mid_3 <- attached_data[[1]]$lat_mid
   lon_mid_3 <- attached_data[[1]]$lon_mid
   lab_stats_3 <- attached_data[[1]]$trend_stats

   list_data <- attached_data[[2]]
   rm(attached_data)

# list_ERA_trend dimensions:
# 1,2: lat, lon
# Matrix.
# 3: mean
# 4: year_slope, P-val
   for (lat_idx in 1:dim(mat_plot_data_3)[1]) {
     for (lon_idx in 1:dim(mat_plot_data_3)[2]) {
# Temporal trend.
         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_data_3[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_3,sub_stat_idx]] }
# Trend CI.
         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_CI_3[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_3,2]] }
      }
   }
# Fix for CI info.
   mat_plot_CI_3[mat_plot_CI_3 > CI_thresh] <- NA
   mat_plot_CI_3[!is.na(mat_plot_CI_3)] <- 1
   #mat_plot_CI[is.na(mat_plot_CI)] <- 0

# Flip longitude to centre on Lon = 0.
   mat_plot_data_3 <- cbind(mat_plot_data_3[,((((360/res)/2)+1):(360/res))],mat_plot_data_3[,1:((360/res)/2)])
   mat_plot_CI_3 <- cbind(mat_plot_CI_3[,((((360/res)/2)+1):(360/res))],mat_plot_CI_3[,1:((360/res)/2)])
# Flip latitude.
   mat_plot_data_3 <- mat_plot_data_3[(144/res):1,]
   mat_plot_CI_3 <- mat_plot_CI_3[(144/res):1,]

#-------------------------------------------------------------------------------------#
## Dataset 4 (NOC_WW3).
#
## Matrix to hold datasets.
#   attach(list_data_path[[4]])
#   attached_data <- list_NOC_trend
#   detach(pos=2)
#
## Set up data structures.
#   mat_plot_data_4 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
#   mat_plot_data_4 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=90)
#   mat_plot_CI_4 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
#   lab_dataset_4 <- attached_data[[1]]$dataset_name
#   lat_mid_4 <- attached_data[[1]]$lat_mid
#   lon_mid_4 <- attached_data[[1]]$lon_mid
#   lab_stats_4 <- attached_data[[1]]$trend_stats
#
#   list_data <- attached_data[[2]]
#   rm(attached_data)
#
## list_ERA_trend dimensions:
## 1,2: lat, lon
## Matrix.
## 3: mean
## 4: year_slope, P-val
#   for (lat_idx in 1:dim(mat_plot_data_4)[1]) {
#     for (lon_idx in 1:dim(mat_plot_data_4)[2]) {
## Temporal trend.
#         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_data_4[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_4,1]] }
## Trend CI.
#         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_CI_4[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_4,2]] }
#      }
#   }
## Fix for CI info.
#   mat_plot_CI_4[mat_plot_CI_4 > 0.1] <- NA
#   mat_plot_CI_4[!is.na(mat_plot_CI_4)] <- 1
#
#-------------------------------------------------------------------------------------#
## Dataset 5 (GOW).
#
## Matrix to hold datasets.
#   attach(list_data_path[[5]])
#   attached_data <- list_GOW_trend
#   detach(pos=2)
#
## Set up data structures.
#   mat_plot_data_5 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
#   mat_plot_data_5 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=90)
#   mat_plot_CI_5 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
#   lab_dataset_5 <- attached_data[[1]]$dataset_name
#   lat_mid_5 <- attached_data[[1]]$lat_mid
## Adjust longitude for plotting.
#   lon_mid_5 <- attached_data[[1]]$lon_mid - 180
#   lab_stats_5 <- attached_data[[1]]$trend_stats
#
#   list_data <- attached_data[[2]]
#   rm(attached_data)
#
## list_ERA_trend dimensions:
## 1,2: lat, lon
## Matrix.
## 3: mean
## 4: year_slope, P-val
#   for (lat_idx in 1:dim(mat_plot_data_5)[1]) {
#     for (lon_idx in 1:dim(mat_plot_data_5)[2]) {
## Temporal trend.
#         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_data_5[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_5,1]] }
## Trend CI.
#         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_CI_5[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_5,2]] }
#      }
#   }
## Fix for CI info.
#   mat_plot_CI_5[mat_plot_CI_5 > 0.1] <- NA
#   mat_plot_CI_5[!is.na(mat_plot_CI_5)] <- 1
## Flip longitude to centre on Lon = 0.
#   mat_plot_data_5 <- cbind(mat_plot_data_5[,((((360/res)/2)+1):(360/res))],mat_plot_data_5[,1:((360/res)/2)])
#   mat_plot_CI_5 <- cbind(mat_plot_CI_5[,((((360/res)/2)+1):(360/res))],mat_plot_CI_5[,1:((360/res)/2)])
#
#-------------------------------------------------------------------------------------#
# Dataset 6 (ECMWF).

# Matrix to hold datasets.
   attach(list_data_path[[6]])
   attached_data <- list_ERA_trend
   detach(pos=2)

# Set up data structures.
   mat_plot_data_6 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   #mat_plot_data_6 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=90)
   mat_plot_CI_6 <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   lab_dataset_6 <- attached_data[[1]]$dataset_name
   lat_mid_6 <- attached_data[[1]]$lat_mid
# Adjust longitude for plotting.
   lon_mid_6 <- attached_data[[1]]$lon_mid
   lab_stats_6 <- attached_data[[1]]$trend_stats

   list_data <- attached_data[[2]]
   rm(attached_data)

# list_ERA_trend dimensions:
# 1,2: lat, lon
# Matrix.
# 3: mean
# 4: year_slope, P-val
   for (lat_idx in 1:dim(mat_plot_data_6)[1]) {
     for (lon_idx in 1:dim(mat_plot_data_6)[2]) {
# Temporal trend.
         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_data_6[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_6,1]] }
# Trend CI.
         if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_CI_6[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx_6,2]] }
      }
   }
# Fix for CI info.
   mat_plot_CI_6[mat_plot_CI_6 > CI_thresh] <- NA
   mat_plot_CI_6[!is.na(mat_plot_CI_6)] <- 1
# Flip longitude to centre on Lon = 0.
   mat_plot_data_6 <- cbind(mat_plot_data_6[,((((360/res)/2)+1):(360/res))],mat_plot_data_6[,1:((360/res)/2)])
   mat_plot_CI_6 <- cbind(mat_plot_CI_6[,((((360/res)/2)+1):(360/res))],mat_plot_CI_6[,1:((360/res)/2)])
# Flip latitude.
   mat_plot_data_6 <- mat_plot_data_6[(144/res):1,]
   mat_plot_CI_6 <- mat_plot_CI_6[(144/res):1,]

#-------------------------------------------------------------------------------------#
# Combine all plot data.
   array_plot_data <- abind(mat_plot_data_1,mat_plot_data_2,mat_plot_data_3,mat_plot_data_6,along=3)
   array_plot_CI <- abind(mat_plot_CI_1,mat_plot_CI_2,mat_plot_CI_3,mat_plot_CI_6,along=3)
   #array_plot_data <- abind(mat_plot_data_1,mat_plot_data_2,mat_plot_data_3,mat_plot_data_4,mat_plot_data_5,mat_plot_data_6,along=3)
   #array_plot_CI <- abind(mat_plot_CI_1,mat_plot_CI_2,mat_plot_CI_3,mat_plot_CI_4,mat_plot_CI_5,mat_plot_CI_6,along=3)
# Combine all longitude data.
   list_lon_mid <- list()
   for (d_idx in c(1,2,3,6)) {
      list_lon_mid[[d_idx]] <- eval(parse(text = paste("lon_mid_",d_idx,sep="")))
   }

##-------------------------------------------------------------------------------------#
## Compute difference.
#   if (flag_norm) {
## Data.
#      mat_plot_data <- 100 * (mat_plot_data_1 - mat_plot_data_2) / mat_plot_data_1
## Plot limits.
#      col_lim <- 10.0
#      col_breaks <- seq(-col_lim,col_lim,2.5)
#      plot_lims <- c(-col_lim,col_lim)
## Plot title.
#      #plot_labels <- paste(lab_years," (CCI - Young[+ERS1]): % diff. in climatological mean",sep="")
#      #plot_labels <- paste(lab_years," (CCI - Young[-ERS1]): % diff. in climatological mean",sep="")
#      plot_labels <- paste(lab_years," (CCI - Young[-ERS12]): % diff. in climatological mean",sep="")
#      leg_label <- paste("%")
#   } else {
## Data.
#      mat_plot_data <- mat_plot_data_1 - mat_plot_data_2
## Plot limits.
#      col_lim <- 0.35
#      col_breaks <- seq(-col_lim,col_lim,0.1)
#      plot_lims <- c(-col_lim,col_lim)
## Plot title.
#      #plot_labels <- paste(lab_years," (CCI - Young[+ERS1]): Diff. in climatological mean",sep="")
#      plot_labels <- paste(lab_years," (CCI - Young[-ERS1]): Diff. in climatological mean",sep="")
#      leg_label <- paste("Hs (m)")
#   }
#
#   lat_mid <- lat_mid_1
#   lon_mid <- lon_mid_1
#   
#-------------------------------------------------------------------------------------#
# Set up data frame for ggplot.
   df_plot <- NULL
   plot_labels <- c("(B) CCI2019 [1992-2017]","(A) RY2019 [1992-2017]","(C) ERA5 [1992-2017]","(D) CY46R1 (ERA5) [1992-2017]")
# Melbourne.
   #plot_labels <- c("(A) CCI [1992-2017]","(B) RY2019 [1992-2017]","(C) ERA 5 [1992-2017]","(F) NOC WW3 (ERA5) [1992-2015]","(E) GOW WW3 (CFSR) [1992-2017]","(D) ECMWF WAM (ERA5) [1992-2017]")
   #for (k in 1:dim(array_plot_data)[3]) {
   for (k in c(2,1,3,4)) {
   #for (k in c(2)) {
      df_plot <- rbind( df_plot,
                            cbind( expand.grid( lat=lat_mid_1, lon=lon_mid_1 ), plot_stat=as.vector(array_plot_data[,,k]), plot_CI=as.vector(array_plot_CI[,,k]), anal=plot_labels[k] )
                            #cbind( expand.grid( lat=lat_mid, lon=lon_mid ), plot_stat=as.vector(mat_plot_data), plot_CI=NA, anal=plot_labels[k] )
                          )
   }

# Viridis plus/minus.
   #colfunc <- colorRampPalette(c("#440d53","#3b518b","#20908d","#4ec469","#f0f921","#f9973f","#ca467a","#8323a7","#0d1687"))
   #colfunc <- colorRampPalette(c("#37daf7","#3e88f2","#20908d","#4ec469","#f7f89e","#f9973f","#ca467a","#8323a7","#0d1687"))
   #colfunc <- colorRampPalette(c("#37daf7","#3e88f2","#20908d","#4ec469","#fbfccd","#f9973f","#ca467a","#8323a7","#0d1687"))
   #seq.plot_cols <- colfunc(40)
# Custom 3.
   colfunc <- colorRampPalette(rev(c("firebrick2","white","royalblue")))
   colfunc <- colorRampPalette(rev(c("darkorchid4","firebrick2","white","royalblue","turquoise3")))
   seq.plot_cols <- colfunc(17)

   func_cols <- function(x,y) {
      i_scale_x <- x
      i_scale_y <- y
      colfunc_diff_core <- colorRampPalette(c("royalblue","white","firebrick2"))
      colsC <- colfunc_diff_core(2*i_scale_x+1)
      colfunc_diff_L <- colorRampPalette(c("mediumaquamarine","royalblue"))
      colsL <- colfunc_diff_L(i_scale_y+1)
      colfunc_diff_H <- colorRampPalette(c("firebrick2","darkorchid4"))
      colsH <- colfunc_diff_H(i_scale_y+1)
      cols <- c(colsL[1:i_scale_y],colsC,colsH[2:(i_scale_y+1)])
      return(cols)
   }

   #seq.plot_cols_diff <- colfunc_diff(17)
   seq.plot_cols <- func_cols(5,4)

# Legend label.
   leg_label <- paste("Trend\n(m/year)")

## Colour ranges.
## Colour ranges for counts.
#   if ( stat_idx_1 == 1 | stat_idx == 5 ) {
      col_lim <- 0.040
      col_breaks <- seq(-col_lim,col_lim,0.01)
      #col_breaks <- c(-0.018,-0.012,-0.006,0,0.006,0.012,0.018)
      plot_lims <- c(-col_lim,col_lim)
#   } else if ( stat_idx == 2 ) {
#      col_lim <- 8
#      col_breaks <- seq(0,col_lim,1)
#      plot_lims <- c(0,col_lim)
#   } else if ( stat_idx == 3 ) {
#      col_lim <- 8
#      col_breaks <- seq(0,col_lim,1)
#      plot_lims <- c(0,col_lim)
#   } else if ( stat_idx == 4 ) {
#      col_lim <- 0.08
#      col_breaks <- seq(-col_lim,col_lim,0.02)
#      plot_lims <- c(-col_lim,col_lim)
#   }

# Chi plot.
   p1 <- ggplot(data = df_plot) + #scale_x_continuous(limits = c(-100, -50), expand = c(0,0)) + scale_y_continuous(limits = c(10, 50), expand = c(0,0)) +
         ylab("Latitude\n") + xlab("\nLongitude") +
         #coord_map(projection = "rectangular", lat0 = 0, xlim = c(-180,180), ylim = c(-75,75)) +
         coord_map(xlim = c(-50,20), ylim = c(35,65), projection = "lambert", lat0 = 50, lat1 = 50) +
#  scale_x_continuous(limits = c(-180, 180)) +
#  scale_y_continuous(limits = c(-75, 75)) +
# Fill.
         geom_tile(aes(x = lon, y = lat, fill = plot_stat)) +
	 #geom_point(size = 1, shape = 15 ) +
         scale_fill_gradientn( colours = seq.plot_cols,
                                #name = paste(lab_stats[stat_idx],"Hs (m)"),
                                name = leg_label,
                                limits = plot_lims,
                                breaks=col_breaks,
                                na.value = "black",
                                guide = guide_colorbar(title.position = "bottom",label.position = "left",ticks.colour = "black",ticks.linewidth = 8.0) ) +
# CI information.
         geom_point(aes(x = lon, y = lat, colour = plot_CI), size = 3.0, shape = 16) +
         scale_colour_gradientn(colors = c("black","black"), values = c("0.0","1.0"), guide = "none", na.value = "transparent") +
# Map.
         geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "#000000", fill = "grey", size = 0.55) +
# Multiple plots.
         facet_wrap(~anal, ncol=2) +
         #ggtitle(plot_labels) +
# Theme stuff.
         theme(axis.title.x=element_blank(),
               axis.title.y=element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major = element_blank(),
               panel.background = element_rect(fill = "black"),

               strip.text = element_text(size = 40, margin = margin(25,0,25,0)),
               strip.background = element_rect(fill = "white"),
               panel.spacing.x = unit(1, "lines"),
               panel.spacing.y = unit(2, "lines"),
               axis.text = element_blank(),
               axis.ticks = element_blank(),

               legend.position = "left",
               legend.margin = margin(0,50,0,0),
               legend.key.width = unit(1, "inch"),
               legend.key.height = unit(2, "inch"),
               legend.title = element_text(size = 30, margin = margin(25,0,0,0)),
               legend.title.align = 0.5,
               legend.text = element_text(size = 25, margin = margin(0,0,0,25))
         )

# Include blank panels.
   fig_file_name <- paste("./figures/multipanel_trends/",res,"deg/compare_GRL_",res,"x",res,"_",lab_months,"_CI_",lab_CI_thresh,".png",sep="")
# 4x1
   png(filename = fig_file_name, width = 3000, height = 2000)
   plot(p1)
   dev.off()
   system(paste("okular",fig_file_name,"&> /dev/null &"))

# Write a table for Andy.
#   write.table(x=df_plot, file="./Hs_mean_trends.csv")

