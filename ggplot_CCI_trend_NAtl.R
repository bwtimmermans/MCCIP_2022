# source("/home/ben/research/NOC/projects/MCCIP_2022/ggplot_CCI_trend_NAtl.R")
   library(ggplot2)
   library(grid)
   library(gridExtra)
   library(RColorBrewer)
   library(colorspace)
   library(fBasics)
   library(abind)

#-------------------------------------------------------#
# Resolution.
   res <- 2
# Specify version.
   ch_version <- "30"
   ch_version <- "11"
# Years.
   lab_years <- "2003-2017"
   lab_years <- "1992-2017"

# Season.
   lab_months <- "annual"
   lab_months <- "JFM"
   lab_months <- "OND"

# Flag for year centering.
   #lab_y_centre <- "winter"
   lab_y_centre <- "summer"

# Flag for regression.
   flag_reg <- "ONI"
   flag_reg <- "NAO"
   flag_reg <- "none"

# Read analysis data (matrix of lists)
# Resolution based.
   data_path <- paste("./output/",res,"deg/list_trend_",ch_version,"_",lab_years,"_",lab_months,"_",lab_y_centre,"_",flag_reg,".Robj",sep="")
   #data_path <- "./output/4deg/list_trend_1992-2018_annual_120_none.Robj"

# Matrix to hold datasets.
   attach(data_path[1])
   attached_data <- list_CCI_trend
   detach(pos=2)

# Set up data structures.
   mat_plot_data <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   mat_plot_CI <- matrix(NA,nrow=length(attached_data[[1]]$lat_mid),ncol=length(attached_data[[1]]$lon_mid))
   lab_dataset <- attached_data[[1]]$dataset_name
   lat_mid <- attached_data[[1]]$lat_mid
   lon_mid <- attached_data[[1]]$lon_mid
   lab_stats <- attached_data[[1]]$trend_stats
   lab_trend <- attached_data[[1]]$trend_labs

   list_data <- attached_data[[2]]

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

# list_CCI_trend dimensions:
# 1,2: lat, lon
# Matrix.
# 3: mean
# 4: year_slope, P-val

# Test for LM and confidence.
   if ( sub_stat_idx == 1 ) {
      for (lat_idx in 1:dim(mat_plot_data)[1]) {
         for (lon_idx in 1:dim(mat_plot_data)[2]) {
# Temporal trend.
            if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_data[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx,sub_stat_idx]] }
# Trend CI.
            if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_CI[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx,2]] }
         }
      }
# Fix for CI info.
      mat_plot_CI[mat_plot_CI > 0.05] <- NA
      mat_plot_CI[!is.na(mat_plot_CI)] <- 1
# Test for SEN and confidence.
   } else if ( sub_stat_idx == 7 ) {
      for (lat_idx in 1:dim(mat_plot_data)[1]) {
         for (lon_idx in 1:dim(mat_plot_data)[2]) {
# Temporal trend.
            if ( !is.na(list_data[lat_idx,lon_idx]) ) { mat_plot_data[lat_idx,lon_idx] <- list_data[[lat_idx,lon_idx]][[stat_idx,sub_stat_idx]] }
# Trend CI.
            if ( !is.na(list_data[lat_idx,lon_idx]) ) { if ( list_data[[lat_idx,lon_idx]][[stat_idx,8]] > 0 | list_data[[lat_idx,lon_idx]][[stat_idx,9]] < 0 ) { mat_plot_CI[lat_idx,lon_idx] <- 1 } }
         }
      }
   }

#-------------------------------------------------------#
# Set up data frame for ggplot.
   df_plot <- NULL
   #plot_labels <- paste(lab_years," (",lab_dataset,"): ",reg_lab," trend in ",lab_stats[stat_idx]," (",lab_months,",",lab_y_centre,",",flag_reg,")",sep="")
   plot_labels <- paste(lab_years," (",lab_dataset," V",ch_version,"): ",reg_lab," trend in ",lab_stats[stat_idx]," (",lab_months,")",sep="")
   for (k in 1:1) {
      df_plot <- rbind( df_plot,
                            cbind( expand.grid( lat=lat_mid, lon=lon_mid ), plot_stat=as.vector(mat_plot_data), plot_CI=as.vector(mat_plot_CI), anal=plot_labels[k] )
                          )
   }

# Custom 3.
   #colfunc <- colorRampPalette(rev(c("darkorchid4","firebrick2","white","royalblue","turquoise3")))
   #seq.plot_cols <- colfunc(17)

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

   seq.plot_cols <- func_cols(5,4)

# Colour ranges.
# Colour ranges for counts.
   if ( stat_idx == 1) {
      #plot_breaks_lo <- 0.9*min(df_plot$plot_stat,na.rm=T)
      #plot_breaks_hi <- 1.1*max(df_plot$plot_stat,na.rm=T)
      #col_breaks <- seq(plot_breaks_lo,plot_breaks_hi,500)
      col_lim <- 0.050
      col_breaks <- seq(-col_lim,col_lim,0.01)
      #plot_lims <- c(0.95*min(df_plot$plot_stat,na.rm=T),1.05*max(df_plot$plot_stat,na.rm=T))
      plot_lims <- c(-col_lim,col_lim)
   } else if ( stat_idx == 2 ) {
      col_breaks <- seq(-0.15,0.15,0.03)
      plot_lims <- c(-0.15,0.15)
      #col_breaks <- seq(-max(df_plot$plot_stat,na.rm=T),max(df_plot$plot_stat,na.rm=T),0.01)
      #plot_lims <- c(-max(df_plot$plot_stat,na.rm=T),max(df_plot$plot_stat,na.rm=T))
   } else if ( stat_idx == 3 ) {
      col_breaks <- seq(-0.15,0.15,0.03)
      plot_lims <- c(-0.15,0.15)
      #col_breaks <- seq(-max(df_plot$plot_stat,na.rm=T),max(df_plot$plot_stat,na.rm=T),0.01)
      #plot_lims <- c(-max(df_plot$plot_stat,na.rm=T),max(df_plot$plot_stat,na.rm=T))
   }

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
                                #name = expression("SD(" * hat(chi) * "*)"),
                                name = paste("Trend\n(m/year)"),
                                limits = plot_lims,
                                breaks=col_breaks,
                                na.value = "black",
                                guide = guide_colorbar(title.position = "bottom",label.position = "left",ticks.colour = "black",ticks.linewidth = 8.0) ) +
# CI information.
         geom_point(aes(x = lon, y = lat, colour = plot_CI), size = 3*res/2, shape = 16) +
         scale_colour_gradientn(colors = c("black","black"), values = c("0.0","1.0"), guide = "none", na.value = "transparent") +
# Map.
         geom_polygon(data = map_data("world"), aes(x = long, y = lat, group = group), color = "#000000", fill = "darkgrey", size = 0.55) +
# Multiple plots.
         facet_wrap(~anal, ncol=2) +
         #ggtitle(plot_labels) +
# Theme stuff.
         theme(axis.title.x=element_blank(),
               axis.title.y=element_blank(),

               strip.text = element_text(size = 50, margin = margin(25,0,25,0)),
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
   fig_file_name <- paste("./figures/CCI_trends/",res,"deg/trend_",reg_lab,"_",lab_dataset,"_V",ch_version,"_",lab_years,"_",lab_months,"_",lab_stats[stat_idx],"_",lab_y_centre,"_",flag_reg,".png",sep="")
   png(filename = fig_file_name, width = 2400, height = 1300)
   plot(p1)
   dev.off()
   system(paste("okular",fig_file_name,"&> /dev/null &"))

