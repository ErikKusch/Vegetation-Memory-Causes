################################################################################
####--------------- Fun_Plot [Region, SoilLayer] # all things vegetation memory (maps, varpar, system dynamics)
Fun_Plot <- function(Region, SoilLayer = 1, Scaled = FALSE){
  Region <- unlist(Region)
  ####--------------- FILE SELECTION ----------------
  Rasters <- list()
  for(i in 1:length(Region)){
    Files <- list.files(Dir.Memory)[grep(pattern = Region[i], list.files(Dir.Memory))]
    File <- Files[grep(pattern = paste("Qsoil", SoilLayer, sep=""), Files)]
    Memory <- brick(paste(Dir.Memory,File, sep="/"))
    Rasters[[i]] <- Memory
  }
  ####--------------- MISC ----------------
  SR_Titles <- list("NDVI[t-1]", "Air Temperature", "Soil Moisture (0-7cm)", "Soil Moisture (7-28cm)", "Soil Moisture (28-100cm)", "Soil Moisture (100-255cm)")
  Titles <- c(paste(SR_Titles[1], "(Intrinsic Memory)"), paste(SR_Titles[2+SoilLayer], "(Inverse Resistance)"), paste(SR_Titles[2], "(Inverse Resistance)"))
  TitlesShort <- c(SR_Titles[1][[1]], SR_Titles[2+SoilLayer][[1]], SR_Titles[2][[1]])
  ####--------------- SCALING -----------------
  if(Scaled == TRUE){
    # loop over all different model coefficients
    for(rasterscale in c(1,4:6)){
      maxs <- NA
      mins <- NA
      # identify maxs and mins
      for(scale in 1:length(Region)){
        maxs[scale] <- maxValue(Rasters[[scale]][[rasterscale]])
        mins[scale] <- minValue(Rasters[[scale]][[rasterscale]])
      }
      # fill first and second NA with max and min
      for(scale in 1:length(Region)){
        values(Rasters[[scale]][[rasterscale]])[which(is.na(values(Rasters[[scale]][[rasterscale]])))[1]] <- max(maxs)
        values(Rasters[[scale]][[rasterscale]])[which(is.na(values(Rasters[[scale]][[rasterscale]])))[2]] <- min(mins)
      }
    }
  }
  ####--------------- PLOTTING ----------------
  ##------- MEMORY COMPONENTS -------
  ## Plotting Setup
  col.signeg <- got(n = 100, alpha = 1, begin = 0, end = 1, direction = -1, option = "targaryen2")
  col.sigpos <- got(n = 100, alpha = 1, begin = 0, end = 1, direction = -1, option = "tyrell")
  col.nonsig <- colorRampPalette(c("grey"))(1)
  col.lags <- got(n = 12, alpha = 1, begin = 0, end = 1, direction = 1, option = "daenerys")
  ##------- TRICOLOUR -------
  Tricols <- list()
  for(plot in 1:length(Region)){
    plot_ras <- Rasters[[plot]][[c(1,4:6)]]
    Triplot <- abs(plot_ras[[2:4]])
    TriMap <- ggRGB(img = Triplot, r = 3, g = 1, b = 2, scale = max(maxValue(Triplot)), stretch = 'none') + ggtitle(label = "Relative Importance of Vegetation Responses") + ylab("Latitude [°]") + xlab("Longitude [°]") + theme_bw(base_size = 15)
    Tricols[[plot]] <- TriMap
  
    ## COLOUR BARS 
    # ggframe <- data.frame(Values = as.vector(Triplot),
    #                       Idents = rep(TitlesShort, each = dim(values(Triplot))[1]),
    #                       Order = rep(1:3, each = dim(values(Triplot))[1]))
    # ggframeSmall <- na.omit(ggframe)
    # TriBars <- ggplotGrob(ggplot(ggframeSmall, aes(x = fct_reorder(Idents,Order), y = Values, fill = Idents)) + geom_boxplot() + theme_minimal(base_size = 15) + guides(fill=guide_legend(title="")) + theme(legend.position = c(.6, .9)) + ylab("Coefficients") + 
    #                         theme(panel.background = element_rect(fill = "lightgrey")) + theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
    # )
    # GGCombined <- TriMap +
    #   annotation_custom(
    #     grob = TriBars,
    #     xmin = extent(plot_ras)[2]-5, # these coordinates need altering for different regions
    #     xmax = extent(plot_ras)[2],
    #     ymin = extent(plot_ras)[3],
    #     ymax = extent(plot_ras)[3]+5
    #   )
    ggsave(file=paste(Dir.Plots, "/", Region[plot], "_RelImportance", SoilLayer, ".jpeg", sep = ""), width = 32, height = 22, units = "cm", quality = 100)
  }
  
  ##------- MODEL COEFFICIENTS -------
  ## setting jpeg size
  if(length(Region) == 1){
    height <- 22
    width <- 32
  }else{
    height <- 11*length(Region)
    width <- 16*2
  }
  
  ## plotting to jpeg
  jpeg(file=paste(Dir.Plots, "/", paste(Region,collapse=""), "_Model", SoilLayer, "_", Scaled, ".jpeg", sep = ""), width = width, height = height, units = "cm", quality = 100, res = 1000)
  if(length(Region)>1){
    if(Scaled == TRUE){
      par(mfrow=c(length(Region), 4),
          oma = c(3,0,1,0) + 0.1,
          mar = c(3,.25,3,0) + 0.1)
    }else{
      par(mfrow=c(length(Region), 4),
          oma = c(3,0,1,0) + 0.1,
          mar = c(3,.25,3,0) + 0.1) # margins: bottom, left, top and right 
    }
  }else{
    par(mfrow=c(2,2))
  }
  for(plot in 1:length(Region)){
    plot_ras <- Rasters[[plot]][[c(1,4:6)]]
    if(Scaled == FALSE | Scaled == TRUE & plot == 1){
      mainTit <- paste(SR_Titles[2+SoilLayer][[1]], "(Memory Length)")
    }else{
      mainTit <- ""
    }
    plot(plot_ras[[1]], col = col.lags, colNA = "black", legend=FALSE, axes=FALSE, main = mainTit)
    if(length(Region) > 1 & Scaled == FALSE | Scaled == TRUE & plot == length(Region)){ # plot legend only for last raster stack if we scale
      plot(plot_ras[[1]], legend.only=TRUE, col=col.lags, smallplot=c(0.017, .93, .08, .105), horizontal = TRUE, axis.args=list(cex.axis=1.5))
    }else{
      if(Scaled == FALSE){
        plot(plot_ras[[1]], legend.only=TRUE, col=col.lags, smallplot=c(.05, .93, .15, .185), horizontal = TRUE, axis.args=list(cex.axis=1.5))  
      }
    }
    for(Plot in 2:4){
      Neg_ras <- plot_ras[[Plot]]
      Neg_ras[which(values(Neg_ras) >= 0)] <- NA
      Pos_ras <- plot_ras[[Plot]]
      Pos_ras[which(values(Pos_ras) < 0)] <- NA
      if(Scaled == FALSE | Scaled == TRUE & plot == 1){
        mainTit <- Titles[Plot-1][[1]]
      }else{
        mainTit <- ""
      }
      plot(Neg_ras, col=col.signeg, colNA = "black", legend=FALSE, axes=FALSE, main = mainTit)
      if(Plot == 2 & min(values(plot_ras[[Plot]]), na.rm = TRUE) >= 0){
        plot(Pos_ras, col=col.sigpos, colNA = "black", legend=FALSE, axes=FALSE, add=TRUE)
      }else{
        plot(Pos_ras, col=col.sigpos, legend=FALSE, axes=FALSE, add=TRUE) 
      }
      min <- min(values(plot_ras[[Plot]]), na.rm = TRUE)
      max <- max(values(plot_ras[[Plot]]), na.rm = TRUE)
      range <- abs(min) + max
      if(length(Region) > 1 & Scaled == FALSE | Scaled == TRUE & plot == length(Region)){ # plot legend only for last raster stack if we scale
        minsegment <- .91 * abs(min)/range
        smallplotxpos <- c(.02+minsegment,.93,.08,.105) # where to put colour scales
        smallplotxneg <- c(0.02,.05+minsegment,.08,.105) # where to put colour scales
        plot(Neg_ras, legend.only=TRUE, col=col.signeg, colNA = "black", smallplot=smallplotxneg, horizontal = TRUE, axis.args=list(cex.axis=1.5))
        if(Plot == 2 & min(values(plot_ras[[Plot]]), na.rm = TRUE) >= 0){
          plot(Pos_ras, legend.only=TRUE, col=col.sigpos, smallplot=c(.02, .93,.08,.105), horizontal = TRUE, axis.args=list(cex.axis=1.5))
        }else{
          plot(Pos_ras, legend.only=TRUE, col=col.sigpos, smallplot=smallplotxpos, horizontal = TRUE, axis.args=list(cex.axis=1.5))
        }
      }else{
        minsegment <- .88 * abs(min)/range
        smallplotxpos <- c(.0+minsegment,.92,.15,.185) # where to put colour scales
        smallplotxneg <- c(0.05,.05+minsegment,.15,.185) # where to put colour scales
        if(Scaled == FALSE){
        plot(Neg_ras, legend.only=TRUE, col=col.signeg, colNA = "black", smallplot=smallplotxneg, horizontal = TRUE, axis.args=list(cex.axis=1.5))
        if(Plot == 2 & min(values(plot_ras[[Plot]]), na.rm = TRUE) >= 0){
          plot(Pos_ras, legend.only=TRUE, col=col.sigpos, smallplot=c(.05, .93, .15, .185), horizontal = TRUE, axis.args=list(cex.axis=1.5))
        }else{
          plot(Pos_ras, legend.only=TRUE, col=col.sigpos, smallplot=smallplotxpos, horizontal = TRUE, axis.args=list(cex.axis=1.5))
        } 
        }
      }
    }
  }
  dev.off()
  
  ##------- VARIANCE PARTITIONING -------
  ## Plotting Setup
  col.varpar1 <- got(n = 100, alpha = 1, begin = 0.2, end = 1, direction = -1, option = "wildfire")
  col.varpar2 <- got(n = 100, alpha = 1, begin = 0.2, end = 1, direction = -1, option = "targaryen")
  col.varpar3 <- got(n = 100, alpha = 1, begin = 0.2, end = 1, direction = -1, option = "jon_snow")
  col.list <- list(col.varpar1, col.varpar2, col.varpar3) 
  ## Data
  VarPars <- list()
  for(plot in 1:length(Region)){
    Alter_ras <- Rasters[[plot]][[7:10]]
    Alter_ras[2] <- 0
    values(Alter_ras)[which(values(Alter_ras) < 0)] <- 0
    cells <- order(values(Alter_ras[[1]]))
    `%nin%` = Negate(`%in%`) # create a 'not in' statement
    cells <- cells[which(cells %nin% which(values(Alter_ras[[1]])>quantile(values(Alter_ras[[1]]), .95, na.rm = TRUE)))]
    plot_df <- data.frame(Data =NA, Cell =NA, Variance =NA)
    Idents <- c("Total", "t-1", "Shared", "Qsoil")
    for(i in 1:4){
      if(i > 1){
        plot_df1 <- data.frame(Data = values(Alter_ras[[i]])[cells],
                               Cell = 1:length(cells), 
                               Variance = rep(Idents[i], length(cells)))
        plot_df <- rbind(plot_df, plot_df1)
      }
    }
    Lims <- c(0, max(plot_df$Data, na.rm = TRUE))
    plot_df <- na.omit(plot_df)
    if(length(Region) == 1){
      p <- ggplot(data = plot_df, aes(y = Data, x = Cell, fill = Variance)) + geom_bar(stat = "identity") + theme_bw(base_size= 25) + xlab("Raster Cells") + ylab("Variance") + scale_fill_manual(values=c(col.list[[3]][1], col.list[[2]][50], col.list[[1]][30])) + ylim(Lims)
    }else{
      p <- ggplot(data = plot_df, aes(y = Data, x = Cell, fill = Variance)) + geom_bar(stat = "identity") + theme_bw(base_size= 25) + scale_fill_manual(values=c(col.list[[3]][1], col.list[[2]][50], col.list[[1]][30])) + ylim(Lims) + xlab("Raster Cells") + ylab("Variance") + ggtitle(Region[plot])
    }
    VarPars[[plot]] <- p
  }
  ## Saving Files
  if(length(Region) ==1){
    jpeg(file=paste(Dir.Plots, "/", paste(Region,collapse=""), "_VarPar", SoilLayer, ".jpeg", sep = ""), width = 32, height = 22, units = "cm", quality = 100, res = 100)
    VarPars[[1]]
  }else{
    height <- 22*round(length(Region)/2)
    width <- 44
    jpeg(file=paste(Dir.Plots, "/", paste(Region,collapse=""), "_VarPar", SoilLayer, ".jpeg", sep = ""), width = width, height = height, units = "cm", quality = 100, res = 100)
    
    if(length(Region)==2){
      ggarrange(VarPars[[1]], VarPars[[2]], ncol=2, 
                common.legend = TRUE, legend="bottom",
                labels = "AUTO")
      
    }
    if(length(Region)==3){
      leg <- VarPars[[1]] + theme_bw(base_size = 15) + guides(fill = guide_legend(override.aes = list(size = 15)))
      legend <- cowplot::get_legend(leg)
      grid.arrange(VarPars[[1]]+ theme(legend.position = "none"),
                   VarPars[[2]]+ theme(legend.position = "none"), 
                   VarPars[[3]]+ theme(legend.position = "none"),
                   legend, 
                   ncol=2,
                   left = textGrob("Explained Variance", rot = 90, vjust = 1),
                   bottom = textGrob("Raster Cells", rot = 0, vjust = 1))
    }
    if(length(Region)==4){
      ggarrange(VarPars[[1]], VarPars[[2]], VarPars[[3]], VarPars[[4]], ncol=2, 
                common.legend = TRUE, legend="bottom",
                labels = "AUTO")
    }
    dev.off()
  }
  
  # Maps
  for(plot in 1:length(Region)){
  jpeg(file=paste(Dir.Plots, "/", Region[plot], "_VarParMap", SoilLayer, ".jpeg", sep = ""), width = 32, height = 22, units = "cm", quality = 100, res = 1000)
  par(mfrow=c(2,2))
  Alter_ras <- Rasters[[plot]][[7:10]]
  Alter_ras[2] <- 0
  values(Alter_ras)[which(values(Alter_ras) < 0)] <- 0
  for(i in 2:4){
    col.varpar <- col.list[[i-1]]
    plot(Alter_ras[[i]], col=col.varpar, colNA = "black", legend=FALSE, axes=FALSE, main = Titles[i-1])
    plot(Alter_ras[[i]], legend.only=TRUE, col=col.varpar, smallplot=c(.05, .93, .15, .185), horizontal = TRUE, axis.args=list(cex.axis=2.5))
  } 
  dev.off()
  }
  
  ##------- SYSTEM DYNAMICS -------
  ## Preparation
  # assuming a negative, linear relationship between recovery rates and intrinsic vegetation memory, the higher this proxy, the faster the recovery at constant intrinsic vegetation memory
  InverseProx <- 10
  Equilibrium <- 0
  plot_ras <- Memory[[c(1,4:5)]]
  # Plot Raster for Selection
  Neg_ras <- plot_ras[[3]]
  Neg_ras[which(values(Neg_ras) >= 0)] <- NA
  Pos_ras <- plot_ras[[3]]
  Pos_ras[which(values(Pos_ras) < 0)] <- NA
  plot(Neg_ras, col=col.signeg, colNA = "black", legend=FALSE, axes=FALSE, main = "Select Cells By Clicking and Finish with ESCAPE. Select entire region without clicking on raster and hitting ESCAPE.")
  plot(Pos_ras, col=col.sigpos, legend=FALSE, axes=FALSE, add=TRUE)
  min <- min(values(plot_ras[[3]]), na.rm = TRUE)
  max <- max(values(plot_ras[[3]]), na.rm = TRUE)
  range <- abs(min) + max
  minsegment <- .88 * abs(min)/range
  smallplotxpos <- c(.05+minsegment,.93,.15,.185) # where to put colour scales
  smallplotxneg <- c(0.05,.05+minsegment,.15,.185) # where to put colour scales
  plot(Neg_ras, legend.only=TRUE, col=col.signeg, colNA = "black", smallplot=smallplotxneg, horizontal = TRUE, axis.args=list(cex.axis=2.5))
  plot(Pos_ras, legend.only=TRUE, col=col.sigpos, smallplot=smallplotxpos, horizontal = TRUE, axis.args=list(cex.axis=2.5))
  ## Data
  # Prompt Cell Selection
  Click_Pos <- click(plot_ras[[3]], cell = TRUE)$cell
  if(is.null(Click_Pos)){
    Data_Pos <- which(!is.na(values(plot_ras[[1]])))
  }else{
    Data_Pos <- adjacent(plot_ras[[3]], cells=Click_Pos, directions=16, pairs=FALSE, include = TRUE)
  }
  # Prepare Empty Data Frame and Progress Bar
  plot_df <- data.frame(State = NA, Time = NA, Response = NA, Lag = NA)
  counter <- 0
  print("Extracting Data")
  pb <- txtProgressBar(min = 0, max = length(Data_Pos), style = 3)
  for(i in Data_Pos){
    if(is.na(plot_ras[[3]][i])){#coastline adjacent cells
      counter <- counter + 1
      setTxtProgressBar(pb, counter)
      next()
    }
    if(plot_ras[[3]][i] < 0){
      Identifier <- "Attenuation"
      if(abs(plot_ras[[1]][i]) < 9){
        Lags <- rep(paste("-0",plot_ras[[1]][i]+1, sep=""), 10)
      }else{
        Lags <- rep(paste("",-plot_ras[[1]][i]-1, sep=""), 10)
      }
    }else{
      Identifier <- "Resonance"
      if(plot_ras[[1]][i] < 9){
        Lags <- rep(paste("0",plot_ras[[1]][i]+1, sep=""), 10)
      }else{
        Lags <- rep(paste("",plot_ras[[1]][i]+1, sep=""), 10)
      }
    }
    States <- c(Equilibrium, plot_ras[[3]][i],
                rep(Equilibrium, 8))
    Times <- c(2,2+plot_ras[[1]][i],
               2:9+plot_ras[[1]][i]+abs(plot_ras[[3]][i]*plot_ras[[2]][i]/InverseProx))
    Response <- rep(Identifier, 10)
    plot_df <- rbind(plot_df,
                     data.frame(State = States, Time = Times, Response = Response, Lag = Lags))
    counter <- counter + 1
    setTxtProgressBar(pb, counter)
  }
  plot_df <- na.omit(plot_df)
  plot_df <- rbind(plot_df, data.frame(State=rep(Equilibrium, 2),
                                       Time =c(0,2),
                                       Response = rep("Equilibrium",2),
                                       Lag = rep("00", 2)))
  # colours for different responses and their lags
  colreson <- got(n = 13, alpha = 1, begin = 0, end = .8, direction = 1, option = "tyrell")
  colreson <- colreson[sort(as.numeric(unique(plot_df$Lag)[which(as.numeric(unique(plot_df$Lag)) > 0)]))]
  colatten <- got(n = 13, alpha = 1, begin = .2, end = 1, direction = -1, option = "targaryen2")
  colatten <- colatten[sort(abs(as.numeric(unique(plot_df$Lag))[which(as.numeric(unique(plot_df$Lag)) < 0)]))]
  ## Plotting
  # dynamics
  Lines_gg <- ggplot(plot_df, aes(x = Time, y = State, colour = as.factor(Lag))) +
    geom_line(data = plot_df[which(plot_df$Response == "Equilibrium"),]) +
    stat_smooth(se=.9, fullrange = FALSE, data = plot_df[which(plot_df$Response != "Equilibrium"),], aes(group = Lag), size = .7, alpha = 0.2) +
    theme_bw(base_size = 15) + geom_vline(aes(xintercept = 2), col = "blue") +
    geom_text(aes(x=1.8, label="Positive Soil Moisture Anomaly", y=1.5), colour="blue", angle=90) + scale_colour_manual(values=c(colatten, "black", colreson)) +
    theme(legend.position = "none")
  # insert map
  Mini_ras <- plot_ras[[1]]
  values(Mini_ras)[which(values(plot_ras[[3]])<0)] <- -values(Mini_ras)[which(values(plot_ras[[3]])<0)]-1
  values(Mini_ras)[which(values(plot_ras[[3]])>=0)] <- values(Mini_ras)[which(values(plot_ras[[3]])>=0)]+1
  Miniras_gg <- gplot(Mini_ras) + geom_tile(aes(fill = as.factor(value))) +
    coord_equal() + theme_void(base_size = 15)  +
    scale_fill_manual(values = c(colatten, colreson)) + theme(legend.position = "none")
  # combining
  Combined_gg <- ggdraw() + draw_plot(Lines_gg) + draw_plot(Miniras_gg, x = 0.35, y = 0.33, scale = .3)
  ggsave(file=paste(Dir.Plots, "/", Region, "_Dynamics", SoilLayer, ".jpeg", sep = ""), width = 32, height = 22, units = "cm", quality = 100)
} # end of Fun_Plot

####--------------- Comres [Variable, Region, Legend, SoilLayer] # plots of vegmem and LHTs
Comres <- function(Variable, Region, Legend = TRUE, SoilLayer = 1){
  col.tair <- got(n = 1, alpha = 1, begin = 0, end = 1, direction = -1, option = "tully")
  col.ndvi <- got(n = 1, alpha = 1, begin = 0, end = 1, direction = 1, option = "tyrell")
  col.qsoil <- got(n = 1, alpha = 1, begin = 0, end = 1, direction = 1, option = "white_walkers")
  col.mem <- got(n = 1, alpha = 1, begin = 0, end = 1, direction = -1, option = "greyjoy")
  VarHold <- Variable
  if(Variable == "FSC-1" | Variable == "FSC-2"){
    Variable <- "FastSlow"
  }
  ## Memory Data
  # Memory
  Dir.Reg <- paste(Dir.Memory, "/", Region, "-1981_2015", sep="")
  Files <- list.files(Dir.Reg)[grep(list.files(Dir.Reg), pattern = ".nc")]
  Memory_ras <- brick(paste(Dir.Reg, Files[SoilLayer], sep="/"))[[1]]
  # t-1
  Dir.Reg <- paste(Dir.Memory, "/", Region, "-1981_2015", sep="")
  Files <- list.files(Dir.Reg)[grep(list.files(Dir.Reg), pattern = ".nc")]
  NDVI_ras <- brick(paste(Dir.Reg, Files[SoilLayer], sep="/"))[[2]]
  # Qsoil
  Dir.Reg <- paste(Dir.Memory, "/", Region, "-1981_2015", sep="")
  Files <- list.files(Dir.Reg)[grep(list.files(Dir.Reg), pattern = ".nc")]
  Qsoil_ras <- brick(paste(Dir.Reg, Files[SoilLayer], sep="/"))[[3]]
  # Tair
  Dir.Reg <- paste(Dir.Memory, "/", Region, "-1981_2015", sep="")
  Files <- list.files(Dir.Reg)[grep(list.files(Dir.Reg), pattern = ".nc")]
  Tair_ras <- brick(paste(Dir.Reg, Files[SoilLayer], sep="/"))[[4]]
  ## COMPADRE data
  Dir.Comp <- paste(Dir.Compadre, Variable, sep="/")
  Compad_ras <- list.files(Dir.Comp)[grep(list.files(Dir.Comp), pattern = Region)]
  if(VarHold != "FSC-2"){
    Compad_ras <- raster(paste(Dir.Comp, Compad_ras, sep="/"))[[1]]
  }else{
    Compad_ras <- brick(paste(Dir.Comp, Compad_ras, sep="/"))[[2]]
  }
  values(Compad_ras)[which(values(Compad_ras) > quantile(values(Compad_ras), .66, na.rm = TRUE))] <- quantile(values(Compad_ras), .66, na.rm = TRUE)
  if(VarHold == "FSC-1" | VarHold == "FSC-2"){
    values(Compad_ras)[which(values(Compad_ras) < quantile(values(Compad_ras), .05, na.rm = TRUE))] <- quantile(values(Compad_ras), .05, na.rm = TRUE) 
  }
  plot_df <- data.frame(
    Data = c(values(Memory_ras), values(NDVI_ras), values(Qsoil_ras), values(Tair_ras)), 
    Identifiers = rep(c("Lag", "t-1", "Qsoil1", "Tair"), each = length(values(Memory_ras))),
    Compadre = rep(values(Compad_ras), 4)
  )
  Variable <- VarHold
  plot_df <- na.omit(plot_df)
  Output <- as.list(rep(NA,16))
  Idents <- c("t-1", "Tair", "Qsoil1", "Lag")
  for(i in 0:(length(Idents)-1)){
    Output[[(4*i+1)]] <- summary(lm(Data ~ Compadre, data = plot_df[which(plot_df$Identifiers == Idents[(i+1)]),]))[["coefficients"]][1,1]
    Output[[(4*i+2)]] <- summary(lm(Data ~ Compadre, data = plot_df[which(plot_df$Identifiers == Idents[(i+1)]),]))[["coefficients"]][1,4]
    Output[[(4*i+3)]] <- summary(lm(Data ~ Compadre, data = plot_df[which(plot_df$Identifiers == Idents[(i+1)]),]))[["coefficients"]][2,1]
    Output[[(4*i+4)]] <- summary(lm(Data ~ Compadre, data = plot_df[which(plot_df$Identifiers == Idents[(i+1)]),]))[["coefficients"]][2,4]
  }
  Linesa <- rep(1,4)
  Linesa[which(unlist(Output)[c(4,8,12,16)] > .05)] <- 2
  Lines <- c(Linesa[4], Linesa[1], Linesa[3], Linesa[2])
  Lines <- rep(Lines, each = length(which(plot_df$Identifiers == "Lag")))
  Lines <- as.factor(Lines)
  plot_df <- cbind(plot_df, Lines)
  if(length(levels(Lines)) == 2){
    if(Legend == TRUE){
      plot <- ggplot(data = plot_df, aes(x = Compadre, y = Data, col = Identifiers, linetype = Lines)) + geom_point(alpha = 0.5, size = 3.5) + theme_bw(base_size = 25) +  xlab(paste("COMPADRE", Variable)) + ylab("Vegetation Response Coefficients") + geom_hline(yintercept = 0, linetype="dotted") + stat_smooth(method = "lm", level = 0.66, aes(linetype = Lines)) + scale_color_manual(values=c(col.mem, col.qsoil, col.ndvi, col.tair)) + 
        labs(linetype="p < .05", colour="Identifiers") + scale_linetype_manual(values = c(1, 2), labels = c("Yes", "No")) + guides(colour = guide_legend(ncol=2, override.aes = list(size = 5)),linetype = guide_legend(override.aes = list(size = 3)))
    }else{
      plot <- ggplot(data = plot_df, aes(x = Compadre, y = Data, col = Identifiers, linetype = Lines)) + geom_point(alpha = 0.5, size = 3.5) + theme_bw(base_size = 25) +  xlab(paste("COMPADRE", Variable)) + ylab("Vegetation Response Coefficients") + geom_hline(yintercept = 0, linetype="dotted") + stat_smooth(method = "lm", level = 0.66, aes(linetype = Lines)) + scale_color_manual(values=c(col.mem, col.qsoil, col.ndvi, col.tair)) + theme(legend.position = "none")
    }
  }else{
    if(Legend == TRUE){
      plot <- ggplot(data = plot_df, aes(x = Compadre, y = Data, col = Identifiers, linetype = Lines)) + geom_point(alpha = 0.5, size = 3.5) + theme_bw(base_size = 25) +  xlab(paste("COMPADRE", Variable)) + ylab("Vegetation Response Coefficients") + geom_hline(yintercept = 0, linetype="dotted") + stat_smooth(method = "lm", level = 0.66, linetype = 2) + scale_color_manual(values=c(col.mem, col.qsoil, col.ndvi, col.tair)) + 
        labs(linetype="p < .05", colour="Identifiers") + scale_linetype_manual(values = c(1, 2), labels = c("Yes", "No")) + guides(colour = guide_legend(ncol=2, override.aes = list(size = 5)),linetype = guide_legend(override.aes = list(size = 2)))
    }else{
      plot <- ggplot(data = plot_df, aes(x = Compadre, y = Data, col = Identifiers, linetype = Lines)) + geom_point(alpha = 0.5, size = 3.5) + theme_bw(base_size = 25) +  xlab(paste("COMPADRE", Variable)) + ylab("Vegetation Response Coefficients") + geom_hline(yintercept = 0, linetype="dotted") + stat_smooth(method = "lm", level = 0.66, linetype = 2) + scale_color_manual(values=c(col.mem, col.qsoil, col.ndvi, col.tair)) + theme(legend.position = "none")
    }
  }
  ## Preparing table
  Output <- unlist(Output)
  tabout <- data.frame(Column = Output[1:2])
  starts <- seq(1,length(Output), by = 2)
  for(i in 2:(length(Output)/2)){
    tabout <- cbind(tabout, Output[starts[i]:(starts[i]+1)])
  }
  colnames(tabout) <- paste(rep(c("t-1", "Tair", "Qsoil", "Lag"), each = 2),rep(c("I", "S"), 4), sep="_")
  rownames(tabout) <- c(paste("V (",dim(plot_df)[1]/4, ")", sep="") , "p-Value")
  tabout <- round(tabout,5)
  ## Plotting
  tbl <- tableGrob(tabout, theme = ttheme_minimal())
  # Plot chart and table into one object
  combinedplot <- grid.arrange(plot, tbl,
                               nrow=2,
                               as.table=TRUE,
                               heights=c(6,1))
  ggsave(combinedplot, file=paste(Dir.Plots, "/", Region, "_C", Variable, "_", SoilLayer, ".jpeg", sep = ""), width = 42, height = 22, units = "cm", quality = 100)
} # end of Comres