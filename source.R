## Load and recompose data
load("./Data/risk_sim.Rdata")
load("./Data/sp_object.Rdata")
load("./Data/sense_data.Rdata")
load("./Data/data_case.Rdata")
load("./Data/data_sim_p1.Rdata")
load("./Data/data_sim_p2.Rdata")
load("./Data/data_sim_p3.Rdata")
load("./Data/data_sim_p4.Rdata")
load("./Data/data_sim_p5.Rdata")
load("./Data/data_sim_p6.Rdata")
load("./Data/data_sim_p7.Rdata")
load("./Data/SUC_112_mods.Rdata")

SpANOVA_mods <- append(SpANOVA_mods_p1, SpANOVA_mods_p2)
SpANOVA_mods <- append(SpANOVA_mods, SpANOVA_mods_p3)
SpANOVA_mods <- append(SpANOVA_mods, SpANOVA_mods_p4)
SpANOVA_mods <- append(SpANOVA_mods, SpANOVA_mods_p5)
SpANOVA_mods <- append(SpANOVA_mods, SpANOVA_mods_p6)
SpANOVA_mods <- append(SpANOVA_mods, SpANOVA_mods_p7)
rm(SpANOVA_mods_p1, SpANOVA_mods_p2, SpANOVA_mods_p3, SpANOVA_mods_p4, 
   SpANOVA_mods_p5, SpANOVA_mods_p6, SpANOVA_mods_p7)

# Define graph style
PSIC <- "#133BF2"
PSIC_ESC <- c("#FFFFFF", "#FF2F1B", "black")
DIF <- c("#133BF2", "#7189F7", "#FFFFFF", "#FF867A", "#FF2F1B")
CScale_dif <- colorRampPalette(DIF)

tema_VDK <- theme_update(plot.title = element_text(size=14, face= "bold", colour= "grey43", hjust = 0.5), 
                         axis.title.x = element_text(size=14, face="bold", colour = "black"), 
                         axis.title.y = element_text(size=14, face="bold", colour = "black"), 
                         panel.border = element_rect(colour = "black", fill=NA, size=0.3), 
                         legend.title = element_text(size=10, face="bold", colour = "black"), 
                         legend.text = element_text(size=8, colour = "black"),
                         strip.background = element_rect(color="black", fill=PSIC, linewidth=0.5, linetype="solid"),
                         strip.text.x = element_text(color = "white", face = "bold"))

css_default_hover <- girafe_css_bicolor(primary = "#FF2F1B", secondary = "#FF2F1B")
set_girafe_defaults(
  opts_hover = opts_hover(css = css_default_hover),
  opts_zoom = opts_zoom(min = 1, max = 4),
  opts_tooltip = opts_tooltip(css = "padding:3px;background-color:#333333;color:white;"),
  opts_sizing = opts_sizing(rescale = TRUE),
  opts_toolbar = opts_toolbar(saveaspng = FALSE, position = "bottom", delay_mouseout = 5000)
)

nodata_plot <- ggplot() + geom_text(aes(x=1, y=1, label="No Data"), size=15, fontface="bold") + ylim(-5,2) +
  geom_text(aes(x=1, y=-1, label="Model Computation"), size=10, fontface="bold") + 
  geom_text(aes(x=1, y=-1.5, label="Failed"), size=10, fontface="bold") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), axis.title.y=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Prepare data
mods <- c("Period.VS.Gender", "Period.VS.Caller", "Caller.VS.Gender")
mods_names <-  SpANOVA_mods$M1_SIM_SpANOVA_v1$Summary
mods_names <- mods_names %>% select(MODEL) %>% pull()

# Functions
`%notin%` <- Negate(`%in%`)

newLabels <- function(x, dig.lab = 4){
  lev <- levels(x)
  pattern <- paste0("^[\\(\\[][-]*\\d*\\.\\d{", dig.lab, "}|,[-]*\\d*\\.\\d{", dig.lab, "}")
  m <- gregexpr(pattern = pattern, levels(x))
  y <- regmatches(lev, m)
  y <- sapply(y, paste, collapse = "")
  y <- paste0(y, substring(lev, nchar(lev)))
  y
}

convert_sp <- function(data){
  data[data=="(-5,-2]"] <- CScale_dif(9)[1]
  data[data=="(-2,-1]"] <- CScale_dif(9)[2]
  data[data=="(-1,-0.5]"] <- CScale_dif(9)[3]
  data[data=="(-0.5,-0.1]"] <- CScale_dif(9)[4]
  data[data=="(-0.1,0.1]"] <- CScale_dif(9)[5]
  data[data=="(0.1,0.5]"] <- CScale_dif(9)[6]
  data[data=="(0.5,1]"] <- CScale_dif(9)[7]
  data[data=="(1,2]"] <- CScale_dif(9)[8]
  data[data=="(2,5]"] <- CScale_dif(9)[9]
  return(data)
}

convert_rr <- function(data){
  data[data=="(0, 0.5]"] <- CScale_dif(5)[1]
  data[data=="(0.5, 0.9]"] <- CScale_dif(5)[2]
  data[data=="(0.9, 1.1]"] <- CScale_dif(5)[3]
  data[data=="(1.1, 2]"] <- CScale_dif(5)[4]
  data[data=="(2, Inf]"] <- CScale_dif(5)[5]
  return(data)
}

plot_spef <- function(sf_obj=NA, fill_by=NA, breaks=NA, title="Spatial Effect", scale_name="Values", legend.pos="right", weight=1, sp_null=0.125){
  
  # Default breaks in case user does not specify one
  if(sum(is.na(breaks))!=0){breaks=c(-5, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 5)}
  
  ncol_fill <- which(colnames(sf_obj)==fill_by)
  sd_total <- round(sd(pull(sf_obj, fill_by))*weight, 3)
  
  if(sum(is.na(pull(sf_obj, fill_by)))==0){
    fig_fill <- convert_sp(levels(droplevels(cut(pull(sf_obj, fill_by)*weight, breaks=c(-5, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 5)))))
    
    fig <- ggplot() + 
      geom_sf(data=sf_obj, aes(fill=cut(pull(sf_obj, fill_by)*weight,  breaks=c(-5, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 5))), colour=NA) + 
      ggtitle(title, subtitle = ifelse(sd_total < sp_null, paste0("Standard Deviation = ", sd_total, "*"), paste0("Standard Deviation = ", sd_total))) +
      scale_fill_manual(values = fig_fill, name=scale_name) +
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
        plot.subtitle = element_text(size=11, face= "bold", colour= ifelse(sd_total < sp_null, "red", "grey13"), hjust = 0),
        axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"),
        strip.text.x = element_text(size = 8, face = "bold.italic"),
        legend.position = legend.pos)
    
  }else{
    fig <- ggplot() + 
      geom_sf(data=sf_obj, aes(fill=pull(sf_obj, fill_by)), colour="black") + 
      ggtitle("No Effect", subtitle = "Standard Deviation = -") + 
      guides(fill=guide_legend(title="Values         ")) +
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
        plot.subtitle = element_text(size=11, face= "bold", colour= ifelse(sd_total < sp_null, "red", "grey13"), hjust = 0),
        axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"),
        strip.text.x = element_text(size = 8, face = "bold.italic"),
        legend.position = legend.pos)
  }
  
  return(fig)
  
}

plot_rme <- function(sf_obj=NA, fill_by=NA, title=NULL, scale_name="Values", legend.pos="right"){
  
  # Default breaks in case user does not specify one
  
  ncol_fill <- which(colnames(sf_obj)==fill_by)
  
  if(sum(is.na(pull(sf_obj, fill_by)))==0){
    fig_fill <- convert_rr(levels(droplevels(cut(pull(sf_obj, fill_by), breaks = c(0, 0.5, 0.9, 1.1, 2, Inf), 
                                                   labels = c("(0, 0.5]", "(0.5, 0.9]", "(0.9, 1.1]", "(1.1, 2]", "(2, Inf]")))))
    
    fig <- ggplot() + 
      geom_sf(data=sf_obj, aes(fill=cut(pull(sf_obj, fill_by),  breaks=c(0, 0.5, 0.9, 1.1, 2, Inf))), colour=NA) + 
      ggtitle(title) + scale_fill_manual(values = fig_fill, name=scale_name) + 
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
        axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"),
        strip.text.x = element_text(size = 8, face = "bold.italic"),
        legend.position = legend.pos)
  }else{
    fig <- ggplot() + 
      geom_sf(data=sf_obj, aes(fill=pull(sf_obj, fill_by)), colour="black") + 
      ggtitle(title) + 
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
        axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"),
        strip.text.x = element_text(size = 8, face = "bold.italic"),
        legend.position = legend.pos)
  }
  
  return(fig)
  
}

inla.MARB <- function(fit_values, sim_values, n.sim=1){
  
  # Make sure fitted values have the same length as simulated values provided
  if(length(fit_values)!=length(sim_values)){stop("Vector of simulated values has different length than the vector of fitted values")}
  
  # Estimate MARB
  MARB <- mean(abs(Reduce("+",mapply(function(x,y){(x-y)/y}, x=fit_values, y=sim_values, SIMPLIFY=FALSE)))/n.sim)
  
  # Return values
  return(MARB)
}

inla.MRRMSE <- function(fit_values, sim_values, n.sim=1){
  
  # Make sure fitted values have the same length as simulated values provided
  if(length(fit_values)!=length(sim_values)){stop("Vector of simulated values has different length than the vector of fitted values")}
  
  # Estimate MRRMSE
  MRRMSE <- median(sqrt(Reduce("+",mapply(function(x,y){((x-y)/y)^2}, x=fit_values, y=sim_values, SIMPLIFY=FALSE))/n.sim))
  
  # Return values
  return(MRRMSE)
}

convert_col <- function(data, breaks, pal_fun, include.lowest=TRUE, right = TRUE, na.col = NA) {
  
  if (!is.numeric(breaks) || length(breaks) < 2)
    stop("`breaks` must be a numeric vector with length >= 2.")
  
  nbins <- length(breaks) - 1
  cols  <- pal_fun(nbins)
  btxt   <- format(breaks, trim = TRUE, scientific = FALSE)
  left   <- head(btxt, -1)
  rightb <- tail(btxt, -1)
  
  labs <- if (right) {
    paste0("(", left, ",", rightb, "]")
  } else {
    paste0("[", left, ",", rightb, ")")
  }
  labs <- stringr::str_replace_all(labs, "(?<=\\d)\\.0(?=[,\\]])", "")
  
  data_lab <- if (is.numeric(data)) {
    as.character(base::cut(data, breaks = breaks, right = right, include.lowest = include.lowest))
  } else {
    trimws(as.character(data))
  }
  
  out <- cols[match(data_lab, labs)]
  out[is.na(data_lab)] <- na.col
  data <- list()
  data[[1]] <- out
  data[[2]] <- labs
  data[[3]] <- cols
  names(data) <- c("fill_by", "tags", "colors")
  data
}

plot.SpANOVA <- function(
    obj,
    obj_type=c("SpANOVA", "INLA"),
    fill_by=c("Spatial", "Heterogeneity", "RR"),
    n_mod,
    sp_obj,
    breaks=NA,
    fil_scale=c("#133BF2", "#7189F7", "#FFFFFF", "#FF867A", "#FF2F1B"),
    col_frontiers="black",
    scale_name="Values",
    sp_null=0.125,
    legend.position="right",
    ncol_fig=2
){
  
  # Basic checks
  if(!(fill_by %in% c("Spatial", "Heterogeneity", "RR"))){stop("Please specify a proper fill_by argument.")}
  if(!(obj_type %in% c("SpANOVA", "INLA"))){stop("Please specify a proper object type.")}
  if(length(fil_scale)<2){stop("Please specify at least two colors for the colour scale.")}
  
  # Prepare breaks
  if(sum(is.na(breaks)) & fill_by %in% c("Spatial", "Heterogeneity")){
    breaks <- c(-5, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 5)
  }else if(sum(is.na(breaks)) & fill_by=="RR"){
    breaks <- c(0, 0.5, 0.9, 1.1, 2, Inf)
  }
  
  # Prepare colour scale
  CScale <- grDevices::colorRampPalette(fil_scale)
  
  # Retrieve groups
  groups_names <- obj[[n_mod]]$groups
  sp_names <- obj[[n_mod]]$sp_effects
  
  # Prepare data to plot
  if(obj_type=="SpANOVA"){
    if(fill_by=="Heterogeneity"){
      sp_obj$sp1 <- obj[[n_mod]]$summary.random$omega_j$mean[1:nrow(sp_obj)]
      sp_obj$sp2 <- obj[[n_mod]]$summary.random$omega_j$mean[(nrow(sp_obj)+1):(2*nrow(sp_obj))]
      sp_obj$sp3 <- obj[[n_mod]]$summary.random$omega_j$mean[((2*nrow(sp_obj)+1)):(3*nrow(sp_obj))]
      sp_obj$sp4 <- obj[[n_mod]]$summary.random$omega_j$mean[(3*nrow(sp_obj)+1):(4*nrow(sp_obj))]
    }else if(fill_by=="RR"){
      sp_obj$sp1 <- obj[[n_mod]]$summary.fitted.values$mean[1:nrow(sp_obj)]
      sp_obj$sp2 <- obj[[n_mod]]$summary.fitted.values$mean[(nrow(sp_obj)+1):(2*nrow(sp_obj))]
      sp_obj$sp3 <- obj[[n_mod]]$summary.fitted.values$mean[((2*nrow(sp_obj)+1)):(3*nrow(sp_obj))]
      sp_obj$sp4 <- obj[[n_mod]]$summary.fitted.values$mean[(3*nrow(sp_obj)+1):(4*nrow(sp_obj))]
    }else if(fill_by=="Spatial"){
      if(n_mod == 1){
        sp_obj$sp1 <- "No Value"
        sp_obj$sp2 <- "No Value"
        sp_obj$sp3 <- "No Value"
        sp_obj$sp4 <- "No Value"
      }else if(n_mod == 2){
        sp_obj$sp1 <- obj[[n_mod]]$summary.random$phi_1$mean
        sp_obj$sp2 <- obj[[n_mod]]$summary.random$phi_2$mean
        sp_obj$sp3 <- obj[[n_mod]]$summary.random$phi_3$mean
        sp_obj$sp4 <- obj[[n_mod]]$summary.random$phi_4$mean
      }else if(n_mod %in% c(3, 4, 5, 6)){
        sp_obj$sp1 <- obj[[n_mod]]$summary.random$phi_11$mean
        sp_obj$sp2 <- "No Value"
        sp_obj$sp3 <- "No Value"
        sp_obj$sp4 <- "No Value"
      }else if(n_mod %in% c(7, 8)){
        sp_obj$sp1 <- obj[[n_mod]]$summary.random$phi_11$mean
        sp_obj$sp2 <- obj[[n_mod]]$summary.random$phi_21$mean
        sp_obj$sp3 <- "No Value"
        sp_obj$sp4 <- "No Value"
      }else if(n_mod %in% c(9, 10)){
        sp_obj$sp1 <- obj[[n_mod]]$summary.random$phi_11$mean
        sp_obj$sp2 <- obj[[n_mod]]$summary.random$phi_12$mean
        sp_obj$sp3 <- "No Value"
        sp_obj$sp4 <- "No Value"
      }else if(n_mod %in% c(11, 12, 13, 14)){
        sp_obj$sp1 <- obj[[n_mod]]$summary.random$phi_11$mean
        sp_obj$sp2 <- obj[[n_mod]]$summary.random$phi_12$mean
        sp_obj$sp3 <- obj[[n_mod]]$summary.random$phi_21$mean
        sp_obj$sp4 <- "No Value"
      }else if(n_mod %in% c(15, 16, 17, 18, 19, 20, 21, 22)){
        sp_obj$sp1 <- obj[[n_mod]]$summary.random$phi_11$mean
        sp_obj$sp2 <- obj[[n_mod]]$summary.random$phi_12$mean
        sp_obj$sp3 <- obj[[n_mod]]$summary.random$phi_21$mean
        sp_obj$sp4 <- obj[[n_mod]]$summary.random$phi_22$mean
      }
    }
  }else if(obj_type=="INLA"){
    if(fill_by=="Heterogeneity"){
      sp_obj$sp1 <- obj$summary.random$omega_j$mean[1:nrow(sp_obj)]
      sp_obj$sp2 <- obj$summary.random$omega_j$mean[(nrow(sp_obj)+1):(2*nrow(sp_obj))]
      sp_obj$sp3 <- obj$summary.random$omega_j$mean[((2*nrow(sp_obj)+1)):(3*nrow(sp_obj))]
      sp_obj$sp4 <- obj$summary.random$omega_j$mean[(3*nrow(sp_obj)+1):(4*nrow(sp_obj))]
    }else if(fill_by=="RR"){
      sp_obj$sp1 <- obj$summary.fitted.values$mean[1:nrow(sp_obj)]
      sp_obj$sp2 <- obj$summary.fitted.values$mean[(nrow(sp_obj)+1):(2*nrow(sp_obj))]
      sp_obj$sp3 <- obj$summary.fitted.values$mean[((2*nrow(sp_obj)+1)):(3*nrow(sp_obj))]
      sp_obj$sp4 <- obj$summary.fitted.values$mean[(3*nrow(sp_obj)+1):(4*nrow(sp_obj))]
    }else if(fill_by=="Spatial"){
      if(n_mod == 1){
        sp_obj$sp1 <- "No Value"
        sp_obj$sp2 <- "No Value"
        sp_obj$sp3 <- "No Value"
        sp_obj$sp4 <- "No Value"
      }else if(n_mod == 2){
        sp_obj$sp1 <- obj$summary.random$phi_1$mean
        sp_obj$sp2 <- obj$summary.random$phi_2$mean
        sp_obj$sp3 <- obj$summary.random$phi_3$mean
        sp_obj$sp4 <- obj$summary.random$phi_4$mean
      }else if(n_mod %in% c(3, 4, 5, 6)){
        sp_obj$sp1 <- obj$summary.random$phi_11$mean
        sp_obj$sp2 <- "No Value"
        sp_obj$sp3 <- "No Value"
        sp_obj$sp4 <- "No Value"
      }else if(n_mod %in% c(7, 8)){
        sp_obj$sp1 <- obj$summary.random$phi_11$mean
        sp_obj$sp2 <- obj$summary.random$phi_21$mean
        sp_obj$sp3 <- "No Value"
        sp_obj$sp4 <- "No Value"
      }else if(n_mod %in% c(9, 10)){
        sp_obj$sp1 <- obj$summary.random$phi_11$mean
        sp_obj$sp2 <- obj$summary.random$phi_12$mean
        sp_obj$sp3 <- "No Value"
        sp_obj$sp4 <- "No Value"
      }else if(n_mod %in% c(11, 12, 13, 14)){
        sp_obj$sp1 <- obj$summary.random$phi_11$mean
        sp_obj$sp2 <- obj$summary.random$phi_12$mean
        sp_obj$sp3 <- obj$summary.random$phi_21$mean
        sp_obj$sp4 <- "No Value"
      }else if(n_mod %in% c(15, 16, 17, 18, 19, 20, 21, 22)){
        sp_obj$sp1 <- obj$summary.random$phi_11$mean
        sp_obj$sp2 <- obj$summary.random$phi_12$mean
        sp_obj$sp3 <- obj$summary.random$phi_21$mean
        sp_obj$sp4 <- obj$summary.random$phi_22$mean
      }
    }
  }
  
  # Estimate standard deviation for each effect
  sd_sp1 <- round(sd(sp_obj$sp1), 3)
  sd_sp2 <- round(sd(sp_obj$sp2), 3)
  sd_sp3 <- round(sd(sp_obj$sp3), 3)
  sd_sp4 <- round(sd(sp_obj$sp4), 3)
  
  # Create titles
  if(fill_by=="Spatial"){
    if(n_mod == 1){
      title1 <- "No Effect"
      title2 <- "No Effect"
      title3 <- "No Effect"
      title4 <- "No Effect"
    }else if(n_mod == 2){
      title1 <- bquote(bold("Spatial Effect | "~phi[1]))
      title2 <- bquote(bold("Spatial Effect | "~phi[2]))
      title3 <- bquote(bold("Spatial Effect | "~phi[3]))
      title4 <- bquote(bold("Spatial Effect | "~phi[4]))
    }else if(n_mod %in% c(3, 4, 5, 6)){
      title1 <- bquote(bold("Spatial Effect | "~phi[11]))
      title2 <- "No Effect"
      title3 <- "No Effect"
      title4 <- "No Effect"
    }else if(n_mod %in% c(7, 8)){
      title1 <- bquote(bold("Spatial Effect | "~phi[11]))
      title2 <- bquote(bold("Spatial Effect | "~phi[12]))
      title3 <- "No Effect"
      title4 <- "No Effect"
    }else if(n_mod %in% c(9, 10)){
      title1 <- bquote(bold("Spatial Effect | "~phi[11]))
      title2 <- bquote(bold("Spatial Effect | "~phi[21]))
      title3 <- "No Effect"
      title4 <- "No Effect"
    }else if(n_mod %in% c(11, 12, 13, 14)){
      title1 <- bquote(bold("Spatial Effect | "~phi[11]))
      title2 <- bquote(bold("Spatial Effect | "~phi[12]))
      title3 <- bquote(bold("Spatial Effect | "~phi[21]))
      title4 <- "No Effect"
    }else if(n_mod %in% c(15, 16, 17, 18, 19, 20, 21, 22)){
      title1 <- bquote(bold("Spatial Effect | "~phi[11]))
      title2 <- bquote(bold("Spatial Effect | "~phi[12]))
      title3 <- bquote(bold("Spatial Effect | "~phi[21]))
      title4 <- bquote(bold("Spatial Effect | "~phi[22]))
    }
  }else if(fill_by=="RR"){
    title1 <- paste0("Relative Risk Adjusted | G1")
    title2 <- paste0("Relative Risk Adjusted | G2")
    title3 <- paste0("Relative Risk Adjusted | G3")
    title4 <- paste0("Relative Risk Adjusted | G4")
  }else if(fill_by=="Heterogeneity"){
    title1 <- bquote(bold("Heterogeneity Effect | "~omega[1]))
    title2 <- bquote(bold("Heterogeneity Effect | "~omega[2]))
    title3 <- bquote(bold("Heterogeneity Effect | "~omega[3]))
    title4 <- bquote(bold("Heterogeneity Effect | "~omega[4]))
  }
  
  # Prepare Figures
  if(fill_by %in% c("Spatial", "Heterogeneity")){
    # Create fig1
    if(sum(sp_obj$sp1=="No Value")==0){
      fig_fig1 <- convert_col(
        data = sp_obj$sp1,
        breaks = breaks,
        pal_fun = CScale,
        right = TRUE,
        include.lowest = FALSE,
        na.col = NA
      )
      
      fig_values1 <- fig_fig1$colors[which(fig_fig1$tags %in% cut(sp_obj$sp1,  breaks=breaks))]
      
      fig1 <- ggplot2::ggplot(data=sp_obj) +
        geom_sf(aes(fill=cut(sp1,  breaks=breaks)), colour=col_frontiers) +
        geom_sf(aes(), fill=fig_fig1$fill_by, colour=col_frontiers) +
        ggtitle(title1, subtitle = ifelse(sd_sp1 < sp_null, paste0("Standard Deviation = ", sd_sp1, "*"), paste0("Standard Deviation = ", sd_sp1))) +
        scale_fill_manual(values = fig_values1, name=scale_name) +
        theme(
          plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
          plot.subtitle = element_text(size=11, face= "bold", colour= ifelse(sd_sp1 < sp_null, "red", "grey13"), hjust = 0),
          axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm"),
          strip.text.x = element_text(size = 8, face = "bold.italic"),
          legend.position = legend.position
        )
    }else{
      fig1 <- ggplot2::ggplot(data=sp_obj) +
        geom_sf(aes(fill=sp1), colour=col_frontiers) +
        scale_fill_manual(values="grey24", name=scale_name) +
        ggtitle(title1, subtitle = paste0("No Effect")) +
        theme(
          plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
          plot.subtitle = element_text(size=11, face= "bold", colour= "red", hjust = 0),
          axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm"),
          strip.text.x = element_text(size = 8, face = "bold.italic"),
          legend.position = legend.position
        )
    }
    
    
    # Create fig2
    if(sum(sp_obj$sp2=="No Value")==0){
      fig_fig2 <- convert_col(
        data = sp_obj$sp2,
        breaks = breaks,
        pal_fun = CScale,
        right = TRUE,
        include.lowest = FALSE,
        na.col = NA
      )
      
      fig_values2 <- fig_fig2$colors[which(fig_fig2$tags %in% cut(sp_obj$sp2,  breaks=breaks))]
      
      fig2 <- ggplot2::ggplot(data=sp_obj) +
        geom_sf(aes(fill=cut(sp2,  breaks=breaks)), colour=col_frontiers) +
        geom_sf(aes(), fill=fig_fig2$fill_by, colour=col_frontiers) +
        ggtitle(title2, subtitle = ifelse(sd_sp2 < sp_null, paste0("Standard Deviation = ", sd_sp2, "*"), paste0("Standard Deviation = ", sd_sp2))) +
        scale_fill_manual(values = fig_values2, name=scale_name) +
        theme(
          plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
          plot.subtitle = element_text(size=11, face= "bold", colour= ifelse(sd_sp2 < sp_null, "red", "grey13"), hjust = 0),
          axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm"),
          strip.text.x = element_text(size = 8, face = "bold.italic"),
          legend.position = legend.position
        )
    }else{
      fig2 <- ggplot2::ggplot(data=sp_obj) +
        geom_sf(aes(fill=sp2), colour=col_frontiers) +
        scale_fill_manual(values="grey24", name=scale_name) +
        ggtitle(title2, subtitle = paste0("No Effect")) +
        theme(
          plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
          plot.subtitle = element_text(size=11, face= "bold", colour= "red", hjust = 0),
          axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm"),
          strip.text.x = element_text(size = 8, face = "bold.italic"),
          legend.position = legend.position
        )
    }
    
    # Create fig3
    if(sum(sp_obj$sp3=="No Value")==0){
      fig_fig3 <- convert_col(
        data = sp_obj$sp3,
        breaks = breaks,
        pal_fun = CScale,
        right = TRUE,
        include.lowest = FALSE,
        na.col = NA
      )
      
      fig_values3 <- fig_fig3$colors[which(fig_fig3$tags %in% cut(sp_obj$sp3,  breaks=breaks))]
      
      fig3 <- ggplot2::ggplot(data=sp_obj) +
        geom_sf(aes(fill=cut(sp3,  breaks=breaks)), colour=col_frontiers) +
        geom_sf(aes(), fill=fig_fig3$fill_by, colour=col_frontiers) +
        ggtitle(title3, subtitle = ifelse(sd_sp3 < sp_null, paste0("Standard Deviation = ", sd_sp3, "*"), paste0("Standard Deviation = ", sd_sp3))) +
        scale_fill_manual(values = fig_values3, name=scale_name) +
        theme(
          plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
          plot.subtitle = element_text(size=11, face= "bold", colour= ifelse(sd_sp3 < sp_null, "red", "grey13"), hjust = 0),
          axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm"),
          strip.text.x = element_text(size = 8, face = "bold.italic"),
          legend.position = legend.position
        )
    }else{
      fig3 <- ggplot2::ggplot(data=sp_obj) +
        geom_sf(aes(fill=sp3), colour=col_frontiers) +
        scale_fill_manual(values="grey24", name=scale_name) +
        ggtitle(title3, subtitle = paste0("No Effect")) +
        theme(
          plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
          plot.subtitle = element_text(size=11, face= "bold", colour= "red", hjust = 0),
          axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm"),
          strip.text.x = element_text(size = 8, face = "bold.italic"),
          legend.position = legend.position
        )
    }
    
    # Create fig4
    if(sum(sp_obj$sp4=="No Value")==0){
      fig_fig4 <- convert_col(
        data = sp_obj$sp4,
        breaks = breaks,
        pal_fun = CScale,
        right = TRUE,
        include.lowest = FALSE,
        na.col = NA
      )
      
      fig_values4 <- fig_fig4$colors[which(fig_fig4$tags %in% cut(sp_obj$sp4,  breaks=breaks))]
      
      fig4 <- ggplot2::ggplot(data=sp_obj) +
        geom_sf(aes(fill=cut(sp4,  breaks=breaks)), colour=col_frontiers) +
        geom_sf(aes(), fill=fig_fig4$fill_by, colour=col_frontiers) +
        ggtitle(title4, subtitle = ifelse(sd_sp4 < sp_null, paste0("Standard Deviation = ", sd_sp4, "*"), paste0("Standard Deviation = ", sd_sp4))) +
        scale_fill_manual(values = fig_values4, name=scale_name) +
        theme(
          plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
          plot.subtitle = element_text(size=11, face= "bold", colour= ifelse(sd_sp4 < sp_null, "red", "grey13"), hjust = 0),
          axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm"),
          strip.text.x = element_text(size = 8, face = "bold.italic"),
          legend.position = legend.position
        )
    }else{
      fig4 <- ggplot2::ggplot(data=sp_obj) +
        geom_sf(aes(fill=sp4), colour=col_frontiers) +
        scale_fill_manual(values="grey24", name=scale_name) +
        ggtitle(title4, subtitle = paste0("No Effect")) +
        theme(
          plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
          plot.subtitle = element_text(size=11, face= "bold", colour= "red", hjust = 0),
          axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
          axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          legend.text = element_text(size = 12),
          legend.key.size = unit(0.75, "cm"),
          strip.text.x = element_text(size = 8, face = "bold.italic"),
          legend.position = legend.position
        )
    }
  }else if(fill_by == "RR"){
    # Create fig1
    fig_fig1 <- convert_col(
      data = sp_obj$sp1,
      breaks = breaks,
      pal_fun = CScale,
      right = TRUE,
      include.lowest = FALSE,
      na.col = NA
    )
    
    fig_values1 <- fig_fig1$colors[which(fig_fig1$tags %in% cut(sp_obj$sp1,  breaks=breaks))]
    
    fig1 <- ggplot2::ggplot(data=sp_obj) +
      geom_sf(aes(fill=cut(sp1,  breaks=breaks)), colour=col_frontiers) +
      geom_sf(aes(), fill=fig_fig1$fill_by, colour=col_frontiers) +
      ggtitle(title1, subtitle = groups_names[1]) +
      scale_fill_manual(values = fig_values1, name=scale_name) +
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
        plot.subtitle = element_text(size=11, face= "bold", colour= "grey13", hjust = 0),
        axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"),
        strip.text.x = element_text(size = 8, face = "bold.italic"),
        legend.position = legend.position
      )
    
    # Create fig2
    fig_fig2 <- convert_col(
      data = sp_obj$sp2,
      breaks = breaks,
      pal_fun = CScale,
      right = TRUE,
      include.lowest = FALSE,
      na.col = NA
    )
    
    fig_values2 <- fig_fig2$colors[which(fig_fig2$tags %in% cut(sp_obj$sp2,  breaks=breaks))]
    
    fig2 <- ggplot2::ggplot(data=sp_obj) +
      geom_sf(aes(fill=cut(sp2,  breaks=breaks)), colour=col_frontiers) +
      geom_sf(aes(), fill=fig_fig2$fill_by, colour=col_frontiers) +
      ggtitle(title2, subtitle = groups_names[2]) +
      scale_fill_manual(values = fig_values2, name=scale_name) +
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
        plot.subtitle = element_text(size=11, face= "bold", colour= "grey13", hjust = 0),
        axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"),
        strip.text.x = element_text(size = 8, face = "bold.italic"),
        legend.position = legend.position
      )
    
    # Create fig3
    fig_fig3 <- convert_col(
      data = sp_obj$sp3,
      breaks = breaks,
      pal_fun = CScale,
      right = TRUE,
      include.lowest = FALSE,
      na.col = NA
    )
    
    fig_values3 <- fig_fig3$colors[which(fig_fig3$tags %in% cut(sp_obj$sp3,  breaks=breaks))]
    
    fig3 <- ggplot2::ggplot(data=sp_obj) +
      geom_sf(aes(fill=cut(sp3,  breaks=breaks)), colour=col_frontiers) +
      geom_sf(aes(), fill=fig_fig3$fill_by, colour=col_frontiers) +
      ggtitle(title3, subtitle = groups_names[3]) +
      scale_fill_manual(values = fig_values3, name=scale_name) +
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
        plot.subtitle = element_text(size=11, face= "bold", colour= "grey13", hjust = 0),
        axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"),
        strip.text.x = element_text(size = 8, face = "bold.italic"),
        legend.position = legend.position
      )
    
    # Create fig4
    fig_fig4 <- convert_col(
      data = sp_obj$sp4,
      breaks = breaks,
      pal_fun = CScale,
      right = TRUE,
      include.lowest = FALSE,
      na.col = NA
    )
    
    fig_values4 <- fig_fig4$colors[which(fig_fig4$tags %in% cut(sp_obj$sp4,  breaks=breaks))]
    
    fig4 <- ggplot2::ggplot(data=sp_obj) +
      geom_sf(aes(fill=cut(sp4,  breaks=breaks)), colour=col_frontiers) +
      geom_sf(aes(), fill=fig_fig4$fill_by, colour=col_frontiers) +
      ggtitle(title4, subtitle = groups_names[4]) +
      scale_fill_manual(values = fig_values4, name=scale_name) +
      theme(
        plot.title = element_text(size=14, face= "bold", colour= "black", hjust = 0),
        plot.subtitle = element_text(size=11, face= "bold", colour= "grey13", hjust = 0),
        axis.text.x=element_blank(),  axis.ticks.x=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank(),
        legend.text = element_text(size = 12),
        legend.key.size = unit(0.75, "cm"),
        strip.text.x = element_text(size = 8, face = "bold.italic"),
        legend.position = legend.position
      )
  }
  
  # Create final figure
  gridExtra::grid.arrange(fig1, fig2, fig3, fig4, ncol=ncol_fig)
}
