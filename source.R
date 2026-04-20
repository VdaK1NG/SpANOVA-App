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


# Prepare data
mods <- c("Period.VS.Gender", "Period.VS.Caller", "Caller.VS.Gender")
mods_names <-  SpANOVA_mods$M1_SIM_SpANOVA_v1$Summary
mods_names <- mods_names %>% select(MODEL) %>% pull()

mun_cv_df$covid.vs.caller_mod1.sp.1 <- NA
mun_cv_df$covid.vs.caller_mod1.sp.2 <- NA
mun_cv_df$covid.vs.caller_mod1.sp.3 <- NA
mun_cv_df$covid.vs.caller_mod1.sp.4 <- NA

mun_cv_df$covid.vs.gender_mod1.sp.1 <- NA
mun_cv_df$covid.vs.gender_mod1.sp.2 <- NA
mun_cv_df$covid.vs.gender_mod1.sp.3 <- NA
mun_cv_df$covid.vs.gender_mod1.sp.4 <- NA

mun_cv_df$caller.vs.gender_mod1.sp.1 <- NA
mun_cv_df$caller.vs.gender_mod1.sp.2 <- NA
mun_cv_df$caller.vs.gender_mod1.sp.3 <- NA
mun_cv_df$caller.vs.gender_mod1.sp.4 <- NA


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

convert_col <- function(data){
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

convert_col2 <- function(data){
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
    fig_fill <- convert_col(levels(droplevels(cut(pull(sf_obj, fill_by)*weight, breaks=c(-5, -2, -1, -0.5, -0.1, 0.1, 0.5, 1, 2, 5)))))
    
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
    fig_fill <- convert_col2(levels(droplevels(cut(pull(sf_obj, fill_by), breaks = c(0, 0.5, 0.9, 1.1, 2, Inf), 
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

nodata_plot <- ggplot() + geom_text(aes(x=1, y=1, label="No Data"), size=15, fontface="bold") + ylim(-5,2) +
  geom_text(aes(x=1, y=-1, label="Model Computation"), size=10, fontface="bold") + 
  geom_text(aes(x=1, y=-1.5, label="Failed"), size=10, fontface="bold") + 
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), axis.title.y=element_blank(), 
        axis.text.y=element_blank(), axis.ticks.y=element_blank())


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
