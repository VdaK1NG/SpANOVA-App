ss_userAction.Log <- periscope:::fw_get_user_log()

shinyServer(function(input, output, session) {
  
  observe({
    updateSelectInput(
      session, "risk_version", "Select a Risk Version",
      choices = switch(input$mod_type,
                       "M0" = c("-"),
                       "M1" = c("Risk Version 1", "Risk Version 2", "Risk Version 3"),
                       "M2" = c("v1", "v2", "v3"),
                       "M3" = c("v1", "v2", "v3"),
                       "M4" = c("v1", "v2", "v3"),
                       "M5" = c("v1", "v2", "v3"),
                       "M6" = c("v1", "v2", "v3")
                       )
    )
  })
  
  observeEvent(
    eventExpr = {
      input$risk_version
      input$mod_type
      input$sp_ef_mod
    }, 
    handlerExpr = {
      # You code to run
    })
  
  observe({
    updateSelectInput(
      session, "risk_version_sens", "Select a Risk Version",
      choices = switch(input$mod_type_sens,
                       "M0" = c("-"),
                       "M1" = c("v1", "v2", "v3"),
                       "M2" = c("v1", "v2", "v3"),
                       "M3" = c("v1", "v2", "v3"),
                       "M4" = c("v1", "v2", "v3"),
                       "M5" = c("v1", "v2", "v3"),
                       "M6" = c("v1", "v2", "v3")
      )
    )
  })
  
  observeEvent(
    eventExpr = {
      input$risk_version_sens
      input$mod_type_sens
      input$base_type_sens
    }, 
    handlerExpr = {
      # You code to run
    })
  
  ##################################### Tabla Summary ###############################################################################
  sim.data.ShANOVA <- reactive({
    
    if(input$mod_type=="M0"){
      n_mod <- which(names(SpANOVA_mods)==paste0(input$mod_type, "_", input$base_type, "_SpANOVA"))
      data <- SpANOVA_mods[[n_mod]]
      data
    }else{
      n_mod <- which(names(SpANOVA_mods)==paste0(input$mod_type, "_", input$base_type, "_SpANOVA", "_", input$risk_version))
      data <- SpANOVA_mods[[n_mod]]
      data
    }
  })
  
  data.table.sim <- reactive({
    
    data_table <- sim.data.ShANOVA()$Summary
    
    # Get MARB and MRRMSE
    data_MARB <- c()
    data_MRRMSE <- c()
    
    if(input$mod_type=="M0"){
      exp_temp <- data_risk[[which(paste0(input$mod_type, "_", input$base_type)==colnames(data_risk))]]
    }else{
      exp_temp <- data_risk[[which(paste0(input$mod_type, "_", input$base_type, "_", toupper(input$risk_version))==colnames(data_risk))]]
      }
    
    for (i in 1:22) {
      adj_temp <- sim.data.ShANOVA()[[i]]$summary.fitted.values$mean
      if(!is.null(adj_temp)){
        data_MARB <- c(data_MARB, inla.MARB(fit_values=adj_temp, sim_values = exp_temp, n.sim=1))
        data_MRRMSE <- c(data_MRRMSE, inla.MRRMSE(fit_values=adj_temp, sim_values = exp_temp, n.sim=1))
      } else {
        data_MARB <- c(data_MARB, NA)
        data_MRRMSE <- c(data_MRRMSE, NA)
      }
    }
    
    data_table$ARB <- data_MARB
    data_table$RMSE <- data_MRRMSE
    
    data_table <- data_table %>% 
      select(MODEL, DIC, WAIC, CPU, LOOCV, LOGCV.3, LOGCV.5, LOGCV.10, sp.null, ARB, RMSE) %>% 
      rename("SP 0.125"=sp.null) %>% 
      mutate(DIC=round(as.numeric(DIC), 1), WAIC=round(as.numeric(WAIC), 1), CPU=round(as.numeric(CPU), 1), 
             ARB=round(as.numeric(ARB), 1), RMSE=round(as.numeric(RMSE), 1)) 
    
    data_table
    
  })
  
  output$table_simres <- renderDataTable({datatable(data.table.sim(), 
                                                    options = list(pageLength = 25, initComplete = JS(
                                                      "function(settings, json) {",
                                                      "$(this.api().table().header()).css({'background-color': '#133BF2', 'color': 'white'});",
                                                      "}")),
                                                    escape = FALSE,rownames = FALSE) %>%
      formatStyle(c("MODEL", "DIC", "WAIC", "CPU", "LOOCV", "LOGCV.3", "LOGCV.5", "LOGCV.10", "SP 0.125", "ARB", "RMSE"), fontWeight = "bold") %>% 
      formatStyle('MODEL', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('DIC', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('WAIC', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('CPU', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('SP 0.125', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red'))) %>% 
      formatStyle('ARB', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red'))) %>% 
      formatStyle('RMSE', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red'))) %>% 
      formatStyle('LOOCV', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red'))) %>% 
      formatStyle('LOGCV.3', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red'))) %>% 
      formatStyle('LOGCV.5', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red'))) %>% 
      formatStyle('LOGCV.10', "SP 0.125", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red'))) 
    })
  
  data.plot.sp <- reactive({
    
    data_temp <- sp_object
    
    if(input$sp_ef_mod=="M0"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$M0$summary.random$omega_j$mean[1:3107]
      data_temp$sp2 <- sim.data.ShANOVA()$M0$summary.random$omega_j$mean[(3107+1):(3107*2)]
      data_temp$sp3 <- sim.data.ShANOVA()$M0$summary.random$omega_j$mean[(3107*2+1):(3107*3)]
      data_temp$sp4 <- sim.data.ShANOVA()$M0$summary.random$omega_j$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M1"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$M1$summary.random$phi_1$mean
      data_temp$sp2 <- sim.data.ShANOVA()$M1$summary.random$phi_2$mean
      data_temp$sp3 <- sim.data.ShANOVA()$M1$summary.random$phi_3$mean
      data_temp$sp4 <- sim.data.ShANOVA()$M1$summary.random$phi_4$mean
      
    }else if(input$sp_ef_mod=="M2-ind(F1L1-F2L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M2.(F1L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- NA
      data_temp$sp3 <- NA
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M2-ind(F1L2-F2L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M2.(F1L2)`$summary.random$phi_11$mean
      data_temp$sp2 <- NA
      data_temp$sp3 <- NA
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M2-ind(F2L1-F1L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M2.(F2L1)`$summary.random$phi_11$mean 
      data_temp$sp2 <- NA
      data_temp$sp3 <- NA
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M2-ind(F2L2-F1L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M2.(F2L2)`$summary.random$phi_11$mean 
      data_temp$sp2 <- NA
      data_temp$sp3 <- NA
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M3-F1.(F1L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.random$phi_21$mean
      data_temp$sp3 <- NA
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M3-F1.(F1L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.random$phi_21$mean
      data_temp$sp3 <- NA
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M4-F2.(F2L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M4.F2(F2L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M4.F2(F2L1)`$summary.random$phi_12$mean
      data_temp$sp3 <- NA
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M4-F2.(F2L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M4.F2(F2L2)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M4.F2(F2L2)`$summary.random$phi_12$mean
      data_temp$sp3 <- NA
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M5-F1.(F1L1)+F2.(F2L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L1)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L1)`$summary.random$phi_21$mean
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M5-F1.(F1L2)+F2.(F2L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L1)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L1)`$summary.random$phi_21$mean
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M5-F1.(F1L1)+F2.(F2L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L2)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L2)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L2)`$summary.random$phi_21$mean
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M5-F1.(F1L2)+F2.(F2L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L2)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L2)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L2)`$summary.random$phi_21$mean
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M6-F1.(F1L1)*F2.(F2L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L1)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L1)`$summary.random$phi_21$mean
      data_temp$sp4 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L1)`$summary.random$phi_22$mean
      
    }else if(input$sp_ef_mod=="M6-F1.(F1L2)*F2.(F2L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L1)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L1)`$summary.random$phi_21$mean
      data_temp$sp4 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L1)`$summary.random$phi_22$mean
      
    }else if(input$sp_ef_mod=="M6-F1.(F1L1)*F2.(F2L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L2)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L2)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L2)`$summary.random$phi_21$mean
      data_temp$sp4 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L2)`$summary.random$phi_22$mean
      
    }else if(input$sp_ef_mod=="M6-F1.(F1L2)*F2.(F2L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L2)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L2)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L2)`$summary.random$phi_21$mean
      data_temp$sp4 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L2)`$summary.random$phi_22$mean
      
    }else if(input$sp_ef_mod=="M6-F2.(F2L1)*F1.(F1L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L1)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L1)`$summary.random$phi_21$mean
      data_temp$sp4 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L1)`$summary.random$phi_22$mean
      
    }else if(input$sp_ef_mod=="M6-F2.(F2L2)*F1.(F1L1)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L1)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L1)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L1)`$summary.random$phi_21$mean
      data_temp$sp4 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L1)`$summary.random$phi_22$mean
      
    }else if(input$sp_ef_mod=="M6-F2.(F2L1)*F1.(F1L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L2)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L2)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L2)`$summary.random$phi_21$mean
      data_temp$sp4 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L2)`$summary.random$phi_22$mean
      
    }else if(input$sp_ef_mod=="M6-F2.(F2L2)*F1.(F1L2)"){
      
      data_temp$sp1 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L2)`$summary.random$phi_11$mean
      data_temp$sp2 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L2)`$summary.random$phi_12$mean
      data_temp$sp3 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L2)`$summary.random$phi_21$mean
      data_temp$sp4 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L2)`$summary.random$phi_22$mean
      
    }
    
    data_temp
  })
  
  data.plot.RME <- reactive({
    
    data_temp <- sp_object
    
    if(input$sp_ef_mod=="M0"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$M0$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$M0$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$M0$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$M0$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M1"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$M1$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$M1$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$M1$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$M1$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M2-ind(F1L1-F2L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M2.(F1L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M2.(F1L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M2.(F1L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M2.(F1L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M2-ind(F1L2-F2L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M2.(F1L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M2.(F1L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M2.(F1L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M2.(F1L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M2-ind(F2L1-F1L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M2.(F2L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M2.(F2L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M2.(F2L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M2.(F2L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M2-ind(F2L2-F1L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M2.(F2L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M2.(F2L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M2.(F2L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M2.(F2L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M3-F1.(F1L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M3-F1.(F1L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M3.F1(F1L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M4-F2.(F2L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M4.F2(F2L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M4.F2(F2L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M4.F2(F2L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M4.F2(F2L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M4-F2.(F2L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M4.F2(F2L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M4.F2(F2L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M4.F2(F2L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M4.F2(F2L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M5-F1.(F1L1)+F2.(F2L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M5-F1.(F1L2)+F2.(F2L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M5-F1.(F1L1)+F2.(F2L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M5.F1(F1L1)+F2(F2L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M5-F1.(F1L2)+F2.(F2L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M5.F1(F1L2)+F2(F2L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      data_temp$sp4 <- NA
      
    }else if(input$sp_ef_mod=="M6-F1.(F1L1)*F2.(F2L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M6-F1.(F1L2)*F2.(F2L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M6-F1.(F1L1)*F2.(F2L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M6.F1(F1L1)*F2(F2L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M6-F1.(F1L2)*F2.(F2L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M6.F1(F1L2)*F2(F2L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M6-F2.(F2L1)*F1.(F1L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M6-F2.(F2L2)*F1.(F1L1)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L1)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L1)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L1)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L1)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M6-F2.(F2L1)*F1.(F1L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M6.F2(F2L1)*F1(F1L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    }else if(input$sp_ef_mod=="M6-F2.(F2L2)*F1.(F1L2)"){
      
      data_temp$rme1 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L2)`$summary.fitted.values$mean[1:3107]
      data_temp$rme2 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L2)`$summary.fitted.values$mean[(3107+1):(3107*2)]
      data_temp$rme3 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L2)`$summary.fitted.values$mean[(3107*2+1):(3107*3)]
      data_temp$rme4 <- sim.data.ShANOVA()$`M6.F2(F2L2)*F1(F1L2)`$summary.fitted.values$mean[(3107*3+1):(3107*4)]
      
    } 
    
    data_temp
  })
  
  data.plot.sp.sim <- reactive({
    
    data_temp2 <- sp_object
    
    if(input$mod_type=="M0" & input$base_type=="DIF"){
      
      data_temp2$sim_sp1 <- data_temp2$group1_dif_risk
      data_temp2$sim_sp2 <- data_temp2$group2_dif_risk
      data_temp2$sim_sp3 <- data_temp2$group3_dif_risk
      data_temp2$sim_sp4 <- data_temp2$group4_dif_risk
      
    }else if(input$mod_type=="M0" & input$base_type=="SIM"){
      
      data_temp2$sim_sp1 <- data_temp2$group1_sim_risk
      data_temp2$sim_sp2 <- data_temp2$group2_sim_risk
      data_temp2$sim_sp3 <- data_temp2$group3_sim_risk
      data_temp2$sim_sp4 <- data_temp2$group4_sim_risk
      
    }else if(input$mod_type=="M1"){
      
      data_temp2$sim_sp1 <- data_temp2$ind.ef.g2
      data_temp2$sim_sp2 <- data_temp2$ind.ef.g1
      data_temp2$sim_sp3 <- data_temp2$ind.ef.g3
      data_temp2$sim_sp4 <- data_temp2$ind.ef.g4
      
    }else if(input$mod_type=="M2"){
      
      data_temp2$sim_sp1 <- data_temp2$ind.ef.g2
      data_temp2$sim_sp2 <- NA
      data_temp2$sim_sp3 <- NA
      data_temp2$sim_sp4 <- NA
      
    }else if(input$mod_type=="M3"){
      
      data_temp2$sim_sp1 <- data_temp2$ind.ef.g2
      data_temp2$sim_sp2 <- data_temp2$POP_DENS_Scale
      data_temp2$sim_sp3 <- NA
      data_temp2$sim_sp4 <- NA
      
    }else if(input$mod_type=="M4"){
      
      data_temp2$sim_sp1 <- data_temp2$ind.ef.g2
      data_temp2$sim_sp2 <- data_temp2$Temp_Scale
      data_temp2$sim_sp3 <- NA
      data_temp2$sim_sp4 <- NA
      
    }else if(input$mod_type=="M5"){
      
      data_temp2$sim_sp1 <- data_temp2$ind.ef.g2
      data_temp2$sim_sp2 <- data_temp2$POP_DENS_Scale
      data_temp2$sim_sp3 <- data_temp2$Temp_Scale
      data_temp2$sim_sp4 <- NA
      
    }else if(input$mod_type=="M6"){
      
      data_temp2$sim_sp1 <- data_temp2$ind.ef.g2
      data_temp2$sim_sp2 <- data_temp2$Temp_Scale
      data_temp2$sim_sp3 <- data_temp2$POP_DENS_Scale
      data_temp2$sim_sp4 <- data_temp2$ind.ef.g1
      
    }  
    
    data_temp2
  })
  
  data.plot.RME.sim <- reactive({
    
    data_temp2 <- sp_object
    
    if(input$mod_type=="M0" & input$base_type=="DIF"){
      
      data_temp2$sim_rme1 <- data_risk$M0_DIF[1:3107]
      data_temp2$sim_rme2 <- data_risk$M0_DIF[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M0_DIF[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M0_DIF[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M0" & input$base_type=="SIM"){
      
      data_temp2$sim_rme1 <- data_risk$M0_SIM[1:3107]
      data_temp2$sim_rme2 <- data_risk$M0_SIM[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M0_SIM[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M0_SIM[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M1" & input$base_type=="SIM" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M1_SIM_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M1_SIM_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M1_SIM_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M1_SIM_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M1" & input$base_type=="SIM" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M1_SIM_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M1_SIM_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M1_SIM_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M1_SIM_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M1" & input$base_type=="SIM" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M1_SIM_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M1_SIM_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M1_SIM_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M1_SIM_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M1" & input$base_type=="DIF" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M1_DIF_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M1_DIF_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M1_DIF_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M1_DIF_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M1" & input$base_type=="DIF" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M1_DIF_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M1_DIF_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M1_DIF_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M1_DIF_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M1" & input$base_type=="DIF" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M1_DIF_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M1_DIF_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M1_DIF_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M1_DIF_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M2" & input$base_type=="SIM" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M2_SIM_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M2_SIM_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M2_SIM_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M2_SIM_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M2" & input$base_type=="SIM" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M2_SIM_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M2_SIM_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M2_SIM_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M2_SIM_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M2" & input$base_type=="SIM" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M2_SIM_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M2_SIM_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M2_SIM_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M2_SIM_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M2" & input$base_type=="DIF" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M2_DIF_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M2_DIF_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M2_DIF_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M2_DIF_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M2" & input$base_type=="DIF" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M2_DIF_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M2_DIF_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M2_DIF_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M2_DIF_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M2" & input$base_type=="DIF" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M2_DIF_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M2_DIF_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M2_DIF_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M2_DIF_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M3" & input$base_type=="SIM" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M3_SIM_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M3_SIM_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M3_SIM_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M3_SIM_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M3" & input$base_type=="SIM" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M3_SIM_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M3_SIM_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M3_SIM_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M3_SIM_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M3" & input$base_type=="SIM" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M3_SIM_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M3_SIM_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M3_SIM_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M3_SIM_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M3" & input$base_type=="DIF" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M3_DIF_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M3_DIF_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M3_DIF_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M3_DIF_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M3" & input$base_type=="DIF" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M3_DIF_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M3_DIF_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M3_DIF_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M3_DIF_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M3" & input$base_type=="DIF" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M3_DIF_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M3_DIF_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M3_DIF_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M3_DIF_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M4" & input$base_type=="SIM" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M4_SIM_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M4_SIM_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M4_SIM_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M4_SIM_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M4" & input$base_type=="SIM" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M4_SIM_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M4_SIM_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M4_SIM_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M4_SIM_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M4" & input$base_type=="SIM" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M4_SIM_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M4_SIM_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M4_SIM_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M4_SIM_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M4" & input$base_type=="DIF" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M4_DIF_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M4_DIF_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M4_DIF_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M4_DIF_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M4" & input$base_type=="DIF" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M4_DIF_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M4_DIF_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M4_DIF_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M4_DIF_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M4" & input$base_type=="DIF" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M4_DIF_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M4_DIF_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M4_DIF_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M4_DIF_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M5" & input$base_type=="SIM" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M5_SIM_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M5_SIM_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M5_SIM_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M5_SIM_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M5" & input$base_type=="SIM" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M5_SIM_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M5_SIM_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M5_SIM_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M5_SIM_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M5" & input$base_type=="SIM" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M5_SIM_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M5_SIM_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M5_SIM_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M5_SIM_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M5" & input$base_type=="DIF" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M5_DIF_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M5_DIF_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M5_DIF_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M5_DIF_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M5" & input$base_type=="DIF" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M5_DIF_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M5_DIF_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M5_DIF_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M5_DIF_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M5" & input$base_type=="DIF" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M5_DIF_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M5_DIF_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M5_DIF_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M5_DIF_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M6" & input$base_type=="SIM" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M6_SIM_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M6_SIM_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M6_SIM_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M6_SIM_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M6" & input$base_type=="SIM" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M6_SIM_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M6_SIM_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M6_SIM_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M6_SIM_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M6" & input$base_type=="SIM" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M6_SIM_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M6_SIM_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M6_SIM_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M6_SIM_V3[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M6" & input$base_type=="DIF" & input$risk_version=="v1"){
      
      data_temp2$sim_rme1 <- data_risk$M6_DIF_V1[1:3107]
      data_temp2$sim_rme2 <- data_risk$M6_DIF_V1[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M6_DIF_V1[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M6_DIF_V1[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M6" & input$base_type=="DIF" & input$risk_version=="v2"){
      
      data_temp2$sim_rme1 <- data_risk$M6_DIF_V2[1:3107]
      data_temp2$sim_rme2 <- data_risk$M6_DIF_V2[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M6_DIF_V2[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M6_DIF_V2[(3107*3+1):(3107*4)]
      
    }else if(input$mod_type=="M6" & input$base_type=="DIF" & input$risk_version=="v3"){
      
      data_temp2$sim_rme1 <- data_risk$M6_DIF_V3[1:3107]
      data_temp2$sim_rme2 <- data_risk$M6_DIF_V3[(3107+1):(3107*2)]
      data_temp2$sim_rme3 <- data_risk$M6_DIF_V3[(3107*2+1):(3107*3)]
      data_temp2$sim_rme4 <- data_risk$M6_DIF_V3[(3107*3+1):(3107*4)]
      
    }
    
    data_temp2
  })
  
  data.sp.w <- reactive({
    
    if(input$mod_type=="M0"){
      
      w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M0") %>% select(V1) %>% pull()
      w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M0") %>% select(V1) %>% pull()
      w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M0") %>% select(V1) %>% pull()
      w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M0") %>% select(V1) %>% pull()
      
    }else if(input$mod_type=="M1"){
      
      if(input$risk_version=="v1"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M1") %>% select(V1) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M1") %>% select(V1) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M1") %>% select(V1) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M1") %>% select(V1) %>% pull()
      }else if(input$risk_version=="v2"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M1") %>% select(V2) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M1") %>% select(V2) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M1") %>% select(V2) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M1") %>% select(V2) %>% pull()
      }else if(input$risk_version=="v3"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M1") %>% select(V3) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M1") %>% select(V3) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M1") %>% select(V3) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M1") %>% select(V3) %>% pull()
      }
      
    }else if(input$mod_type=="M2"){
      
      if(input$risk_version=="v1"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M2") %>% select(V1) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M2") %>% select(V1) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M2") %>% select(V1) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M2") %>% select(V1) %>% pull()
      }else if(input$risk_version=="v2"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M2") %>% select(V2) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M2") %>% select(V2) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M2") %>% select(V2) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M2") %>% select(V2) %>% pull()
      }else if(input$risk_version=="v3"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M2") %>% select(V3) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M2") %>% select(V3) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M2") %>% select(V3) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M2") %>% select(V3) %>% pull()
      }
      
      
    }else if(input$mod_type=="M3"){
      
      if(input$risk_version=="v1"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M3") %>% select(V1) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M3") %>% select(V1) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M3") %>% select(V1) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M3") %>% select(V1) %>% pull()
      }else if(input$risk_version=="v2"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M3") %>% select(V2) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M3") %>% select(V2) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M3") %>% select(V2) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M3") %>% select(V2) %>% pull()
      }else if(input$risk_version=="v3"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M3") %>% select(V3) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M3") %>% select(V3) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M3") %>% select(V3) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M3") %>% select(V3) %>% pull()
      }
      
    }else if(input$mod_type=="M4"){
      
      if(input$risk_version=="v1"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M4") %>% select(V1) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M4") %>% select(V1) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M4") %>% select(V1) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M4") %>% select(V1) %>% pull()
      }else if(input$risk_version=="v2"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M4") %>% select(V2) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M4") %>% select(V2) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M4") %>% select(V2) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M4") %>% select(V2) %>% pull()
      }else if(input$risk_version=="v3"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M4") %>% select(V3) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M4") %>% select(V3) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M4") %>% select(V3) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M4") %>% select(V3) %>% pull()
      }
      
    }else if(input$mod_type=="M5"){
      
      if(input$risk_version=="v1"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M5") %>% select(V1) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M5") %>% select(V1) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M5") %>% select(V1) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M5") %>% select(V1) %>% pull()
      }else if(input$risk_version=="v2"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M5") %>% select(V2) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M5") %>% select(V2) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M5") %>% select(V2) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M5") %>% select(V2) %>% pull()
      }else if(input$risk_version=="v3"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M5") %>% select(V3) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M5") %>% select(V3) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M5") %>% select(V3) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M5") %>% select(V3) %>% pull()
      }
      
    }else if(input$mod_type=="M6"){
      
      if(input$risk_version=="v1"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M6") %>% select(V1) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M6") %>% select(V1) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M6") %>% select(V1) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M6") %>% select(V1) %>% pull()
      }else if(input$risk_version=="v2"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M6") %>% select(V2) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M6") %>% select(V2) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M6") %>% select(V2) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M6") %>% select(V2) %>% pull()
      }else if(input$risk_version=="v3"){
        w_p1 <- data_weights %>% filter(SP_EF==1, Model=="M6") %>% select(V3) %>% pull()
        w_p2 <- data_weights %>% filter(SP_EF==2, Model=="M6") %>% select(V3) %>% pull()
        w_p3 <- data_weights %>% filter(SP_EF==3, Model=="M6") %>% select(V3) %>% pull()
        w_p4 <- data_weights %>% filter(SP_EF==4, Model=="M6") %>% select(V3) %>% pull()
      }
      
    }
    
    data_w_temp <- c(w_p1, w_p2, w_p3, w_p4)
    data_w_temp
  })
  
  observeEvent(input$action, output$plot_rme_sim <- renderPlot({
  
    if(input$sp_ef_mod=="M0"){
      
      fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
      fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
      fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
      fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
      fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
      fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
      fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
      fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
      
    }else if(input$sp_ef_mod=="M1"){
    
      fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
      fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
      fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
      fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
      fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
      fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
      fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
      fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M2-ind(F1L1-F2L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M2-ind(F1L2-F2L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M2-ind(F2L1-F1L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M2-ind(F2L2-F1L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M3-F1.(F1L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")

  }else if(input$sp_ef_mod %in% c("M3-F1.(F1L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M4-F2.(F2L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M4-F2.(F2L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M5-F1.(F1L1)+F2.(F2L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M5-F1.(F1L2)+F2.(F2L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M5-F1.(F1L1)+F2.(F2L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M5-F1.(F1L2)+F2.(F2L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme5", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M6-F1.(F1L1)*F2.(F2L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M6-F1.(F1L2)*F2.(F2L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M6-F1.(F1L1)*F2.(F2L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M6-F1.(F1L2)*F2.(F2L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M6-F2.(F2L1)*F1.(F1L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M6-F2.(F2L2)*F1.(F1L1)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M6-F2.(F2L1)*F1.(F1L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }else if(input$sp_ef_mod %in% c("M6-F2.(F2L2)*F1.(F1L2)")){
    
    fig1.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme4", title = "Simulated Risk - Group 1")
    fig1.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme1", title = "RME Adjusted - Group 1")
    fig2.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme2", title = "Simulated Risk - Group 2")
    fig2.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme2", title = "RME Adjusted - Group 2")
    fig3.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme3", title = "Simulated Risk - Group 3")
    fig3.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme3", title = "RME Adjusted - Group 3")
    fig4.1 <- plot_rme(sf_obj = data.plot.RME.sim(), fill_by = "sim_rme1", title = "Simulated Risk - Group 4")
    fig4.2 <- plot_rme(sf_obj = data.plot.RME(), fill_by = "rme4", title = "RME Adjusted - Group 4")
    
  }
  
    grid.arrange(fig1.1, fig1.2, fig2.1, fig2.2, fig3.1, fig3.2, fig4.1, fig4.2, ncol=2)
  
  }, height = 1500))
  
  observeEvent(input$action, output$plot_sp_sim <- renderPlot({
    
    if(input$sp_ef_mod=="M0"){
      
      fig1.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp1", title = "Simulated Spatial Effect 1")
      fig1.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp1", title = bquote(bold("Heterogeneity Effect Adjusted | "~omega[1])))
      fig2.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp2", title = "Simulated Spatial Effect 2")
      fig2.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp2", title = bquote(bold("Heterogeneity Effect Adjusted | "~omega[2])))
      fig3.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp3", title = "Simulated Spatial Effect 3")
      fig3.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp3", title = bquote(bold("Heterogeneity Effect Adjusted | "~omega[3])))
      fig4.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp4", title = "Simulated Spatial Effect 4")
      fig4.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp4", title = bquote(bold("Heterogeneity Effect Adjusted | "~omega[4])))
      
    }else if(input$sp_ef_mod=="M1"){
      
      fig1.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp1", title = "Simulated Spatial Effect 1", weight = data.sp.w()[1])
      fig1.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp1", title = bquote(bold("Spatial Effect | "~phi[1])))
      fig2.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp2", title = "Simulated Spatial Effect 2", weight = data.sp.w()[2])
      fig2.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp2", title = bquote(bold("Spatial Effect | "~phi[2])))
      fig3.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp3", title = "Simulated Spatial Effect 3", weight = data.sp.w()[3])
      fig3.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp3", title = bquote(bold("Spatial Effect | "~phi[3])))
      fig4.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp4", title = "Simulated Spatial Effect 4", weight = data.sp.w()[4])
      fig4.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp4", title = bquote(bold("Spatial Effect | "~phi[4])))
      
    }else if(input$sp_ef_mod %in% c("M2-ind(F1L1-F2L1)", "M2-ind(F1L2-F2L1)", "M2-ind(F2L1-F1L2)", "M2-ind(F2L2-F1L2)")){
      
      fig1.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp1", title = "Simulated Spatial Effect 1", weight = data.sp.w()[1])
      fig1.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp1", title = bquote(bold("Spatial Effect | "~phi[11])))
      fig2.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp2", title = "Simulated Spatial Effect 2", weight = data.sp.w()[2])
      fig2.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp2", title = NULL)
      fig3.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp3", title = "Simulated Spatial Effect 3", weight = data.sp.w()[3])
      fig3.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp3", title = NULL)
      fig4.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp4", title = "Simulated Spatial Effect 4", weight = data.sp.w()[4])
      fig4.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp4", title = NULL)
      
    }else if(input$sp_ef_mod %in% c("M3-F1.(F1L1)", "M3-F1.(F1L2)")){
      
      fig1.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp1", title = "Simulated Spatial Effect 1", weight = data.sp.w()[1])
      fig1.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp1", title = bquote(bold("Spatial Effect | "~phi[11])))
      fig2.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp2", title = "Simulated Spatial Effect 2", weight = data.sp.w()[2])
      fig2.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp2", title = bquote(bold("Spatial Effect | "~phi[12])))
      fig3.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp3", title = "Simulated Spatial Effect 3", weight = data.sp.w()[3])
      fig3.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp3", title = NULL)
      fig4.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp4", title = "Simulated Spatial Effect 4", weight = data.sp.w()[4])
      fig4.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp4", title = NULL)
      
    }else if(input$sp_ef_mod %in% c("M4-F2.(F2L1)", "M4-F2.(F2L2)")){
      
      fig1.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp1", title = "Simulated Spatial Effect 1", weight = data.sp.w()[1])
      fig1.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp1", title = bquote(bold("Spatial Effect | "~phi[11])))
      fig2.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp2", title = "Simulated Spatial Effect 2", weight = data.sp.w()[2])
      fig2.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp2", title = bquote(bold("Spatial Effect | "~phi[21])))
      fig3.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp3", title = "Simulated Spatial Effect 3", weight = data.sp.w()[3])
      fig3.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp3", title = NULL)
      fig4.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp4", title = "Simulated Spatial Effect 4", weight = data.sp.w()[4])
      fig4.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp4", title = NULL)
      
    }else if(input$sp_ef_mod %in% c("M5-F1.(F1L1)+F2.(F2L1)", "M5-F1.(F1L2)+F2.(F2L1)", "M5-F1.(F1L1)+F2.(F2L2)", "M5-F1.(F1L2)+F2.(F2L2)")){
      
      fig1.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp1", title = "Simulated Spatial Effect 1", weight = data.sp.w()[1])
      fig1.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp1", title = bquote(bold("Spatial Effect | "~phi[11])))
      fig2.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp2", title = "Simulated Spatial Effect 2", weight = data.sp.w()[2])
      fig2.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp2", title = bquote(bold("Spatial Effect | "~phi[12])))
      fig3.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp3", title = "Simulated Spatial Effect 3", weight = data.sp.w()[3])
      fig3.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp3", title = bquote(bold("Spatial Effect | "~phi[21])))
      fig4.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp4", title = "Simulated Spatial Effect 4", weight = data.sp.w()[4])
      fig4.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp4", title = NULL)
      
    }else if(input$sp_ef_mod %in% c("M6-F1.(F1L1)*F2.(F2L1)", "M6-F1.(F1L2)*F2.(F2L1)", "M6-F1.(F1L1)*F2.(F2L2)", "M6-F1.(F1L2)*F2.(F2L2)",
                                    "M6-F2.(F2L1)*F1.(F1L1)", "M6-F2.(F2L2)*F1.(F1L1)", "M6-F2.(F2L1)*F1.(F1L2)", "M6-F2.(F2L2)*F1.(F1L2)")){
      
      fig1.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp1", title = "Simulated Spatial Effect 1", weight = data.sp.w()[1])
      fig1.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp1", title = bquote(bold("Spatial Effect | "~phi[11])))
      fig2.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp2", title = "Simulated Spatial Effect 2", weight = data.sp.w()[2])
      fig2.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp2", title = bquote(bold("Spatial Effect | "~phi[12])))
      fig3.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp3", title = "Simulated Spatial Effect 3", weight = data.sp.w()[3])
      fig3.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp3", title = bquote(bold("Spatial Effect | "~phi[21])))
      fig4.1 <- plot_spef(sf_obj = data.plot.sp.sim(), fill_by = "sim_sp4", title = "Simulated Spatial Effect 4", weight = data.sp.w()[4])
      fig4.2 <- plot_spef(sf_obj = data.plot.sp(), fill_by = "sp4", title = bquote(bold("Spatial Effect | "~phi[22])))
      
    }
    
    grid.arrange(fig1.1, fig1.2, fig2.1, fig2.2, fig3.1, fig3.2, fig4.1, fig4.2, ncol=2)
    
  }, height = 1500))
  
  # Resultados modelo
  case.table.data <- reactive({
    
    data_table <- get(input$vars)
    data_table <- data_table$Summary 
    data_table$NUM <- 1:22
    data_table <- data_table %>% select(NUM, MODEL, DIC, WAIC, sp.null, CPU) %>% mutate(CPU=round(CPU, 2), DIC=round(DIC, 2), WAIC=round(WAIC, 2)) 
    data_table
    
  })
  
  output$tabla_summary <- renderDataTable({
    datatable(case.table.data(), 
              options = list(pageLength = 25, initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#133BF2', 'color': 'white'});",
                "}")),
              escape = FALSE,rownames = FALSE)  %>%
      formatStyle(c("NUM", "MODEL", "DIC", "WAIC", "CPU", "sp.null"), fontWeight = "bold") %>% 
      formatStyle('NUM', "sp.null", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('MODEL', "sp.null", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('DIC', "sp.null", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('WAIC', "sp.null", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('CPU', "sp.null", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red')))  %>% 
      formatStyle('sp.null', "sp.null", color  = styleEqual(c(0, 1, 2, 3, 4), c('black', 'red', 'red', 'red', 'red'))) 
    
  })
  
  case.mod.data <- reactive({
    
    data_mod <- get(input$vars)
    data_mod
  })
  
  n.mod <- reactive({
    
    data_mod <- as.numeric(input$n_mod)
    data_mod
    
  })
    
    
  
  output$plot_sp <- renderPlot({
    
    fig <- plot.SpANOVA(
      obj=case.mod.data(),
      obj_type="SpANOVA",
      fill_by="Spatial",
      n_mod=n.mod(),
      sp_obj=mun_cv_df,
      fil_scale=c("#133BF2", "#7189F7", "#FFFFFF", "#FF867A", "#FF2F1B"),
      col_frontiers="black",
      scale_name="Values",
      sp_null=0.125,
      legend.position="right",
      ncol_fig=4
    )
    
    fig
    
  }, height = 500)
  
  output$plot_rme <- renderPlot({
  
    fig <- plot.SpANOVA(
      obj=case.mod.data(),
      obj_type="SpANOVA",
      fill_by="RR",
      n_mod=n.mod(),
      sp_obj=mun_cv_df,
      fil_scale=c("#133BF2", "#7189F7", "#FFFFFF", "#FF867A", "#FF2F1B"),
      col_frontiers="black",
      scale_name="Values",
      sp_null=0.125,
      legend.position="right",
      ncol_fig=4
    )
    
    fig

  
  } , height = 500)
  
  
  ###################################
  data.table_sense <- reactive({
    if(input$mod_type_sens=="M0"){ 
      n_mod <- paste0(input$mod_type_sens, "_", input$base_type_sens) 
      data_temp <- sens_data[[which(names(sens_data)==n_mod)]]
      data_temp$row_col <- c("Perfect", rep("No", 21))
    }else{
      n_mod <- paste0(input$mod_type_sens, "_", input$base_type_sens, "_", input$risk_version_sens)
      data_temp <- sens_data[[which(names(sens_data)==n_mod)]]
      if(input$mod_type_sens=="M1"){data_temp$row_col <- c("No", "Perfect", rep("No", 20))}
      if(input$mod_type_sens=="M2"){data_temp$row_col <- c("No", "No", rep("Perfect", 4), rep("No", 16))}
      if(input$mod_type_sens=="M3"){data_temp$row_col <- c(rep("No", 6), "Perfect", "Yes", rep("No", 14))}
      if(input$mod_type_sens=="M4"){data_temp$row_col <- c(rep("No", 8), "Yes", "Perfect", rep("No", 12))}
      if(input$mod_type_sens=="M5"){data_temp$row_col <- c(rep("No", 10), "Yes", "Perfect", "Yes", "Yes", rep("No", 8))}
      if(input$mod_type_sens=="M6"){data_temp$row_col <- c(rep("No", 14), "Yes", "Yes",  "Perfect", rep("Yes", 5))}
    }
    data_temp$null_ef_v1 <- ifelse(str_detect(data_temp$V1, "\\[0\\]"), "0", "1 or more")
    data_temp$null_ef_v2 <- ifelse(str_detect(data_temp$V2, "\\[0\\]"), "0", "1 or more")
    data_temp$null_ef_v3 <- ifelse(str_detect(data_temp$V3, "\\[0\\]"), "0", "1 or more")
    data_temp$null_ef_v4 <- ifelse(str_detect(data_temp$V4, "\\[0\\]"), "0", "1 or more")
    data_temp$null_ef_v5 <- ifelse(str_detect(data_temp$V5, "\\[0\\]"), "0", "1 or more")
    data_temp$null_ef_v6 <- ifelse(str_detect(data_temp$V6, "\\[0\\]"), "0", "1 or more")
    data_temp$null_ef_v7 <- ifelse(str_detect(data_temp$V7, "\\[0\\]"), "0", "1 or more")
    data_temp$null_ef_v8 <- ifelse(str_detect(data_temp$V8, "\\[0\\]"), "0", "1 or more")
    data_temp
  })
  
  output$table_sense <- renderDataTable({datatable(data.table_sense(), 
                                                   options = list(pageLength = 25, initComplete = JS(
                                                     "function(settings, json) {",
                                                     "$(this.api().table().header()).css({'background-color': '#133BF2', 'color': 'white'});",
                                                     "}"), 
                                                     columnDefs = list(list(visible = FALSE, targets = names(data.table_sense())[10:18]))),
                                                   escape = FALSE,rownames = FALSE)  %>%
      formatStyle(c("Mod", "V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"), fontWeight = "bold") %>% 
      formatStyle('row_col', target = 'row', backgroundColor = styleEqual(c("No", "Yes", "Perfect"), c('white', '#b2b2ff', "#A1FB63"))) %>% 
      formatStyle('V1', "null_ef_v1", color  = styleEqual(c("0", "1 or more"), c('black', 'red'))) %>% 
      formatStyle('V2', "null_ef_v2", color  = styleEqual(c("0", "1 or more"), c('black', 'red'))) %>% 
      formatStyle('V3', "null_ef_v3", color  = styleEqual(c("0", "1 or more"), c('black', 'red'))) %>% 
      formatStyle('V4', "null_ef_v4", color  = styleEqual(c("0", "1 or more"), c('black', 'red'))) %>% 
      formatStyle('V5', "null_ef_v5", color  = styleEqual(c("0", "1 or more"), c('black', 'red'))) %>% 
      formatStyle('V6', "null_ef_v6", color  = styleEqual(c("0", "1 or more"), c('black', 'red'))) %>% 
      formatStyle('V7', "null_ef_v7", color  = styleEqual(c("0", "1 or more"), c('black', 'red'))) %>% 
      formatStyle('V8', "null_ef_v8", color  = styleEqual(c("0", "1 or more"), c('black', 'red'))) 
    
  })
  
  output$table_version <- renderDataTable({
    
    data_tab <- data.frame(
      "Version" = c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8"), 
      "Prior" = c(rep("PC Prior", 4), rep("Sdunif", 4)), 
      "sp.copy.fixed" = c(rep("FALSE", 2), rep("TRUE", 2), rep("FALSE", 2), rep("TRUE", 2)),
      "scale.mod" = c(rep(c("FALSE", "TRUE"), 4))
      )
    
    datatable(data_tab, 
              options = list(initComplete = JS(
                "function(settings, json) {",
                "$(this.api().table().header()).css({'background-color': '#133BF2', 'color': 'white'});",
                "}")),
              escape = FALSE,rownames = FALSE)  %>%
      formatStyle(c("Version", "Prior", "sp.copy.fixed", "scale.mod"), fontWeight = "bold") %>% 
      formatStyle('Prior', backgroundColor = styleEqual(c("PC Prior", "Sdunif"), c("#A1FB63", "white"))) %>% 
      formatStyle('sp.copy.fixed', backgroundColor = styleEqual(c("FALSE", "TRUE"), c("#FF867A", "#7189F7"))) %>% 
      formatStyle('scale.mod', backgroundColor = styleEqual(c("FALSE", "TRUE"), c("#FF867A", "#7189F7")))
    
  })
  
  
  
  data.sense <- reactive({
    if(input$mod_type_sens=="M0"){ 
      n_mod <- paste0(input$mod_type_sens, "_", input$base_type_sens, "_SpANOVA") 
      data_temp <- hyperpam_dbs %>% filter(mod_spec==n_mod)
      }else{
      n_mod <- paste0(input$mod_type_sens, "_", input$base_type_sens, "_SpANOVA", "_", input$risk_version_sens)
      data_temp <- hyperpam_dbs %>% filter(mod_spec==n_mod)
      }
    data_temp
  })
  
  
  output$plot_m1_sens <- renderPlot({
    
    data_fig1 <- data.sense() %>% 
      filter(effect=="Precision for phi_1", sim_type=="M1")
    
    data_fig2 <- data.sense() %>% 
      filter(effect=="Precision for phi_2", sim_type=="M1")
    
    data_fig3 <- data.sense() %>% 
      filter(effect=="Precision for phi_3", sim_type=="M1")
    
    data_fig4 <- data.sense() %>% 
      filter(effect=="Precision for phi_4", sim_type=="M1")
    
    fig1 <- ggplot(data = data_fig1) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_1") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig2 <- ggplot(data = data_fig2) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_2") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig3 <- ggplot(data = data_fig3) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_3") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig4 <- ggplot(data = data_fig4) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_4") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    
    grid.arrange(fig1, fig2, fig3, fig4, ncol=2)

    
  }, height = 500)
  
  output$plot_m2_sens <- renderPlot({
    
    data_fig1 <- data.sense() %>% 
      filter(effect=="Precision for phi_11", sim_type=="M2.(F1L1)")
    
    data_fig2 <- data.sense() %>% 
      filter(effect=="Precision for phi_11", sim_type=="M2.(F1L2)")
    
    data_fig3 <- data.sense() %>% 
      filter(effect=="Precision for phi_11", sim_type=="M2.(F2L1)")
    
    data_fig4 <- data.sense() %>% 
      filter(effect=="Precision for phi_11", sim_type=="M2.(F2L2)")
    
    fig1 <- ggplot(data = data_fig1) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M2.(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig2 <- ggplot(data = data_fig2) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M2.(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig3 <- ggplot(data = data_fig3) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M2.(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig4 <- ggplot(data = data_fig4) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M2.(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    grid.arrange(fig1, fig2, fig3, fig4, ncol=2)
    
    
  }, height = 500)
  
  output$plot_m3_sens <- renderPlot({
    
    data_fig1 <- data.sense() %>% 
      filter(effect=="Precision for phi_11", sim_type=="M3.F1(F1L1)")
    
    data_fig2 <- data.sense() %>% 
      filter(effect=="Precision for phi_21", sim_type=="M3.F1(F1L1)")
    
    data_fig3 <- data.sense() %>% 
      filter(effect=="Precision for phi_11", sim_type=="M3.F1(F1L2)")
    
    data_fig4 <- data.sense() %>% 
      filter(effect=="Precision for phi_21", sim_type=="M3.F1(F1L2)")
    
    fig1 <- ggplot(data = data_fig1) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M3.F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig2 <- ggplot(data = data_fig2) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M3.F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig3 <- ggplot(data = data_fig3) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M3.F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig4 <- ggplot(data = data_fig4) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M3.F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    grid.arrange(fig1, fig2, fig3, fig4, ncol=2)
    
    
  }, height = 500)
  
  output$plot_m4_sens <- renderPlot({
    
    data_fig1 <- data.sense() %>% 
      filter(effect=="Precision for phi_11", sim_type=="M4.F2(F2L1)")
    
    data_fig2 <- data.sense() %>% 
      filter(effect=="Precision for phi_12", sim_type=="M4.F2(F2L1)")
    
    data_fig3 <- data.sense() %>% 
      filter(effect=="Precision for phi_11", sim_type=="M4.F2(F2L2)")
    
    data_fig4 <- data.sense() %>% 
      filter(effect=="Precision for phi_12", sim_type=="M4.F2(F2L2)")
    
    fig1 <- ggplot(data = data_fig1) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M4.F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig2 <- ggplot(data = data_fig2) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M4.F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig3 <- ggplot(data = data_fig3) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M4.F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig4 <- ggplot(data = data_fig4) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M4.F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    grid.arrange(fig1, fig2, fig3, fig4, ncol=2)
    
    
  }, height = 500)
  
  output$plot_m5_sens <- renderPlot({
    
    data_fig1 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M5.F1(F1L1)+F2(F2L1)")
    data_fig2 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M5.F1(F1L1)+F2(F2L1)")
    data_fig3 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M5.F1(F1L1)+F2(F2L1)")
    
    data_fig4 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M5.F1(F1L2)+F2(F2L1)")
    data_fig5 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M5.F1(F1L2)+F2(F2L1)")
    data_fig6 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M5.F1(F1L2)+F2(F2L1)")
    
    data_fig7 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M5.F1(F1L1)+F2(F2L2)")
    data_fig8 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M5.F1(F1L1)+F2(F2L2)")
    data_fig9 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M5.F1(F1L1)+F2(F2L2)")
    
    data_fig10 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M5.F1(F1L2)+F2(F2L2)")
    data_fig11 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M5.F1(F1L2)+F2(F2L2)")
    data_fig12 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M5.F1(F1L2)+F2(F2L2)")
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig1 <- ggplot(data = data_fig1) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M5.F1(F1L1)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig2 <- ggplot(data = data_fig2) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L1)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig3 <- ggplot(data = data_fig3) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M5.F1(F1L1)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig4 <- ggplot(data = data_fig4) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L2)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig5 <- ggplot(data = data_fig5) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L2)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig6 <- ggplot(data = data_fig6) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L2)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig7 <- ggplot(data = data_fig7) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L1)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig8 <- ggplot(data = data_fig8) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L1)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig9 <- ggplot(data = data_fig9) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L1)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig10 <- ggplot(data = data_fig10) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L2)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig11 <- ggplot(data = data_fig11) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L2)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig12 <- ggplot(data = data_fig12) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M5.F1(F1L2)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    grid.arrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10, fig11, fig12, ncol=3)
    
    
  }, height = 1000)
  
  output$plot_m6_sens <- renderPlot({
    
    data_fig1 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M6.F1(F1L1)*F2(F2L1)")
    data_fig2 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M6.F1(F1L1)*F2(F2L1)")
    data_fig3 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M6.F1(F1L1)*F2(F2L1)")
    data_fig4 <- data.sense() %>% filter(effect=="Precision for phi_22", sim_type=="M6.F1(F1L1)*F2(F2L1)")
    
    data_fig5 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M6.F1(F1L2)*F2(F2L1)")
    data_fig6 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M6.F1(F1L2)*F2(F2L1)")
    data_fig7 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M6.F1(F1L2)*F2(F2L1)")
    data_fig8 <- data.sense() %>% filter(effect=="Precision for phi_22", sim_type=="M6.F1(F1L2)*F2(F2L1)")
    
    data_fig9 <- data.sense()  %>% filter(effect=="Precision for phi_11", sim_type=="M6.F1(F1L1)*F2(F2L2)")
    data_fig10 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M6.F1(F1L1)*F2(F2L2)")
    data_fig11 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M6.F1(F1L1)*F2(F2L2)")
    data_fig12 <- data.sense() %>% filter(effect=="Precision for phi_22", sim_type=="M6.F1(F1L1)*F2(F2L2)")
    
    data_fig13 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M6.F1(F1L2)*F2(F2L2)")
    data_fig14 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M6.F1(F1L2)*F2(F2L2)")
    data_fig15 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M6.F1(F1L2)*F2(F2L2)")
    data_fig16 <- data.sense() %>% filter(effect=="Precision for phi_22", sim_type=="M6.F1(F1L2)*F2(F2L2)")
    
    data_fig17 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M6.F2(F2L1)*F1(F1L1)")
    data_fig18 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M6.F2(F2L1)*F1(F1L1)")
    data_fig19 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M6.F2(F2L1)*F1(F1L1)")
    data_fig20 <- data.sense() %>% filter(effect=="Precision for phi_22", sim_type=="M6.F2(F2L1)*F1(F1L1)")
    
    data_fig21 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M6.F2(F2L1)*F1(F1L2)")
    data_fig22 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M6.F2(F2L1)*F1(F1L2)")
    data_fig23 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M6.F2(F2L1)*F1(F1L2)")
    data_fig24 <- data.sense() %>% filter(effect=="Precision for phi_22", sim_type=="M6.F2(F2L1)*F1(F1L2)")
    
    data_fig25 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M6.F2(F2L2)*F1(F1L1)")
    data_fig26 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M6.F2(F2L2)*F1(F1L1)")
    data_fig27 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M6.F2(F2L2)*F1(F1L1)")
    data_fig28 <- data.sense() %>% filter(effect=="Precision for phi_22", sim_type=="M6.F2(F2L2)*F1(F1L1)")
    
    data_fig29 <- data.sense() %>% filter(effect=="Precision for phi_11", sim_type=="M6.F2(F2L2)*F1(F1L2)")
    data_fig30 <- data.sense() %>% filter(effect=="Precision for phi_12", sim_type=="M6.F2(F2L2)*F1(F1L2)")
    data_fig31 <- data.sense() %>% filter(effect=="Precision for phi_21", sim_type=="M6.F2(F2L2)*F1(F1L2)")
    data_fig32 <- data.sense() %>% filter(effect=="Precision for phi_22", sim_type=="M6.F2(F2L2)*F1(F1L2)")
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig1 <- ggplot(data = data_fig1) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M6.F1(F1L1)*F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig2 <- ggplot(data = data_fig2) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M6.F1(F1L1)*F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig3 <- ggplot(data = data_fig3) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M6.F1(F1L1)*F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig4 <- ggplot(data = data_fig4) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_22 - M6.F1(F1L1)*F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig5 <- ggplot(data = data_fig5) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M6.F1(F1L2)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig6 <- ggplot(data = data_fig6) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M6.F1(F1L2)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig7 <- ggplot(data = data_fig7) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M6.F1(F1L2)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig8 <- ggplot(data = data_fig8) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_22 - M6.F1(F1L2)+F2(F2L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig9 <- ggplot(data = data_fig9) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M6.F1(F1L1)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig10 <- ggplot(data = data_fig10) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M6.F1(F1L1)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig11 <- ggplot(data = data_fig11) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M6.F1(F1L1)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig12 <- ggplot(data = data_fig12) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_22 - M6.F1(F1L1)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig13 <- ggplot(data = data_fig13) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M6.F1(F1L2)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig14 <- ggplot(data = data_fig14) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M6.F1(F1L2)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig15 <- ggplot(data = data_fig15) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M6.F1(F1L2)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig16 <- ggplot(data = data_fig16) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_22 - M6.F1(F1L2)+F2(F2L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig17 <- ggplot(data = data_fig17) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M6.F2(F2L1)+F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig18 <- ggplot(data = data_fig18) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M6.F2(F2L1)+F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig19 <- ggplot(data = data_fig19) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M6.F2(F2L1)+F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig20 <- ggplot(data = data_fig20) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_22 - M6.F2(F2L1)+F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig21 <- ggplot(data = data_fig21) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M6.F2(F2L1)+F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig22 <- ggplot(data = data_fig22) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M6.F2(F2L1)+F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig23 <- ggplot(data = data_fig23) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M6.F2(F2L1)+F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig24 <- ggplot(data = data_fig24) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_22 - M6.F2(F2L1)+F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig25 <- ggplot(data = data_fig25) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M6.F2(F2L2)+F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig26 <- ggplot(data = data_fig26) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M6.F2(F2L2)+F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig27 <- ggplot(data = data_fig27) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M6.F2(F2L2)+F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig28 <- ggplot(data = data_fig28) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_22 - M6.F2(F2L2)+F1(F1L1)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    fig29 <- ggplot(data = data_fig29) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_11 - M6.F2(F2L2)+F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig30 <- ggplot(data = data_fig30) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_12 - M6.F2(F2L2)+F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig31 <- ggplot(data = data_fig31) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_21 - M6.F2(F2L2)+F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    fig32 <- ggplot(data = data_fig32) +
      geom_point(aes(x = Version, y = mean, color = Version)) + 
      geom_errorbar(aes(x = Version, ymax = `0.975quant`, ymin = `0.025quant`, color = Version)) +
      xlab(NULL) + ylab(NULL) + ggtitle("Variance of phi_22 - M6.F2(F2L2)+F1(F1L2)") +
      theme(legend.position = "none", text = element_text(size = 17))
    
    #-----------------------------------------------------------------------------------------------------------#
    
    grid.arrange(fig1, fig2, fig3, fig4, fig5, fig6, fig7, fig8, fig9, fig10, fig11, fig12, fig13, fig14, fig15, fig16, 
                 fig17, fig18, fig19, fig20, fig21, fig22, fig23, fig24, fig25, fig26, fig27, fig28, fig29, fig30, fig31, fig32, 
                 ncol=4)
    
    
  }, height = 2200)

})