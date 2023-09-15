#' Create rounded survival table (to avoid individual observations)
tidy_surv <-
  function(
    survfit,
    times = NULL,
    addtimezero=FALSE
  ) {
    
    # tidy post-fit survival dataset, with extra estimates than provided by broom::tidy.coxph
    
    mintime <- min(survfit$time)
    timezero <- min(0, mintime-1)
    
    
    if (is.null(times)) {
      output <-
        survfit %>%
        broom::tidy() %>%
        transmute(
          time,
          lagtime = lag(time, 1L, default=timezero),
          leadtime = lead(time),
          interval = time - lagtime,
          
          n.risk,
          n.event,
          n.censor,
          
          summand = n.event / ((n.risk - n.event) * n.risk),
          
          surv=cumprod(1 - n.event / n.risk),
          #surv.ll = conf.low,
          #surv.ul = conf.high,
          surv.se = surv * sqrt(cumsum(summand)), #greenwood's formula
          
          lsurv = log(surv),
          lsurv.se = sqrt(cumsum(summand)),
          
          # kaplan meier hazard estimates
          haz = n.event / (n.risk * interval), # =-(surv-lag(surv))/lag(surv)
          cml.haz = cumsum(haz),
          cml.haz.se = std.error, # = surv.se/surv,
          haz.se = haz * sqrt((n.risk - n.event) / (n.risk * n.event)),
          
          
          # actuarial hazard estimates
          haz_ac = n.event / ((n.risk - (n.censor / 2) - (n.event / 2)) * interval), # =(cml.haz-lag(cml.haz))/interval
          cml.haz_ac = -log(surv), #=cumsum(haz_ac)
          haz_ac.se = (haz_ac * sqrt(1 - (haz_ac * interval / 2)^2)) / sqrt(n.event),
          
          # log(-log()) scale
          
          llsurv = log(-log(surv)),
          llsurv.se = sqrt((1 / log(surv)^2) * cumsum(summand)),
          
          surv.ll = exp(-exp(llsurv + qnorm(0.025)*llsurv.se)),
          surv.ul = exp(-exp(llsurv + qnorm(0.975)*llsurv.se)),
          
        )
    }
    
    else {
      
      output <-
        survfit %>%
        broom::tidy() %>%
        complete(
          time = times,
          fill = list(n.event = 0, n.censor = 0)
        ) %>%
        fill(n.risk, .direction = c("up")) %>%
        transmute(
          time,
          lagtime = lag(time, 1L, default = timezero),
          leadtime = lead(time),
          interval = time - lagtime,
          
          n.risk,
          n.event,
          n.censor,
          
          summand = n.event / ((n.risk - n.event) * n.risk),
          
          surv=cumprod(1 - n.event / n.risk),
          
          #surv.ll = conf.low,
          #surv.ul = conf.high,
          surv.se = surv * sqrt(cumsum(summand)), #greenwood's formula
          
          lsurv = log(surv),
          lsurv.se = sqrt(cumsum(summand)),
          
          # kaplan meier hazard estimates
          haz = n.event / (n.risk * interval), # =-(surv-lag(surv))/lag(surv)
          cml.haz.se = std.error, #  = surv.se/surv
          cml.haz = cumsum(haz), # =cumsum(haz_km)
          haz.se = haz * sqrt((n.risk - n.event) / (n.risk * n.event)),
          
          # actuarial hazard estimates
          haz_ac = n.event / ((n.risk - (n.censor / 2) - (n.event / 2)) * interval), # =(cml.haz-lag(cml.haz))/interval
          cml.haz_ac = -log(surv), #=cumsum(haz_ac)
          haz_ac.se = (haz_ac * sqrt(1 - (haz_ac * interval / 2)^2)) / sqrt(n.event),
          
          # log(-log()) scale
          
          llsurv = log(-log(surv)),
          llsurv.se = sqrt((1 / log(surv)^2) * cumsum(summand)),
          
          
          surv.ll = exp(-exp(llsurv + qnorm(0.025)*llsurv.se)),
          surv.ul = exp(-exp(llsurv + qnorm(0.975)*llsurv.se)),
        )
    }
    
    if(addtimezero){
      output <- output %>%
        add_row(
          time = timezero,
          lagtime = NA_real_,
          leadtime = mintime,
          interval = leadtime-time,
          summand=0,
          
          #estimate=1, std.error=0, conf.high=1, conf.low=1,
          
          surv=1,
          surv.ll=1,
          surv.ul=1,
          surv.se=0,
          
          haz=0, haz.se=0, cml.haz=0,
          haz_ac=0, haz_ac.se=0, cml.haz_ac=0,
          .before=1
        )
    }
    
    return(output)
  }

create_rounded_surv_table = function(input_data, threshold = 5) {
  
  plot_max <- max(input_data$time)
  
  sfit <- survfit(Surv(time, status) ~ 1, data = input_data, conf.type="log-log")
  
  surv_obj_tidy <- tidy_surv(sfit, times=seq_len(100), addtimezero = TRUE) # return survival table for each day of follow up
  
  mintime <- min(sfit$time)
  timezero <- min(0, mintime-1)
  
  data_surv_rounded <-
    surv_obj_tidy %>%
    mutate(
      lagtime = lag(time, 1L, default=timezero),
      N = plyr::round_any(max(n.risk, na.rm=TRUE),threshold),
      cml.event = cumsum(replace_na(n.event, 0)),
      cml.censor = cumsum(replace_na(n.censor, 0)),
      cml.event = floor_any(cml.event, threshold),
      cml.censor = floor_any(cml.censor, threshold),
      n.event = diff(c(0,cml.event)),
      n.censor = diff(c(0,cml.censor)),
      n.risk = N - lag(cml.event + cml.censor,1,0),
      summand = n.event / ((n.risk - n.event) * n.risk),
      surv = cumprod(1 - n.event / n.risk),
      surv.se = surv * sqrt(cumsum(replace_na(summand, 0))),
      llsurv = log(-log(surv)),
      llsurv.se = sqrt((1 / log(surv)^2) * cumsum(summand)),
      surv.ll = exp(-exp(llsurv + qnorm(0.025)*llsurv.se)),
      surv.ul = exp(-exp(llsurv + qnorm(0.975)*llsurv.se)),
      cum.in = 1 - surv,
      haz = -(surv-lag(surv, 1, 1))/lag(surv, 1, 1), # n.event / (n.risk * interval),
      haz.se = haz * sqrt((n.risk - n.event) / (n.risk* n.event)),
      cml.haz = cumsum(haz),
      cml.haz.se = surv.se/surv,
    ) %>%
    select(
      time,lagtime,
      n.risk, n.event, n.censor, summand,
      surv, surv.se, surv.ll, surv.ul,
      haz, haz.se,
      cml.haz, cml.haz.se
    )
  #write.csv(data_surv_rounded, "data_surv_rounded.csv")
  data_surv_rounded
}
