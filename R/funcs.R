# descriptor colors for sqi
getdsccol <- function(dscin = NULL, palout = F, palfac = NULL){
  
  if(is.factor(dscin))
    stop('dscin must be character')

  # relative severity levels
  l1 <- '#008000' #'Green'
  l2 <- '#90EE90' #'LightGreen'
  l3 <- '#FFB6C1' #'LightPink'
  l4 <- '#DC143C' #'Crimson'
  
  # additional cols for specific sqi categories
  l5 <- '#4F94CD' #'steelblue3'
  l6 <- '#00C5CD' #'turquoise3'
  l7 <- '#8B0000' #'red4'
  
  if(palout) return(c(l1, l2, l3, l4, l5, l6, l7))

  #  color categories
  StreamHealthIndex <- list(
    'Healthy and unstressed' = l1, 
    'Healthy and resilient' = l2,
    'Impacted by unknown stress' = l3,
    'Impacted and stressed' = l4
  )                                   
  BiologicalCondition <- list(
    'Healthy' = l1,
    'Impacted for CSCI' = l5,
    'Impacted for ASCI' = l6,
    'Impacted for CSCI and ASCI' = l4
  )
  OverallStressCondition <- list(
    'Low' = l1, 
    'Moderate' = l5,
    'Severe' = l4
  )
  OverallStressCondition_detail <- list(
    'Low stress' = l1, 
    'Stressed by chemistry degradation' = l5,
    'Stressed by habitat degradation' = l6,
    'Stressed by chemistry and habitat degradation' = l4, 
    'Stressed by low levels of chemistry or habitat degradation' = l7
  )

  # return individual palette with label is provided
  if(!is.null(palfac)){
    out <- get(palfac) %>% 
      unlist %>% 
      data.frame(nms = names(.), col = ., stringsAsFactors = F)
    return(out)
  }
  
  # get all
  allcol <- c(StreamHealthIndex, BiologicalCondition, OverallStressCondition, OverallStressCondition_detail)
  allcol <- unlist(allcol)
  
  # select colors
  out <- allcol[dscin]
  
  return(out)
  
}

#' Get relative color value for vline in relative distribution plots to match gauges
#'
#' @param val 
#' @param lims 
#' @param var 
#'
#' @return
#' @export
#'
#' @examples
relcol <- function(val, lims, var, gaugecols =c('#ff0000', '#f9c802', '#a9d70b')) {
  
  # color palette function
  pal_exp <- colorFactor(
    palette = c('#ff0000', '#f9c802', '#a9d70b'),
    na.color = 'yellow',
    levels = c('lo', 'md', 'hi'))
  
  # setup limits labels and breaks for increasing/decreasing
  lbs <- c('lo', 'md', 'hi')
  brks <- c(-Inf, lims[2], lims[3], Inf)
  if(lims[1] > lims[2]){
    lbs <- rev(lbs)
    brks <- c(Inf, lims[2], lims[3], -Inf) 
  }
  valcat <- cut(val, breaks = brks, labels = lbs, right = F)
  
  out <- pal_exp(valcat)
  
  return(out)
      
}

# get relative distribution plot
#
# catslng observed data for selected site, all variables
# dstdat all observed data for sites in selected region
# selvr chr str of selected variables to plot
# collims limits defining categories for selected variables
#
dst_fun <- function(catslng, dstdat, selvr, collims){

  # make plots
  out <- dstdat %>% 
    filter(var %in% selvr) %>% 
    rename(
      distval = val
      ) %>% 
    mutate(
      var = factor(var, levels = selvr)
    ) %>% 
    arrange(var) %>% 
    group_by(var) %>% 
    nest %>% 
    left_join(collims, by = 'var') %>% 
    left_join(catslng, by = 'var') %>% 
    mutate(
      plos = purrr::pmap(list(as.character(var), data, lims, val), function(var, data, lims, val){

        # color limits
        toplo <- data 
        
        ##
        # create plot
        
        # # get bins, have to use this for scaling density
        # if(var %in% c('Total nitrogen', 'Total phosphorus')){
        #   bins <- log10(1 + toplo$distval) %>%
        #     range %>%
        #     diff
        #   
        # } else {
        #   bins <- toplo$distval %>%
        #     range %>%
        #     diff
        # }
        # bins <- bins/25
        
        # color for vertical line
        valcol <- relcol(val, lims, var)
        
        # get %tile
        ptile <- ecdf(toplo$distval)(val) %>%
          round(2) %>%
          `*`(100)
        suff <- case_when(ptile %in% c(11,12,13) ~ "th",
                      ptile %% 10 == 1 ~ 'st',
                      ptile %% 10 == 2 ~ 'nd',
                      ptile %% 10 == 3 ~'rd',
                      TRUE ~ "th")
        ptile <- paste0(ptile, suff, ' %tile')
        
        p <- ggplot(toplo, aes(x = distval)) + 
          # geom_histogram(aes(y=..density..), fill = 'black', alpha = 0.2) + 
          # geom_density(aes(y=..density..), fill = scales::alpha('#99ccff', 0.4)) +
          geom_density(aes(y=..scaled..), fill = scales::alpha('#99ccff', 0.4)) +
          geom_rug(alpha = 1/2) +
          geom_vline(aes(xintercept = val), colour = valcol, size = 3, alpha = 0.7) +
          geom_vline(aes(xintercept = val), colour = valcol, size = 1, alpha = 1) + 
          theme(
            axis.title.y = element_blank(),
            panel.grid = element_blank(), 
            plot.title = element_text(size = 12)
          ) + 
          ggtitle(ptile)
        
        # add plot themes  
        p <- p +
          theme_bw() + 
          theme(
            axis.title.y = element_blank(),
            legend.position = 'none', 
            panel.grid = element_blank()
          ) +
          xlab(var)
        
        # log-axes if tn, tp
        if(var %in% c('Total nitrogen', 'Total phosphorus'))
          p <- p + scale_x_log10()
        
        return(p)
        
      })
    ) %>% 
    select(plos)
  
  return(out)
  
}

#' Create plot for expected site scores from landscape model and constraint classes
#'
#' @param cats input data for selected site
#'
#' @return
#' @export
#'
#' @examples
expplo_fun <- function(cats){
  
  # levels
  strcls_levs <- c('likely unconstrained', 'possibly unconstrained', 'possibly constrained', 'likely constrained')
  perf_levs <- c('under scoring', 'expected', 'over scoring')
  
  # color palette for stream expectations
  pal_exp <- colorFactor(
    palette = brewer.pal(9, 'Paired')[c(2, 1, 5, 6)],
    na.color = 'yellow',
    levels = strcls_levs
    )
  
  # remove geometry
  cats <- cats %>% 
    st_set_geometry(NULL)
  
  toplo1 <- cats %>%
    dplyr::select(COMID, MasterID, strcls, CSCI, strcls, lower, meds, upper) %>%
    dplyr::mutate(
      strcls = factor(strcls, levels = strcls_levs),
      perf_mlt = case_when(
        CSCI < lower ~ 'under scoring', 
        CSCI > upper ~ 'over scoring', 
        CSCI >= lower & CSCI <= upper ~ 'expected'
      ), 
      perf_mlt = factor(perf_mlt, levels = perf_levs)
    ) %>%
    dplyr::rename(
      `Stream Class` = strcls,
      `Relative\nscore` = perf_mlt
    ) %>% 
    tidyr::gather('var', 'val', lower, meds, upper)
  
  # total expected range
  toplo2 <- toplo1 %>%
    dplyr::select(COMID, MasterID, `Stream Class`, var, val)
    
  # median expectation
  toplo3 <- toplo1 %>%
    dplyr::select(COMID, MasterID, var, val) %>% 
    filter(var %in% 'meds')

  p <- ggplot(toplo1, aes(y = MasterID, x = val)) +
    geom_line(data = toplo1, aes(x = val, colour = `Stream Class`), alpha = 0.1, size = 2) +
    geom_line(aes(colour = `Stream Class`), alpha = 0.6, size = 2) +
    geom_point(data = toplo3, colour = 'white', size = 1, alpha = 1, shape = 15) +
    theme_minimal(base_family = 'serif', base_size = 16) +
    theme(
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 10),
      axis.ticks.y = element_blank(), 
      legend.position = 'top', 
      panel.grid = element_blank(),
      title = element_text(size = 14), 
      legend.text= element_text(size = 10),
      axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
      axis.ticks.x = element_line(colour = 'black', size=0.5, linetype='solid')
    ) +
    scale_x_continuous('CSCI', limits = c(0.1, 1.4), breaks = seq(0.1, 1.4, by = 0.2)) +
    scale_colour_manual('Stream segment class', values = pal_exp(levels(toplo1$`Stream Class`)), drop = F) +
    geom_point(aes(x = CSCI, fill = `Stream Class`, shape = `Relative\nscore`), size = 4.5, alpha = 0.8) +
    geom_vline(xintercept = 0.79, linetype = 'dashed', size = 1) +
    scale_shape_manual('Relative site score', values = c(25, 21, 24), drop = FALSE) +
    scale_fill_manual(values = pal_exp(levels(toplo1$`Stream Class`)), na.value = 'yellow', guide = F, drop = F) +
    guides(shape = guide_legend(title.position = 'top'), colour = guide_legend(title.position = 'top', ncol = 2))
  
  return(p)
  
}
  
#' Format data for download
#'
#' @param dat all data filtered by spatial selections
#'
#' @return
#' @export
#'
#' @examples
formdl_fun <- function(dat){

  # rename columns, remove some columns, remove sf class but retain lat, lon
  # get ecdf ests for all observed data
  out <- dat %>% 
    rename(
      SQI = StreamHealthIndex, 
      biological_condition = BiologicalCondition,
      stress_condition = OverallStressCondition_detail,
      pOverall = pChemHab, 
      County = cnty, 
      SMC_region = SMC_Name,
      Regional_board = RBNAME,
      Constraint_class = strcls, 
      CRAM = indexscore_cram
    ) %>% 
    dplyr::select(-CSCI_rc, -ASCI_rc, -Bio_BPJ, -bio_fp, -SiteSet, -WaterChemistryCondition, -HabitatCondition, -OverallStressCondition, -lower, -meds, -upper) %>% 
    dplyr::mutate(
      lon = st_coordinates(.)[, 1], 
      lat = st_coordinates(.)[, 2]
    ) %>% 
    st_set_geometry(NULL) 
  
  # ecdf ests
  ecdf_tojn <- out %>% 
    tidyr::gather('var', 'val', ASCI, blc, bs, Cond, CSCI, Ev_FlowHab, H_AqHab, H_SubNat, hy, CRAM, IPI, PCT_SAFN, ps, TN, TP, XCMG) %>% 
    dplyr::group_by(var) %>% 
    dplyr::select(MasterID, yr, var, val) %>% 
    dplyr::mutate(
      val = ecdf(val)(val),
      varptile = paste0(var, '_ptile')
      ) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(-var) %>% 
    tidyr::spread(varptile, val)
  
  # sanity check
  stopifnot(nrow(ecdf_tojn) == nrow(dat))
  
  # join ecdf with out
  out <- out %>%
    full_join(ecdf_tojn, by = c('MasterID', 'yr'))
  
  return(out)
  
}  