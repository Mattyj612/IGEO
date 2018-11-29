IGEO_theme <- function(...){
  (ggthemes::theme_foundation(base_size = 14, base_family = "Helvetica") +
      theme(
        panel.background = element_rect(colour = NA),
        plot.background = element_rect(colour = NA),
        panel.border = element_blank(), 
        panel.grid.major = element_line(colour="#f0f0f0"),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black", size = 1.1),
        plot.title = element_text(hjust = 0.5, size = rel(1.1), face = "bold",
          family = "Helvetica",
          margin = margin(t = 0, r = 0, l = 0, b = 10)),
        axis.title = element_text(hjust = 0.5, size = rel(1), face = "bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 7, l = 20, b = 0)),
        legend.margin = margin(t= 0, r = 10, l = 0, b = 15),
        legend.direction = "vertical",
        #legend.title = element_blank(),
        legend.key = element_rect(color = NA)
      ))
}

IGEO_fill <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

IGEO_colors <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
}

clean_p_val <- function(col_name){
  case_when(
    col_name < 0.05 & col_name > 0.01 ~ "p < 0.05",
    col_name < 0.01 & col_name > 0.001 ~ "p < 0.01",
    col_name < 0.001 ~ "p < 0.001",
    TRUE ~ as.character(round(col_name, 3))
  )
}

clean_IGEO_model <- function(model, col_names){
  model %>% 
    tidy(conf.int = TRUE) %>% 
    filter(group == "fixed") %>% 
    mutate(
      p_value = round(p.value, 4),
      p_value_clean = clean_p_val(p.value),
      p_value_star = stars.pval(p.value)
    ) %>% 
    select(-p.value, -group) %>% 
    mutate(
      term = col_names
    ) %>% 
    rename(
      "conf_lower" = conf.low,
      "conf_upper" = conf.high,
      "std_error" = std.error,
      "log_odds" = estimate
    ) %>% 
    kable()
}

clean_IGEO_emmeans <- function(model){
  model %>% 
    tidy() %>% 
    select(-df) %>% 
    rename(
      "std_error" = std.error,
      "probability" = prob
      ) %>% 
    mutate(
      conf_lower = probability - 1.96*std_error,
      conf_upper = probability + 1.96*std_error
    ) %>% 
    select(-asymp.LCL, -asymp.UCL) %>% 
    arrange(surface, desc(planar), desc(spherical))
}

clean_IGEO_contrasts <- function(contrast_object){
  contrast_object %>% 
    tidy() %>% 
    mutate(
      p_value = round(p.value, 4),
      p_value_clean = clean_p_val(p.value),
      p_value_star = stars.pval(p.value)
    ) %>% 
    rename(
      "odds_ratio" = odds.ratio,
      "std_error" = std.error
    ) %>%
    select(surface, everything(), -p.value, -z.ratio, -df) %>% 
    kable()
}

by_case <- function(col_1, col_2, gl, gc, al, ac){
  case_when(
    col_1 == "line" & col_2 == "geo" ~ gl,
    col_1 == "curve" & col_2 == "geo" ~ gc,
    col_1 == "line" & col_2 == "arc" ~ al,
    col_1 == "curve" & col_2 == "arc" ~ ac
  )
}

