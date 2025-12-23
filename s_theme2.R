# CONTAINING: [a] THEMES, [b] LABLR (spread labels), [c] RSHPR (reshape long), [d] INDXR (index ts)

library(ggplot2) #Base
library(tidyr) #Function2
library(dplyr) #Function3
options(scipen = 999)  # increase penalty for scientific notation

#########
# 0a: Define Themes 
theme_set(theme_grey()) 

t_default <- theme(
  plot.title.position = "plot",
  plot.title = element_text(hjust = 0),
  panel.border = element_blank(),                                    
  plot.margin = margin(t = 0, r = 145, b = 0, l = 0),               
  panel.grid.major.x = element_blank(),                              # x grid
  panel.grid.minor.x = element_blank(),                              # x grid
  axis.line.x = element_blank(),                                     # x=0 line
  axis.line.y = element_blank(),                                     # y=0 line
  axis.ticks.length=unit(.25, "cm"),                                 # tick length
  axis.title.x = element_blank(),                                    # x title
  legend.title=element_blank(),                                      # legend title
  legend.key.size = unit(.5, 'cm')),                                 # legend key size
  legend.background = element_rect(fill = NA, color = NA)            # background color of the legend box
  )

t_basic<- theme(    
  plot.background = element_rect(fill = "#ffffff", color = NA),      
  panel.background = element_rect(fill = "#ffffff", color = NA),    
  panel.grid.major = element_line(color = "gray90"),    
  panel.grid.minor = element_line(color = "gray90"),                              
                 
  axis.title = element_text(color = "gray90", size = 22, face="bold"),
  axis.ticks = element_line(color = "gray90" , linewidth = 1),      
  axis.text = element_text(color = "gray60", size = 22),
                 
  plot.title = element_text(hjust = 0,color = "gray20", size = 40, face = "bold",),
  plot.subtitle = element_text(hjust = 0,size = 28,color = "gray50",margin = margin(b = 15)),
  plot.caption = element_text( color = "gray90", hjust = 0,size=15),
                 
  legend.text = element_text(size=24,color = "gray60"),
                 
  legend.position = "top",                                           
  legend.direction = "horizontal")

#fb_dark
t_fbdark <- theme(  
  plot.background = element_rect(fill = "#232136", color = NA),      # overall plot bg  
  panel.background = element_rect(fill = "#232136", color = NA),     # panel bg
  panel.grid.major = element_line(color = "#403C53",linetype = "dotted"),     # major grid lines
  panel.grid.minor = element_line(color = "#403C53",linetype = "dotted"),     # minor grid lines

  axis.title = element_blank(),                                      # axis title
  axis.ticks = element_line(color = "#403C53", linewidth = 0),       # axis ticks
  axis.text = element_text(color = "#403C53",size=15),               # axis text

  plot.title = element_text(hjust = 0,family ="Bahnschrift",face="bold",size =50,color="#8400F0"),
  plot.subtitle = element_text(hjust = 0,family ="Sergoe UI",size = 32,color="#382E6B",margin = margin(b = 15)),
  plot.caption = element_text( color = "#403C53", hjust = 0,size=13),
  legend.position = "none")

#fb_light
t_fblight <- theme(  
  plot.background = element_rect(fill = "#CCE2C6", color = NA),      # overall plot bg  
  panel.background = element_rect(fill = "#CCE2C6", color = NA),     # panel bg
  panel.grid.major = element_line(color = "#403C53",linetype = "dotted"),     # major grid lines
  panel.grid.minor = element_line(color = "#403C53",linetype = "dotted"),     # minor grid lines
  
  axis.title = element_blank(),                                      # axis title
  axis.ticks = element_line(color = "#403C53", linewidth = 0),       # axis ticks
  axis.text = element_text(color = "#403C53",size=15),               # axis text
  
  plot.title = element_text(hjust = 0,family ="Bahnschrift",face="bold",size =50,color="#8400F0"),
  plot.subtitle = element_text(hjust = 0,family ="Segoe UI",size = 32,color="#382E6B",margin = margin(b = 15)),
  plot.caption = element_text( color = "#403C53", hjust = 0,size=13),
  legend.position = "none")

#clean
t_o_clean <- theme(
  plot.margin = margin(t = 15, r = 15, b = 15, l = 15),               
  panel.grid.major = element_blank(),   
  panel.grid.minor = element_blank(),  
  panel.grid.major.x = element_blank(),      
  panel.grid.minor.x = element_blank(),
  axis.line.x = element_blank(),                                   
  axis.line.y = element_blank(),                                    
  axis.title = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks = element_blank(),     
  axis.ticks.length=unit(.0, "cm"),
  axis.text = element_blank(),
  legend.position = "none")

#########
# 0b: Apply 
themes_list <- list(
  "basic" = t_basic,
  "fblight" = t_fblight,
  "fbdark"  = t_fbdark
)

go_theme <- function() {
  option_a <- getOption("pick_theme", "basic")  # default to "light"
  option_b <- getOption("clean", FALSE)
  
  th <- t_default + themes_list[[option_a]]
  
  # Apply optional update
  if (option_b) th <- th + t_o_clean
  
  return(th)
}
theme_set(go_theme())

#########
# FUNCTION 1: LABEL SPREAD

#FUNCTION TO ASSURE NO LABEL OVERLAP
LABLR <- function(x, min_dist = 5) {
  x_sorted <- sort(x)
  adjusted <- x_sorted
  for (i in 2:length(x_sorted)) {
    if ((adjusted[i] - adjusted[i-1]) < min_dist) {
      adjusted[i] <- adjusted[i-1] + min_dist}}
  return(adjusted)
  }
#


#########
# FUNCTION 2: RESHAPER WIDE (N x N :: VAR + YEAR_x) -> LONG (N x 3 :: TIME + VAR + VAL)

  #SAMPLEDATA
  # df_wide <- data.frame(
  #   VAR = c("A", "B", "C"),
  #   `2020` = sample(50:100, 3, replace = TRUE),
  #   `2021` = sample(50:100, 3, replace = TRUE),
  #   `2022` = sample(50:100, 3, replace = TRUE),
  #   check.names = FALSE)


RSHPR <- function(df, id_col="VAR", names_to = "TIME", values_to = "VAL") {
    df  %>%
    pivot_longer(
      cols = -all_of(id_col),  # keep the id column fixed
      names_to = names_to,
      values_to = values_to
    )%>% mutate(across(c(TIME, VAL), as.numeric))
}
  #df_input <- RSHPR(df_wide)


#########
# FUNCTION 3: INDXR (N x 3 :: TIME + VAR + VAL)


INDXR <- function(df, time_col = "TIME", var_col = "VAR", val_col = "VAL") {
  df %>%
    group_by(across(all_of(var_col))) %>%
    arrange(across(all_of(time_col))) %>%
    mutate(
      !!sym(val_col) := (!!sym(val_col) / first(!!sym(val_col))) * 100
    ) %>%
    ungroup()
}
  #df_input2 <- INDXR(df_input)
