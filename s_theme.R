set_theme <- getOption("pick_theme", default = "light")

if (set_theme == "dark") {
  cat("Dark theme activated\n")
  VTHEME <- theme(
    panel.border = element_blank(),                                    # border remove
    plot.margin = margin(t = 0, r = 145, b = 0, l = 0),                # margin for labels
    
    plot.background = element_rect(fill = "#232136", color = NA),      # overall plot bg  
    panel.background = element_rect(fill = "#232136", color = NA),     # panel bg
    panel.grid.major = element_line(color = "#403C53",linetype = "dotted"),     # major grid lines
    panel.grid.minor = element_line(color = "#403C53",linetype = "dotted"),     # minor grid lines
    panel.grid.major.x = element_blank(),                              # x grid
    panel.grid.minor.x = element_blank(),                              # x grid
    axis.line.x = element_blank(),                                     # x=0 line
    axis.line.y = element_blank(),                                     # y=0 line
    
    axis.title = element_blank(),                                      # axis title
    axis.ticks = element_line(color = "#403C53", linewidth = 0),       # axis ticks
    axis.text = element_text(color = "#403C53",size=15),               # axis text
    
    # title, subtitle, caption, legend:
    plot.title = element_text(hjust = 0,family =TITLE[5],face="bold",size =50,color=VCOL[5]),
    plot.subtitle = element_text(hjust = 0,family =TITLE[4],size = 32,color=VCOL[4],margin = margin(b = 15)),
    plot.caption = element_text( color = "#403C53", hjust = 0,size=13),
    legend.position = "none")
} else if (set_theme == "light") {
  cat("Light theme activated\n")
  # light theme code here
  VTHEME <- theme(
    panel.border = element_blank(),                                    
    plot.margin = margin(t = 0, r = 145, b = 0, l = 0),               
    
    plot.background = element_rect(fill = "#ffffff", color = NA),      
    panel.background = element_rect(fill = "#ffffff", color = NA),    
    panel.grid.major = element_line(color = "gray90"),    
    panel.grid.minor = element_line(color = "gray90"),   
    panel.grid.major.x = element_blank(),      
    panel.grid.minor.x = element_blank(),
    axis.line.x = element_blank(),                                   
    axis.line.y = element_blank(),                                    
    
    axis.title = element_text(color = "gray90", size = 22, face="bold"),
    axis.title.x = element_blank(),
    axis.ticks = element_line(color = "gray90" , linewidth = 1),      
    axis.ticks.length=unit(.25, "cm"),
    axis.text = element_text(color = "gray60", size = 22),
    
    plot.title = element_text(hjust = 0,color = "gray20", size = 50, face = "bold",),
    plot.subtitle = element_text(hjust = 0,size = 32,color = "gray50",margin = margin(b = 15)),
    plot.caption = element_text( color = "gray90", hjust = 0,size=15),
    legend.title=element_blank(),
    legend.position = "top", 
    legend.text = element_text(size=24,color = "gray60"),
    legend.key.size = unit(.5, 'cm'),
    legend.direction = "horizontal"
  )
}else if (set_theme == "clean") {
  cat("Clean theme activated\n")
  # light theme code here
  VTHEME <- theme(
    panel.border = element_blank(),                                    
    plot.margin = margin(t = 15, r = 15, b = 15, l = 15),               
    
    plot.background = element_blank(),     
    panel.background = element_blank(),   
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
    
    plot.title = element_text(hjust = 0,color = "gray20", size = 50, face = "bold"),
    plot.subtitle = element_text(hjust = 0,size = 32,color = "gray50",margin = margin(b = 15)),
    plot.caption = element_text( color = "gray90", hjust = 0,size=15),
    legend.position = "none"
  )
}
theme_set(VTHEME)

#########
# FUNCTION 1: LABEL SPREAD

#FUNCTION TO ASSURE NO LABEL OVERLAP
LAB_VAL <- function(x, min_dist = 6) {
  x_sorted <- sort(x)
  adjusted <- x_sorted
  for (i in 2:length(x_sorted)) {
    if ((adjusted[i] - adjusted[i-1]) < min_dist) {
      adjusted[i] <- adjusted[i-1] + min_dist}}
  return(rev(adjusted)) 
  }
#


#########
# FUNCTION 2: RESHAPER WIDE -> LONG (N x N :: VAR + YEAR_x)

# df_wide <- data.frame(
#   VAR = c("A", "B", "C"),
#   `2020` = sample(50:100, 3, replace = TRUE),
#   `2021` = sample(50:100, 3, replace = TRUE),
#   `2022` = sample(50:100, 3, replace = TRUE),
#   check.names = FALSE 
# )
library(tidyr)
RSHPR <- function(df, id_col="VAR", names_to = "TIME", values_to = "VAL") {
  df_long <- df %>%
    pivot_longer(
      cols = -all_of(id_col),  # keep the id column fixed
      names_to = names_to,
      values_to = values_to
    )
  return(df_long)
}
# df_input <- RSHPR(df_wide)
