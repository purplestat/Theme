set_theme <- getOption("pick_theme", default = "dark")

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
    axis.text = element_text(color = "#403C53",size=14),               # axis text
    
    # title, subtitle, caption, legend:
    plot.title = element_text(hjust = 0,family =TITLE[5],face="bold",size =50,color=VCOL[5]),
    plot.subtitle = element_text(hjust = 0,family =TITLE[4],size = 32,color=VCOL[4]),
    plot.caption = element_text( color = "#403C53", hjust = 0,size=12),
    legend.position = "none")
  theme_set(VTHEME)
} else if (set_theme == "light") {
  cat("Light theme activated\n")
  # light theme code here
}



#FUNCTION TO ASSURE NO LABEL OVERLAP
LAB_VAL <- function(x, min_dist = 6) {
  x_sorted <- sort(x)
  adjusted <- x_sorted
  for (i in 2:length(x_sorted)) {
    if ((adjusted[i] - adjusted[i-1]) < min_dist) {
      adjusted[i] <- adjusted[i-1] + min_dist}}}



