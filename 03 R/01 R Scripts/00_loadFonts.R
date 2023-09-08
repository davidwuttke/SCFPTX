library(showtext)
library(extrafont)
#font_import()
loadfonts(device = "win")

font_add("latex", "./cmr10.ttf")
showtext_auto()


##
# library(ggplot2)
# p <- ggplot(data.frame(x=1:5,y=1:5),aes(x,y))+
#   geom_point()+
#   geom_text(aes(label=y),nudge_x=0.5,family="latex")+
#   theme_bw(base_family="latex")
# p

