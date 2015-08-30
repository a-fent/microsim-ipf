library(Cairo)
# Use Cairo's PDF to output graphics
# pdf <- CairoPDF


# A lost cause, for the time being - OTFs on Windows
## if ( Sys.info()['sysname'] == "Windows" ) {
##   windowsFonts(Secca="Secca")
##   par(family="Secca")
## }
if ( Sys.info()['sysname'] == "Linux" ) {
  CairoFonts(regular="Secca:style=Regular",
             italic="Secca:style=Italic",
             bold="Secca:style=Bold")
}


rgb255 <- function(r,g,b) rgb(r/255, g/255, b/255)

dark.red <- rgb255(215,48,39)
orange <- rgb255(252,141,89)
pale.orange <- rgb255(254,224,144)
yellow <- rgb255(255,255,191)
pale.blue <- rgb255(224,243,248)
blue <- rgb255(145,191,219)
dark.blue <- rgb255(69,117,180)

# red.1 <- rgb255(178,24,43)
red.3 <- rgb255(99,28,0)
red.2 <- rgb255(239,138,98)
red.1 <- rgb255(253,219,199)
white <- rgb255(255,255,255)
grey.3 <- rgb255(77,77,77)
grey.2 <- rgb255(153,153,153)
grey.1 <- rgb255(224,224,224)
black <- rgb(0,0,0)

the_theme <- theme_bw() +
    theme(legend.position="bottom",
          text=element_text(size=8),
          plot.title=element_text(lineheight=0.5, face="bold"),
          plot.margin=unit(c(0.5,1,0,0.5), "lines") ) #,
#          panel.margin=unit(c(0,0,0,0), "lines") )
theme_set(the_theme)

the_map_theme <- the_theme + theme(axis.text=element_blank(),
                                   axis.title=element_blank(),
                                   axis.ticks.length=unit(0, "lines"),
                                   line=element_blank() )

the_map_theme_legend_bottom <- the_map_theme +
    theme(legend.key.width=unit(1.5, "lines"),
          legend.key.height=unit(1, "lines"),
          legend.position="bottom")

the_map_theme_legend_right <- the_map_theme +
    theme(legend.key.width=unit(1, "lines"),
          legend.key.height=unit(1.5, "lines"),
          legend.position="right")


colpal.paired <- c(red.3, red.2, grey.3)
colpal.ascending <- c(grey.1, grey.2, red.1, red.2, red.3)
colpal.qual <- c(red.2, blue, grey.2)
colpal.diverging <- c(grey.2, grey.1, red.1, red.2, red.3)
colpal.diverging.darker <- c(grey.2, red.1, red.2, red.3)

# For powerpoint
pp_png <- function(...) CairoPNG(width=28, height=21, units="cm", dpi=150, ...)
pp_svg <- function(...) Cairo(width=280, height=210, units="mm", type="svg", ...)

