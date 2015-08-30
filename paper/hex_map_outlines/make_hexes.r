library(rgdal)
library(rgeos)
# library(devtools)
# install_github("mbedward/packcircles")
library(packcircles)
library(readr)
library(ggplot2)


### LOAD SOME STUFF
boroughs <- read_csv("data/inner_outer_london.csv")
umbr <- read_csv("../umbr2/umbr14-esw.csv")
# LSOA to LA
lkup <- read_csv("/home/alex/borders/lookup/LSOA11_CCG13_LAD13_EN_LU.csv")
# Don't need the CCG information
lkup <- lkup[,-grep("CCG", colnames(lkup))]
umbr <- merge(umbr, lkup, by.x="Geogcode", by.y="LSOA11CD")
# Subset - only London boroughs
umbr.l <- subset(umbr, LAD13NM %in% boroughs$LA.NAME)
umbr.l$LAD13NM <- as.character(umbr.l$LAD13NM)
umbr.l <- merge(umbr.l, boroughs, by.x="LAD13NM", by.y="LA.NAME")




### LET'S MAKE BUBBLES
lsoa.sppt <- readOGR("/home/alex/Documents/DATA/BORDERS/EW_LSOA11_centroids", "LSOA_2011_EW_PWC")
lsoa.pt.ln <- lsoa.sppt[lsoa.sppt$LSOA11CD %in% umbr.l$Geogcode,]

install.packages("packcircles")

head(umbr.l)

lsoa.circ <- data.frame(x=coordinates(lsoa.pt.ln)[,1],
                        y=coordinates(lsoa.pt.ln)[,2],
                        r=250)
xlims <- c(min(lsoa.circ$x)-1000, max(lsoa.circ$x)+1000)
ylims <- c(min(lsoa.circ$y)-1000, max(lsoa.circ$y)+1000)

coded.circ <- cbind(lsoa.circ, umbr.l[match(lsoa.pt.ln@data$LSOA11CD, umbr.l$Geogcode),c("LA.ABBR3", "NUTS312NM")])

# More is better
# lsoa.circ <- circleLayout(foo$layout, xlims, ylims, maxiter=200)
coded <- cbind(foo$layout, umbr.l[match(lsoa.pt.ln@data$LSOA11CD, umbr.l$Geogcode),
                                  c("LA.ABBR3", "NUTS312NM")])

ggplot(coded) +
    geom_point(aes(x=x, y=y, size=r, colour=NUTS312NM), alpha=0.3) +
        coord_equal(xlim=xlims, ylim=ylims) +
    # coord_equal(xlim=c(520000,540000), ylim=c(170000,190000)) +
            theme_bw()

write.csv(coded,"paper/displaced_lsoas.csv")


### LET'S PUT THE BUBBLES IN HEXAGONS
coded <- read.csv("coded_lsoas.csv")
hex.pt <- spsample(gBuffer(gConvexHull(lsoa.pt.ln), width=1000),
                   n=240, type="hexagonal",  offset=c(0,0))
hex.poly <- HexPoints2SpatialPolygons(hex.pt)

bubble.points <- SpatialPoints(cbind(coded$x, coded$y),
                               proj4string=lsoa.pt.ln@proj4string)

lsoa.hex <- data.frame(lsoa=lsoa.pt.ln$LSOA11CD,
                       hex=over(bubble.points, hex.poly))
lsoa.hex$hex.id <- sprintf("ID%s", lsoa.hex$hex)

write.csv(lsoa.hex, "paper/hex_lsoa_memberships.csv")

lsoa.hex <- read.csv("paper/hex_lsoa_memberships.csv")
lsoa.01.to.11 <- read.csv("../umbr2/data/LSOA01_LSOA11_LAD11_EW_LU.csv")

hex.01 <- merge(lsoa.hex, lsoa.01.to.11,
                by.x="lsoa", by.y="LSOA11CD")
# There are around 45 duplicates -
# split LSOAs belonging to more than one
# table(table(hex.01$LSOA01CD, hex.01$hex.id))
hex.01 <- subset(hex.01, ! duplicated(LSOA01CD),
                 c("LSOA01CD", "hex", "hex.id"))
write.csv(hex.01, "paper/hex_lsoa01_memberships.csv")


# Code Hexes
library(data.table)
umbr.hex <- data.table(merge(umbr.l, lsoa.hex, by.x="Geogcode", by.y="lsoa"))
hex.rates <- umbr.hex[,list("All.MTB"=sum(All.MTB, na.rm=TRUE),
                            "Hholds"=sum(Hholds, na.rm=TRUE),
                            "Inner"=sum(InOut == "Inner") >= sum(InOut == "Outer"),
                            "LA"=names(which.max(table(LAD13NM))),
                            "NUTS2"=names(which.max(table(NUTS212NM))),
                            "NUTS3"=names(which.max(table(NUTS312NM)))),
                      by=c("Year", "hex.id")]
hex.rates[,UMBR:=All.MTB/Hholds]

# Outlines of Base Hexes
hex.gg <- fortify(hex.poly)
write.csv(hex.gg, "paper/hex_outlines_hexes.csv")

# Outlines of NUTS3 Areas
nuts3.hexes <- lapply(unique(hex.rates$NUTS3), function(n3) {
    nut3.hx <- hex.poly[unlist(hex.rates[Year == 2001 &
                                         NUTS3 == n3 &
                                         hex.id != "IDNA", "hex.id", with=FALSE]),]
    nut3.hx <- gUnionCascaded(gBuffer(gBuffer(nut3.hx, width=100),width=-100))
    cbind(fortify(nut3.hx), n3)
})
nuts3.hexes <- do.call(rbind, nuts3.hexes)
write.csv(nuts3.hexes,"paper/hex_outlines_nuts3.csv")
# Outlines of LA areas
la.hexes <- lapply(unique(hex.rates$LA), function(la) {
    la.hx <- hex.poly[unlist(hex.rates[Year == 2001 &
                                       LA == la &
                                       hex.id != "IDNA", "hex.id", with=FALSE]),]
    la.hx <- gUnionCascaded(gBuffer(gBuffer(la.hx, width=100),width=-100))
    cbind(fortify(la.hx), la)
})
la.hexes <- do.call(rbind, la.hexes)
write.csv(la.hexes, "paper/hex_outlines_la.csv")


hex.toplot <- merge(hex.gg, hex.rates, by.x="id", by.y="hex.id")
  # Must be in correct order to plot correctly
hex.toplot <- hex.toplot[order(hex.toplot$Year, hex.toplot$id, hex.toplot$order),]
library(scales)



ggplot(data=subset(hex.toplot), aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill=UMBR), alpha=1) +
    scale_fill_gradientn("UMBR", colours=c("#EEEEEE", "#CCCCCC", "#AAAAAA", "#333333", "black"),
                         guide="colorbar", label=percent) +
    #      geom_polygon(data=inner, fill="transparent", colour="white", linetype=1, size=0.75) +
    facet_wrap(~Year) +
    coord_equal()

hex.diffs <- merge(subset(hex.rates, Year==2001), subset(hex.rates, Year==2013),
                   by="hex.id", suffixes=c(".01", ".13"))

hex.d.toplot <- merge(hex.gg, hex.diffs, by.x="id", by.y="hex.id")
hex.d.toplot <- hex.d.toplot[order(hex.d.toplot$id, hex.d.toplot$order),]

chg.min <- min(hex.d.toplot$UMBR.13 - hex.d.toplot$UMBR.01)
chg.max <- max(hex.d.toplot$UMBR.13 - hex.d.toplot$UMBR.01)


ggplot(data=hex.d.toplot, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill=UMBR.13-UMBR.01), alpha=1) +
    scale_fill_gradientn("UMBR", colours=c("green", "grey", "red"),
                         values=rescale(c(chg.min, 0, chg.max)) ) +
    #      geom_polygon(data=inner, fill="transparent", colour="white", linetype=1, size=0.75) +
    geom_polygon(data=la.hexes, aes(group=la), fill="transparent", colour="#CCCCCC", size=0.2) +
    geom_polygon(data=nuts3.hexes, aes(group=n3), fill="transparent", colour="white", size=1) +
    coord_equal() +
    theme_bw()

ggplot(data=hex.d.toplot, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill=LA.13), alpha=1) +
#    scale_fill_gradientn("UMBR", colours=c("green", "grey", "red") ) +
    geom_polygon(data=nuts3.hexes, aes(group=n3), fill="transparent", colour="white", size=1) +
    coord_equal()

chg.min <- min(hex.d.toplot$Hholds.13 - hex.d.toplot$Hholds.01)
chg.max <- max(hex.d.toplot$Hholds.13 - hex.d.toplot$Hholds.01)


ggplot(data=hex.d.toplot, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill=Hholds.13-Hholds.01), alpha=1) +
        scale_fill_gradientn("Change in Households", colours=c("black", "grey", "pink", "red"),
                             values=rescale(c(chg.min, 0, 3000, chg.max)) ) +
    #      geom_polygon(data=inner, fill="transparent", colour="white", linetype=1, size=0.75) +
    geom_polygon(data=nuts3.hexes, aes(group=n3), fill="transparent", colour="white", size=1) +
    coord_equal() +
    theme_bw()

dw.01 <- read.delim("data/lnd-ctax_band-lsoa-2001.tab", skip=5)
dw.01 <- dw.01[,c(1:10, 13, 14, 16, 18, 20, 22, 24, 26, 28, 30)]
colnames(dw.01)[11:20] = c("All", "A", "B", "C", "D", "E", "F", "G", "H", "X")
dw.01$value.tot <- apply(dw.01[,LETTERS[1:8]], 1, function(x) sum(x * 1:8) )
dw.01$mean.band <- apply(dw.01[,LETTERS[1:8]], 1, function(x) sum(x * 1:8) / sum(x) )

dw.11 <- read.delim("data/lnd-ctax_band-lsoa-2011.tab", skip=5)
dw.11 = dw.11[,c(11, 15, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34)]
colnames(dw.11)[2:12] = c("All", "A", "B", "C", "D", "E", "F", "G", "H", "I", "X")
dw.11$value.tot <- apply(dw.11[,LETTERS[1:8]], 1, function(x) sum(x * 1:8) )
dw.11$mean.band <- apply(dw.11[,LETTERS[1:8]], 1, function(x) sum(x * 1:8) / sum(x) )

dw <- merge(dw.01, dw.11, by="LSOA_CODE",
            all.y=FALSE, suffixes=c(".01", ".11"))

dw.hex <- merge(dw, hex.01, by.x="LSOA_CODE", by.y="LSOA01CD")
hex.tots <- aggregate(cbind(All.01, All.11,
                            value.tot.01, value.tot.11)~hex.id, dw.hex, sum)

hex.tots$mean.value.01 <- with(hex.tots, value.tot.01/All.01)
hex.tots$mean.value.11 <- with(hex.tots, value.tot.11/All.11)
hex.tots$value.change <- with(hex.tots, mean.value.11 - mean.value.01)
hex.tots$value.change.capped <- ifelse(hex.tots$value.change < 1,
                                       hex.tots$value.change, 1)

base.hexes <- read.csv("paper/hex_outlines_hexes.csv")

hx.to.plot <- merge(hex.tots, hexes, by.x="hex.id", by.y="id")
hx.to.plot <- hx.to.plot[order(hx.to.plot$hex.id, hx.to.plot$order),]

ggplot(data=hx.to.plot, aes(x=long, y=lat, group=group)) +
    geom_polygon(aes(fill=value.change), alpha=1) +
        scale_fill_gradientn("UMBR",
                             colours=c("#EEEEEE",
                                 "#CCCCCC",
                                 "#AAAAAA", "#333333", "black") ) +
coord_equal() +
    theme_bw()
