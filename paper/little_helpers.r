# Definitions for boroughs
borough.defs <- read_csv("data/inner_outer_london.csv")
clean.la.colnames <- function(df) {
  stripped.names <- gsub("\\.", " ", colnames(df))
}
shorten.la.names <- function(la.names) {
    short <- borough.defs[match(la.names, borough.defs$LA.NAME), "LA.SHORTNAME"]
    short[is.na(short)] <- as.character(la.names[is.na(short)])
    short
}
abbreviate.la.names <- function(la.names) {
    short <- borough.defs[match(la.names, borough.defs$LA.NAME), "LA.ABBR3"]
    short[is.na(short)] <- as.character(la.names[is.na(short)])
    short
}

library(Hmisc)
# Wrapper for Hmisc's latex
latex.glove <- function(tbl, ...) {
  latex(tbl, file="", numeric.dollar=FALSE, ...)
}

