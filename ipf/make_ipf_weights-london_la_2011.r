# Generate sets of weights by IPF for London boroughs in 2011
source("../r/ipf_functions.r")

# YEAR <- 2001
YEAR <- 2011


#* frs.hh (FRS hholds dataset, only cases in HBAI, excluding those with non-resident partners)
#* frs.ad (FRS adults dataset, ditto)
#* hbai (hbai 11/12)
source("frs_2011-load_recode.r")

frs.hh.lond <- subset(frs.hh, REGION == "LO")
frs.ad.lond <- subset(frs.ad, SERNUM %in% frs.hh.lond$SERNUM)

source("constraints_2011.r")

constraints.l <- align.constraints(adult.constraint.tables, frs.ad.lond)
constraints.u <- align.constraints(hhold.constraint.tables, frs.hh.lond)

surv.data.l <- subset(frs.ad.lond, select=c("SERNUM", ad.vars) )
surv.data.u <- subset(frs.hh.lond, select=c("SERNUM", hh.vars,
                                       paste(ad.vars, "HRP", sep=".")) )

## SINGLE LEVEL SIMULATION
system.time(new.wts.sl <- ipf.areas(constraints.u,
                                    surv.data.u,
                                    iterations=20))
write.csv(round(new.wts.sl,2),
          "weights/london_la_2011-singlelevel.csv")

ptm <- proc.time()

## MULTI LEVEL SIMULATION
## Parallel version
library(parallel)

new.wts.ml <- ipf.areas.2level.p(constraints.l, surv.data.l,
                                 constraints.u, surv.data.u,
                                 n.clusters=6, iterations=20)
proc.time() - ptm

write.csv(round(new.wts.ml,2),
          "weights/london_la_2011-multilevel.csv")

### SIMULATION WITH SPI TAX INCOME data
frs.ad.lond$person.id <- paste(frs.ad.lond$SERNUM,
                               frs.ad.lond$PERSON)
# Whether a tax payer, based on annual personal allowance £7,475
inc.tax.threshold <- 7475
frs.ad.lond$is.tax.payer <- with(frs.ad.lond,
                                 ann.tax.income > inc.tax.threshold)
frs.ad.tax <- subset(frs.ad.lond, is.tax.payer)
spi <- read.csv("constraints/spi_london-2011_12.csv")
spi$mu <- log(spi$tot.median)
spi$sigma <- sqrt(2 * log(spi$tot.mean / spi$tot.median) )

la.probs <- apply(spi, 1,
                  function(r) dlnorm(frs.ad.tax$ann.tax.income,
                                     as.numeric(r["mu"]),
                                     as.numeric(r["sigma"])) )
colnames(la.probs) <- spi$Area
rownames(la.probs) <- frs.ad.tax$person.id


# The all-london distribution
lond.mu <- log(23800) # median
lond.sigma <- sqrt(2 * log(39100 / 23800)) # mean / median
lond.probs <- dlnorm(frs.ad.tax$ann.tax.income, lond.mu, lond.sigma)

# Relative probalities to London average
la.probs.rel <- apply(la.probs, 2, function(c) c / lond.probs)
## # Judgement adjustment to very high probabilities
## la.probs.rel <- ifelse(la.probs.rel > 10,
##                         sqrt(10 * la.probs.rel), la.probs.rel)
la.probs.rel <- apply(la.probs.rel, 2, function(c) c * length(c) / sum(c))

spi.seed.weights <- matrix(1,
                           nrow=nrow(frs.ad.lond),
                           ncol=ncol(la.probs.rel),
                           dimnames=list(frs.ad.lond$person.id,
                           colnames(la.probs.rel)))
spi.seed.weights[rownames(la.probs.rel),] <- la.probs.rel

# Get rid of London column
spi.seed.weights <- spi.seed.weights[,-1]
# colnames(spi.seed.weights) %in% rownames(constraints.l[[1]])

ptm <- proc.time()
new.wts.sp <- ipf.areas.2level.p(constraints.l, surv.data.l,
                                 constraints.u, surv.data.u,
                                 n.clusters=6,
                                 seed.weights=spi.seed.weights,
                                 iterations=20)
proc.time() - ptm

## new.wts.sp.2 <- cbind(new.wts.sp, London=rowSums(new.wts.sp))
## foo <- area.simulations(new.wts.sp.2, subset(hbai, GVTREGN==8), "SERNUM")
## area.sim.quantiles(foo, ~S_OE_AHC)
## area.sim.means(foo, ~S_OE_AHC)

write.csv(round(new.wts.sp,2),
          "weights/london_la_2011-multilev_with_stwts.csv")




