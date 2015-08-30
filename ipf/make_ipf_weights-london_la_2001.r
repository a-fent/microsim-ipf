# Generate sets of weights by IPF for London boroughs in 2001
source("../r/ipf_functions.r")

YEAR <- 2001
# YEAR <- 2011

# Load frs.ad, frs.hh, hbai
source("frs_2001-load_recode.r")

# London cases
frs.hh.lond <- subset(frs.hh, GVTREGN == 8)
frs.ad.lond <- subset(frs.ad, SERNUM %in% frs.hh.lond$SERNUM)

source("constraints_2001.r")

constraints.l <- align.constraints(adult.constraint.tables, frs.ad.lond)
constraints.u <- align.constraints(hhold.constraint.tables, frs.hh.lond)

# Create subsets of the data with only the needed variables
surv.data.l <- subset(frs.ad.lond, select=c("SERNUM", ad.vars) )
hh.and.hrp.vars <- c("SERNUM", hh.vars, paste(ad.vars, "HRP", sep="."))
surv.data.u <- subset(frs.hh.lond, select=hh.and.hrp.vars)


# SINGLE-LEVEL HOUSEHOLD-ONLY CONSTRAINTS
system.time(new.wts.sl <- ipf.areas(constraints.u, surv.data.u, iterations=20))
# Using survey's rake - very similar results
# system.time(new.wts.rk <- iterative.rake.areas(constraints.u, surv.data.u))
write.csv(round(new.wts.sl, 2),
          "weights/london_la_2001-singlelevel.csv")

### MULTI-LEVEL CONSTRAINTS
library(parallel)
ptm <- proc.time()
new.wts.ml <- ipf.areas.2level.p(constraints.l, surv.data.l,
                                 constraints.u, surv.data.u,
                                 iterations=20, n.clusters=6)
proc.time() - ptm
write.csv(round(new.wts.ml, 2), "weights/london_la_2001-multilevel.csv")

### SPI SIMULATION
frs.ad.lond$person.id <- paste(frs.ad.lond$SERNUM, frs.ad.lond$PERSON)
# Whether a tax payer, based on annual personal allowance £4,385
inc.tax.threshold <- 4385
frs.ad.lond$is.tax.payer <- with(frs.ad.lond,
                                 ann.tax.income > inc.tax.threshold)
frs.ad.tax <- subset(frs.ad.lond, is.tax.payer)

# SPI data
spi <- read.csv("constraints/spi_london-2001_02.csv")
spi$mu <- log(spi$tot.median)
spi$sigma <- sqrt(2 * log(spi$tot.mean / spi$tot.median) )


la.probs <- apply(spi, 1,
                  function(r) dlnorm(frs.ad.tax$ann.tax.income,
                                     as.numeric(r["mu"]),
                                     as.numeric(r["sigma"])) )
colnames(la.probs) <- spi$Area
rownames(la.probs) <- frs.ad.tax$person.id

lond.mu <- log(18500) # London median from SPI
lond.sigma <- sqrt(2 * log(28800 / 18500)) # London mean from SPI
lond.probs <- dlnorm(frs.ad.tax$ann.tax.income, lond.mu, lond.sigma)

la.probs.rel <- apply(la.probs, 2, function(c) c / lond.probs)
# la.probs.rel <- apply(la.probs, 2, function(c) sqrt(c / lond.probs))
la.probs.rel <- apply(la.probs.rel, 2, function(c) c * length(c) / sum(c))

# sim.1 <- area.simulations(la.probs.rel, frs.ad.tax, "person.id")
# area.sim.means(sim.1,~ann.tax.income)
# apply(la.probs.rel,2,summary)

spi.seed.weights <- matrix(1, nrow=nrow(frs.ad.lond),
                           ncol=ncol(la.probs.rel),
                           dimnames=list(frs.ad.lond$person.id,
                                         colnames(la.probs.rel)))
spi.seed.weights[rownames(la.probs.rel),] <- la.probs.rel


ptm <- proc.time()
new.wts.sp <- ipf.areas.2level.p(constraints.l, surv.data.l,
                                 constraints.u, surv.data.u,
                                 n.clusters=6, iterations=20,
                                 seed.weights=spi.seed.weights)
proc.time() - ptm

write.csv(round(new.wts.sp,2),
          "weights/london_la_2001-multilev_with_stwts.csv")

