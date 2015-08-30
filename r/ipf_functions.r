# Weighted counts, quantiles &c
library(Hmisc)
library(plyr)
library(survey)

# Read and load a list of constraining tables for a set of regions /
# areas The constraints should be a single list, with the names being
# the name of the constraining variable in the survey data to be
# reweighted, and the values being the path to the tables
read.constraints <- function(cnst.spec) {
    cnst.tables <- list()
    for ( cnst.variable in names(cnst.spec) ) {
        cnst.tables[[cnst.variable]] <- read.csv(cnst.spec[[cnst.variable]],
                                                 row.names = 1)
    }
    cnst.tables
}

# Fetch the set of marginal constraints for a particular region from the
# constraint table object. The +area+ should be a name within the
# row.names of the constraint tables
margins.for.area <- function(cnst.tables, area) {
    margins <- list()
    for ( cnst.var in names(cnst.tables) ) {
        margins[[cnst.var]] <- unlist(cnst.tables[[cnst.var]][area,])
    }
    margins
}

# Checks constraints to make sure they all contain the same list of
# areas. Make a copy of +constraints+, where the setting of levels in
# the constraint tables are aligned to the levels of factors in
# +surv.data+
align.constraints <- function(constraints, surv.data) {
    # PRE-CHECKING: Check that all constraint tables contain the same
    # list of areas in the row names
    row.check <- lapply(constraints,
                        function(tbl) { sum(row.names(tbl) !=
                                            row.names(constraints[[1]])) } )
    if ( any(row.check > 0) ) {
        stop(paste("The row names for areas in these constraints do not match: ",
                   paste(names(row.check[row.check > 0]), sep=", ")))

    }
    new.constraints <- list()
    for ( c in names(constraints) ) {
        cstr <- constraints[[c]]
        # Re-order columns of constraint table to match factor levels
        new.constraints[[c]] <-
            cstr[,match(levels(surv.data[,c]), colnames(cstr))]
    }
    # POST-CHECKING: make sure that the names of the constraint variables
    # match the levels of the variables in the survey
    for ( c in names(new.constraints) ) {
        if ( ! all(names(new.constraints[[c]]) == levels(surv.data[,c]) ) ) {
            stop(sprintf("The variable [%s]s levels in survey data doesn't match the constraint columns",
                         c) )
        }
    }
    return(new.constraints)
}


# Reweight for a set of areas. Returns a dataframe of new weights, one
# row for each case in the survey, one column for each area in the
# constraints
ipf.areas <- function(constraints.raw, surv.data,
                                     seed.weights = NA, iterations=10) {
    # Check and match up factor levels in survey data and constraints
    constraints <- align.constraints(constraints.raw, surv.data)
    # Seed weights = 1 if nothing else specified
    if ( is.na(seed.weights) ) {
        seed.weights <- data.frame(matrix(1, nrow=nrow(surv.data), ncol=nrow(constraints[[1]])),
                                   row.names=surv.data$id)
        colnames(seed.weights) <- row.names(constraints[[1]])
        rownames(seed.weights) <- surv.data$SERNUM
    }
    # Output data frame
    new.weights <- seed.weights
    # Loop through areas
    for ( area in row.names(constraints[[1]]) ) {
        # Only include those with > 0 starting weight in this area
        area.surv.data <- surv.data[seed.weights[,area] > 0,]
        # FIXME - only include those with > 0 starting weight
        area.weights <- seed.weights[,area]
        # Get the margins for the current area of interest
        margins <- margins.for.area(constraints, area)
        for ( i in 1:iterations ) {
            for ( c in names(margins) ) {
                margin <- margins[[c]]
                area.weights <- reweight.once(area.surv.data[,c], area.weights, margin)
                if( any(is.na(area.weights) ) ) {
                    stop(sprintf("getting weird in area %s on variable %s, iteration %i", area, c, i))
                }
            }
        }
        # FIXME - lookup
        new.weights[,area] <- area.weights
    }
    new.weights
}

iterative.rake.areas <- function(constraints, surv.data,
                                 seed.weights=NULL, iterations=10) {
    if ( is.null(seed.weights) ) {
        seed.weights <- rep(1, nrow(surv.data))
        names(seed.weights) <- surv.data$SERNUM
    }
    # To hold the results
    out.weights <- data.frame(row.names=surv.data$SERNUM)
    # Formula for the constraints
    surv.margins <- lapply(names(constraints),
                           function(n) as.formula(sprintf("~%s", n)))
    for ( area in row.names(constraints[[1]]) ) {
        initial <- svydesign(data=surv.data, ids=~1, weights=seed.weights)
        area.margins <- lapply(names(constraints), function(con) {
            area.tots <- constraints[[con]][area,]
            df <- data.frame(Freq=unlist(area.tots))
            df[,con] <- names(area.tots)
            df
        })
        final <- rake(initial, surv.margins, area.margins)
        out.weights[,area] <- weights(final)
    }
    out.weights
}


# 2-level reweighting
# Assumes SERNUM is linking variable
ipf.areas.2level <- function(constraints.l, surv.data.l,
                                            constraints.u, surv.data.u,
                                            iterations=10) {
    constraints.l <- align.constraints(constraints.l, surv.data.l)
    constraints.u <- align.constraints(constraints.u, surv.data.u)

    new.wts <- data.frame(row.names=surv.data.u$SERNUM)
    # multiple-level reweighting
    for ( area in row.names(constraints.l[[1]]) ) {

        new.wts[,area] <- wts.u
    }
    new.wts
}

# 2-level reweighting, parallel version
# TODO - separate internal content (area.iter) into a separate function
ipf.areas.2level.p <- function(constraints.l, surv.data.l,
                                              constraints.u, surv.data.u,
                                              seed.weights = NULL,
                                              iterations = 10, n.clusters=4) {
    constraints.l <- align.constraints(constraints.l, surv.data.l)
    constraints.u <- align.constraints(constraints.u, surv.data.u)

    if ( is.null(seed.weights) ) {
        seed.weights <- matrix(1, nrow=nrow(surv.data.l), ncol=nrow(constraints.l[[1]]))
        seed.weights <- data.frame(seed.weights)
        colnames(seed.weights) <- row.names(constraints.l[[1]])
    }

    # multiple-level reweighting
    area.iter <- function(area) {
        # Starting weights
        wts.l <- seed.weights[,area]
        # Margins at both levels
        margins.l <- margins.for.area(constraints.l, area)
        margins.u <- margins.for.area(constraints.u, area)
        for ( i in 1:iterations ) {
            # Constraint to adult margins
            for ( c in names(margins.l) ) {
                wts.l <- reweight.once(surv.data.l[,c], wts.l, margins.l[[c]])
            }
            mean.wts <- aggregate(wts.l, list(surv.data.l$SERNUM), mean)
            # Average household weights from their adult members
            wts.u <- mean.wts[ match(surv.data.u$SERNUM, mean.wts$Group.1), "x"]
            # Constrain to household margins
            for ( c in names(margins.u) ) {
                wts.u <- reweight.once(surv.data.u[,c], wts.u, margins.u[[c]])
            }
            # Spread household weights across adults
            wts.l <- wts.u[ match(surv.data.l$SERNUM, surv.data.u$SERNUM) ]
        }
        wts.u
    }

    # This, or type="FORK" parLapply is faster - but leaks memory
    # new.wts <- mclapply(row.names(constraints.l[[1]]), area.iter, mc.cores=6)
    cl <- makeCluster(n.clusters)
    clusterExport(cl, varlist=c("margins.for.area", "reweight.once") )
    new.wts <- parLapply(cl, row.names(constraints.l[[1]]), area.iter)
    stopCluster(cl)

    # Tidy up and convert to a data frame
    new.wts <- t(as.data.frame(do.call(rbind, new.wts)))
    colnames(new.wts) <- row.names(constraints.l[[1]])
    rownames(new.wts) <- surv.data.u$SERNUM
    new.wts
}

reweight.once <- function(case.variables, prior.weights, constraint) {
    # Calculate counts at start weightings
    curr.tab <- Hmisc::wtd.table(case.variables, prior.weights, "table")
    # How much each class within variable needs to be adjusted
    adj.scale <- constraint / curr.tab[names(constraint)]
    # Calculate new weights for each case
    return(prior.weights * adj.scale[case.variables])
}

# Run function over the survey data, weighted successively with weights
fun.with.weights <- function(surv.var, weights, fun, ...) {
    results <- data.frame()
    for ( wt in colnames(weights) )  {
        results <- rbind(results, fun(surv.var, weights[,wt], ...))
    }
    rownames(results) <- colnames(weights)
    results
}

# Area table from weights - HMisc version
area.weighted.table <- function(surv.var, weights, rounded=TRUE) {
    results <- fun.with.weights(surv.var, weights, Hmisc::wtd.table, 'table')
    if ( rounded ) {
        results <- round(results)
    }
    colnames(results) <- levels(surv.var)
    results
}

# Calculate RMSE (mean absolute error per cell)
assess.fit.rmse <- function(cstrs, sims) {
    rmse.tbl <- ldply(names(cstrs), function(c) {
        in.tbl <- cstrs[[c]]
        out.tbl <- area.sim.table(sims,
                                  as.formula(sprintf("~%s",c)))
        in.tbl.p <- prop.table(as.matrix(in.tbl), 1)
        out.tbl.p <- prop.table(as.matrix(out.tbl), 1)
        data.frame(
            n.areas=nrow(in.tbl),
            n.classes=ncol(in.tbl),
            tse=sum( (in.tbl - out.tbl)^2 ), # Total sq. error
            rmse.prop=sqrt( mean( (in.tbl.p - out.tbl.p)^2 )))
    } )
    rmse.tbl$rmse.abs <- with(rmse.tbl,
                              sqrt(tse/(n.areas*n.classes)))
    rmse.tbl <- rmse.tbl[,c(1,2,5,4)]
    row.names(rmse.tbl) <- names(cstrs)
    rmse.tbl
}


# Set up a set of survey objects for the weights +sim.weights+ applied
# to the data +surv.data+. The rownames of the weights should identify a
# case in surv.data, marked by +surv.key+. The optional argument
# +wt.multiplier+ can be used to specify a vector (of
# length=nrow(surv.data)), by which the weights should be adjusted.
area.simulations <- function(sim.weights, surv.data, surv.key,
                             wt.multiplier=1) {
    wts <- sim.weights[match(surv.data[,surv.key], rownames(sim.weights)),,
                       drop=FALSE]
    wts <- wts * wt.multiplier
    sims <- lapply(colnames(wts),
                   function(area) svydesign(data=surv.data,
                                            id=~1,
                                            weights=wts[,area]))
    names(sims) <- colnames(wts)
    sims
}

# Returns the weights in the correct order matched to the dataset
area.weights <- function(sims) {
    wts.l <- lapply(sims, weights)
    do.call(cbind, wts.l)
}

# Generate a table from a formula, using survey
area.sim.table <- function(sims, formula) {
    tbl <- lapply(sims, svytable, formula=formula, round=TRUE)
    do.call(rbind, tbl)
}

# Generate a table from a formula, using survey
area.sim.means <- function(sims, formula) {
    tbl <- lapply(sims, svymean, x=formula, round=TRUE)
    do.call(rbind, tbl)
}

# Generate a table from a formula, using survey
area.sim.totals <- function(sims, formula) {
    tbl <- lapply(sims, svytotal, x=formula, round=TRUE)
    do.call(rbind, tbl)
}

# Generate a table from a formula, using survey
area.sim.quantiles <- function(sims, formula, quantiles=seq(0.1, 0.9, 0.1) ) {
    tbl <- lapply(sims, svyquantile,
                  x=formula, quantiles=quantiles, ties="rounded")
    qiles <- round(do.call(rbind, tbl))
    rownames(qiles) <- names(sims)
    qiles
}
