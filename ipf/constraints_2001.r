# Defining the constraints by linking variables in the survey cases to
# CSV tables with local population totals

### HOUSEHOLD CONSTRAINTS
hhold.constraint.tables <- read.constraints( list(
    "ACCOM.TYPE"  = "constraints/london/lnd-accom_type-2001.csv",
    "TENURE"      = "constraints/london/lnd-tenure-2001.csv",
    "CTAX.BAND"   = "constraints/london/lnd-ctax_band-2001.csv",
    "EMPLOY.STAT.74.HRP" = "constraints/london/lnd-econact_hrp-2001.csv", 
    "HHOLD.TYPE"  = "constraints/london/lnd-hhold_comp-2001.csv"
    ) )

### ADULT CONSTRAINTS
adult.constraint.tables <- read.constraints( list(
    "ETHNICITY"      = "constraints/london/lnd-ethnicity-2001.csv",
    "NSSEC.ACTIVE"   = "constraints/london/lnd-nssec_econact-2001.csv",
    "AGE.SEX"        = "constraints/london/lnd-age_sex-2001.csv",
    "EMPSTAT.LIVARR" = "constraints/london/lnd-empstat_livarr-2001.csv") 
)
