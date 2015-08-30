# Defining the constraints by linking variables in the survey cases to
# CSV tables with local population totals

### HOUSEHOLD CONSTRAINTS
hhold.constraint.tables <- read.constraints( list(
    # DC4402EW - Accommodation type by type of central heating in household by tenure
    "ACCOM.TYPE"  = "constraints/london/lnd-accom_type-2011.csv",
    # QS405EW - Tenure - Households
    "TENURE"      = "constraints/london/lnd-tenure-2011.csv",
    # [N'hoods Stats data]
    "CTAX.BAND"   = "constraints/london/lnd-dwellstock-2011.csv",
    "EMPLOY.STAT.HRP" = "constraints/london/lnd-econact_hrp-2011.csv",
    "HHOLD.TYPE"  = "constraints/london/lnd-hhtype_alt-2011.csv"
    ) )

### ADULT constraints
adult.constraint.tables <- read.constraints( list(
    # KS201EW - Ethnic group
    "ETHNICITY"      = "constraints/london/lnd-ethnicity-2011.csv",
    # DC6606EW
    "NSSEC.ACTIVE"   = "constraints/london/lnd-nssec_econact-2011.csv",
    # DC2101EW - Ethnic group by sex by age
    "AGE.SEX"        = "constraints/london/lnd-age_sex-2011.csv",
    # DC6401EW - Economic activity by living arrangements
    "EMPSTAT.LIVARR" = "constraints/london/lnd-empstat_livarr-2011.csv"
    ) )
