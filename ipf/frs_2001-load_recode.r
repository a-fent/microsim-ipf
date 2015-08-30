# This is survey data preparation and re-coding for 2001/02. It returns
# three data-sets, 1) frs.ad, 2) frs.hh, which contain a set of
# variables recoded to match Census 2001 definitions, plus 3) hbai, the
# HBAI set for that year

library(readr)

# Change these as appropriate
frs.dat.dir <- "~/Documents/Purgatory/FRS0102/tab/"
hbai.dat.dir <- "~/Documents/Purgatory/HBAI1213/tab/"

hbai  <- read_tsv(paste(hbai.dat.dir, "hbai0102.tab", sep=""))

# Detailed benefit unit, adult and benefits information from the FRS
frs.hh  <- read_tsv(paste(frs.dat.dir, "househol.tab", sep=""))
frs.ad  <- read_tsv(paste(frs.dat.dir, "adult.tab", sep=""))

# Following variables are recoded in this file
# Adult:
ad.vars <- c(
#    "AGE.GROUP", # - 6 broad age classes
    "AGE.SEX",   # - 10 year age groups up to 16-75, plus sex
    "NSSEC.CLASS", #  - factorised social class variables
    "NSSEC.ACTIVE", # - NSSEC, active adults; inactive + age>74 labelled separately
#    "DEGREE", # - degree or not
#    "QUAL", # - degree, no qualifications, all others
    "EMPLOY.STAT", # - 4-way employment status (EMP/INACT/UNEMP/RET)
    "EMPLOY.STAT.PT", # - 5-way employment status (FT/PT/INACT/UNEMP/RET)
    "EMPLOY.STAT.74", # - 5-way employment status (EMP/INACT/UNEMP/RET/OVER74),
    "LIVARR", # - Whether partner living in household (CPL/SGL)
    "EMPSTAT.LIVARR", # - combined employment status and living arrangements (with PT/FT + OVER74)
    "ETHNICITY" # 5-way major ethnic group
    )

# Household:
# In addition, the above variables for HRP adults are copied
# into the household dataset, with the suffix .HRP
hh.vars <- c(
    "SERNUM", # Household serial number
    "ACCOM.TYPE", # Flat / terr / semi / detached
    "TENURE", # 4-way housing tenure, soc & private rent, mortage/outright ownership
    "HHOLD.TYPE", # Household type classification (cpl/sgl/many adults, n kids)
    # "ROOMS9", # Number of rooms, capped at 9
    "CTAX.BAND", # Council tax band of dwelling, labelled by letter
    "REGION", # Labelled region name
    "BROAD.REGION" # South/Mids/North + countries separately
    )

# Just England and Wales
frs.hh <- subset(frs.hh, GVTREGN < 12) # Country is not coded in FRS 2001/02
frs.ad <- subset(frs.ad, SERNUM %in% frs.hh$SERNUM)


### PERSON VARIABLES ###
# Age-sex
ages.10 <- c(NA, NA, NA, "16_24", "25_34", "35_44", "45_54", "55_59", "60_64", "65_74", "75_", "75_")
frs.ad$AGE.SEX <- as.factor(paste(c("M", "F")[frs.ad$SEX],
                                  ages.10[frs.ad$IAGEGR2], sep="."))

# Employment statuses of adults - simplified (all inactive treated as either retired or other)
empstats1 <- c("FT", "PT", "FT", "PT", "UNEMP", "RET",
              "INACT_O", "INACT_O", "INACT_O", "INACT_O", "INACT_O")
frs.ad$EMPLOY.STAT.PT <- empstats1[frs.ad$EMPSTATI]

# Employment statuses of adults - simplified, FT+PT combined
empstats2 <- c("EMP", "EMP", "EMP", "EMP", "UNEMP", "RET",
              "INACT_O", "INACT_O", "INACT_O", "INACT_O", "INACT_O")
frs.ad$EMPLOY.STAT <- empstats2[frs.ad$EMPSTATI]

# Recode if over-74 - 2001 Census economic variables (NS-SEC, EMPsTAT)
# often only include for 16-74 year olds
frs.ad$EMPLOY.STAT.74 <- ifelse(frs.ad$IAGEGR2 > 10, "OVER_74", frs.ad$EMPLOY.STAT)
frs.ad$EMPLOY.STAT.74 <- as.factor(frs.ad$EMPLOY.STAT.74)

# Living arrangements
frs.ad$LIVARR <- ifelse( frs.ad$MARITAL < 3 & frs.ad$SPOUT < 1, "CPL", "SGL")
# Combined employment status / living arrangement variable
frs.ad$EMPSTAT.LIVARR <- paste(frs.ad$LIVARR, as.character(frs.ad$EMPLOY.STAT.PT), sep=".")
frs.ad$EMPSTAT.LIVARR <- ifelse(frs.ad$IAGEGR2 > 10, "OVER_74", frs.ad$EMPSTAT.LIVARR)
frs.ad$EMPSTAT.LIVARR <- as.factor(frs.ad$EMPSTAT.LIVARR)

# NSSEC Class
# Note - most inactive adults are "not classified" in FRS, so are excluded
frs.ad$NSSEC.CLASS <- cut(frs.ad$NSSEC, c(0, 3.9, 5.9, 7.9, 9.9, 11.9, 12.9, 13.9, 15.9, 18),
                        labels=c("HIGHER.MP", "LOWER.MP", "INTER", "SMALL.EMP",
                            "LOWER.ST", "SEMI.RT", "RT", "NEVER", "NOTCLASS") )
# Count out over 74's and inactive;
frs.ad$NSSEC.ACTIVE <- as.character(frs.ad$NSSEC.CLASS)
frs.ad[frs.ad$EMPSTATI > 5, "NSSEC.ACTIVE"] <- "INACTIVE_O74"
frs.ad[frs.ad$IAGEGR2 > 10, "NSSEC.ACTIVE"] <- "INACTIVE_O74"
frs.ad[is.na(frs.ad$NSSEC.ACTIVE), "NSSEC.ACTIVE"] <- "NEVER_NOT"
frs.ad$NSSEC.ACTIVE <- as.factor(frs.ad$NSSEC.ACTIVE)

ethnix <- c(rep("White", 2), rep("Mixed", 4), rep("Asian", 4),
            rep("Black", 3), rep("Other", 2) )
# Census 2001 Groups
frs.ad$ETHNICITY <- as.factor( ethnix[frs.ad$ETHGRP] )


# Estimation of personal taxable income
frs.ad$ann.tax.income <-
    with(frs.ad, (INEARNS + ININV + SEINCAM2 + INPENINC + INRINC) * 52)

### HOUSEHOLD VARIABLES ###
# Housing tenure
tenures <- c("SOCRENT", "SOCRENT", "PVTRENT", "PVTRENT", "OWNOUT", "OWNMORT")
frs.hh$TENURE <- as.factor(tenures[frs.hh$PTENTYPE])

# Accommodation type
accoms <- c("DETACHED", "SEMI", "TERR", "FLAT_OTHER", "FLAT_OTHER")
frs.hh$ACCOM.TYPE <- as.factor(accoms[frs.hh$PACCTYPE])

# Household types
hh.types <- c("SP.P", "SP.NP", "CPL.NK", "CPL.NK", "CPL.NK", "MULTI.NK",
              "LP.1K", "LP.MK", "LP.MK", "CPL.1K", "CPL.MK", "CPL.MK",
              "MULTI.K", "MULTI.K", "MULTI.K")
frs.hh$HHOLD.TYPE <- as.factor(hh.types[frs.hh$HHCOMP])

# Council tax bands
# "Not valued separately" (v small num coses) → C (for now)
bands <- c(LETTERS[1:8], "C")
frs.hh$CTAX.BAND <- as.factor(bands[frs.hh$CTBAND])

# HOUSEHOLD DATA
# Copy HRP information into the household dataset
frs.hh <- merge(frs.hh, subset(frs.ad, HRPID==1, c(ad.vars, "SERNUM")),
                by="SERNUM")
# Add .HRP suffix to identify these
colnames(frs.hh)[match(ad.vars, colnames(frs.hh))] <- paste(ad.vars, "HRP", sep=".")

# Broad regions
reg.recode <- c("NORTH", # NE
                "NORTH", # NW
                "NORTH", # N/A
                "NORTH", # YH
                "MIDS", # EM
                "MIDS", # WM
                "SOUTH", # EE
                "SOUTH", # LO
                "SOUTH", # SE
                "MIDS", # SW
                "WALES", # WA
                "SCOT", # SC
                "NIRE") # NI

frs.hh$BROAD.REGION <- as.factor(reg.recode[frs.hh$GVTREGN])

reg.recode2 <- c("NE", # NE
                 "NW", # NW
                 "", # N/A
                "YH", # YH
                "EM", # EM
                "WM", # WM
                "EE", # EE
                "LO", # LO
                "SE", # SE
                "SW", # SW
                "WA", # WA
                "SC", # SC
                "NI") # NI
frs.hh$REGION <- reg.recode2[frs.hh$GVTREGN]


# Only cases that are in HBAI - so excluding households with non-resident partners
frs.hh <- subset(frs.hh, SERNUM %in% hbai$SERNUM)
frs.ad <- subset(frs.ad, SERNUM %in% hbai$SERNUM)
