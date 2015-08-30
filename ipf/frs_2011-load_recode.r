# This is survey data preparation and re-coding for 2011/12. It returns three
# data-sets, 1) frs.ad, 2) frs.hh, which contain a set of variables
# recoded to match Census 2011 definitions, plus 3) hbai, the HBAI set for
# that year

# Faster reading to data.frame
library(readr)

# Change these as appropriate
frs.dat.dir <- "~/Documents/Purgatory/FRS1112/tab/"
hbai.dat.dir <- "~/Documents/Purgatory/HBAI1213/tab/"

# Detailed household, adult information from the FRS
frs.hh  <- read_tsv(paste(frs.dat.dir, "househol.tab", sep=""))
frs.ad  <- read_tsv(paste(frs.dat.dir, "adult.tab", sep=""))

# HBAI
hbai  <- read_tsv(paste(hbai.dat.dir, "hbai1112_g4.tab", sep=""))

# Following variables are recoded in this file
# Adult:
ad.vars <- c(
    "AGE.GROUP", # - 6 broad age classes
    "AGE.SEX",   # - 10 year age groups up to 16-75, plus sex
    "NSSEC.CLASS", #  - factorised social class variables
    "NSSEC.ACTIVE", # - NSSEC, active adults; inactive labelled
    "DEGREE", # - degree or not
    "QUAL", # - degree, no qualifications, all others
    "EMPLOY.STAT", # - 5-way employment status (FT/PT/INACT/UNEMP/RET)
    "LIVARR", # - Whether partner living in household (CPL/SGL)
    "EMPSTAT.LIVARR", # - combined employment status and living arrangements
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
    "ROOMS9", # Number of rooms, capped at 9
    "CTAX.BAND", # Council tax band of dwelling, labelled by letter
    "REGION", # Labelled region name
    "BROAD.REGION" # South/Mids/North + countries separately
    )

# NOT USED FOR NOW
# frs.child <- read.delim(paste(frs.dat.dir, "child.tab", sep=""))
# NOT REQUIRED AT THIS STAGE

# Just England and Wales
frs.hh <- subset(frs.hh, COUNTRY %in% c(1,2))
frs.ad <- subset(frs.ad, SERNUM %in% frs.hh$SERNUM)

# Age group - broad
frs.ad$AGE.GROUP <- cut(frs.ad$IAGEGR4, c(0,5,7,10,13,15,17),
                        labels=c("AGE.16_24", "AGE.25_34", "AGE.35_49",
                                 "AGE.50_64", "AGE.65_74", "AGE.75_"))

# Age & Sex - 10 year groups
ages.10 <- c(NA, NA, NA, "16_24", "25_34", "35_44", "45_54", "55_59", "60_64", "65_74", "75_")
frs.ad$AGE.SEX <- as.factor(paste(c("M", "F")[frs.ad$SEX], ages.10[frs.ad$IAGEGR3], sep="."))


# Note - most inactive adults are not classified
frs.ad$SOC.CLASS <- cut(frs.ad$NSSEC, c(0, 3.9, 5.9, 7.9, 9.9, 11.9, 12.9, 13.9, 15.9, 18),
                        labels=c("HIGHER.MP", "LOWER.MP", "INTER", "SMALL.EMP",
                            "LOWER.ST", "SEMI.RT", "RT", "NEVER", "NOTCLASS") )
# Another copy
frs.ad$NSSEC.CLASS <- frs.ad$SOC.CLASS

## Recoding NSSEC of inactive adults - only active adults have NSSEC
frs.ad$NSSEC.ACTIVE <- as.character(frs.ad$NSSEC.CLASS)
frs.ad[frs.ad$EMPSTATI > 5, "NSSEC.ACTIVE"] <- "INACTIVE"
frs.ad$NSSEC.ACTIVE <- as.factor(frs.ad$NSSEC.ACTIVE)


# FIXME - check coding of qualifications in FRS documentation
frs.ad$DEGREE <- as.factor(ifelse(frs.ad$HI2QUAL==1, "Degree", "NoDegree"))
frs.ad$QUAL <- "Other"
frs.ad[frs.ad$HI2QUAL == 1, "QUAL"] <- "Degree"
frs.ad[frs.ad$HI2QUAL == 8, "QUAL"] <- "None"
frs.ad[frs.ad$HI1QUAL6 %in% c(1,3), "QUAL"] <- "None"
frs.ad$QUAL <- as.factor(frs.ad$QUAL)

# Employment statuses of adults - simplified (all inactive treated as either retired or other)
empstats <- c("FT", "PT", "FT", "PT", "UNEMP", "RET", "INACT_O", "INACT_O", "INACT_O", "INACT_O", "INACT_O")
frs.ad$EMPLOY.STAT <- as.factor(empstats[frs.ad$EMPSTATI])

# Living arrangements (Census definitionP) - whether has partner/spouse in household
frs.ad$LIVARR <- ifelse( frs.ad$MARITAL < 3 & frs.ad$SPOUT < 1, "CPL", "SGL")

# Combined employment status / living arrangement variable
frs.ad$EMPSTAT.LIVARR <- as.factor(paste(frs.ad$LIVARR, as.character(frs.ad$EMPLOY.STAT), sep="."))

# Census 2011 groups: Chinese → Asian
frs.ad$ETHNICITY <- as.factor( c("White", "Mixed", "Asian","Black", "Other")[frs.ad$ETHGR3] )


# Estimation of personal taxable income
frs.ad$ann.tax.income <-
    with(frs.ad, (INEARNS + ININV + SEINCAM2 + INPENINC + INRINC) * 52)

############## HOUSEHOLD ######################
# ACCOMMODATION TYPE
# Flat here includes all other types
frs.hh$ACCOM.TYPE <- cut(frs.hh$TYPEACC, c(0,1,2,3,8),
                      labels=c("DETACHED", "SEMI", "TERRACE", "FLAT") )
# A small number of not-knowns / unclassifed - puts
frs.hh[frs.hh$TYPEACC == -1, "ACCOM.TYPE"] <- "FLAT"


# TENURE
tenures <- c("SOCRENT", "SOCRENT", "PVTRENT", "PVTRENT", "OWNOUT", "OWNMORT")
frs.hh$TENURE <- as.factor(tenures[frs.hh$PTENTYP2])

# Household composition - matches Census "alternative definition"
frs.hh$HHOLD.TYPE <- cut(frs.hh$HHCOMPS, c(0,4,7,8,11,13,14,17),
                      labels=c("SINGLE.NOKIDS", "COUPLE.NOKIDS", "MANY.NOKIDS",
                          "SINGLE.KIDS", "COUPLE.1_2KIDS", "COUPLE.3KIDS",
                          "MANY.KIDS"))
# Number of rooms
frs.hh$ROOMS9 <- cut(frs.hh$ROOMS10, c(0:8, 11),
                  labels=c(paste("R", 1:8, sep=""), "R9PLUS"))

# Council Tax Band of dwelling
frs.hh$CTAX.BAND <- factor(frs.hh$HDTAX, labels=LETTERS[1:8])
summary(frs.hh$HDTAX)
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


### NOT (Yet) implemented
#table(frs.ad$HEALTH)

#### HOUSEHOLD DATA
# Copy HRP information into household dataset
frs.hh <- merge(frs.hh, subset(frs.ad, HRPID == 1, c(ad.vars, "SERNUM")),
                by="SERNUM")
colnames(frs.hh)[match(ad.vars, colnames(frs.hh))] <- paste(ad.vars, "HRP", sep=".")


# Only cases that are in HBAI - so excluding households with non-resident partners
frs.hh <- subset(frs.hh, SERNUM %in% hbai$SERNUM)
frs.ad <- subset(frs.ad, SERNUM %in% hbai$SERNUM)
