library(data.table)
library(CDISC)

dm <- cdiscpilot01$sdtm$dm
saveRDS(dm, "dm.rds")
names(dm) <- toupper(names(dm))
fwrite(dm, "dm.csv")

adsl <- cdiscpilot01$adam$adsl
saveRDS(adsl, "adsl.rds")
names(adsl) <- toupper(names(adsl))
fwrite(adsl, "adsl.csv")

