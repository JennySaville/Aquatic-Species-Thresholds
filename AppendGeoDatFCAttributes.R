# Scrape watershed acreage and landuse/landcover calcs from geodatabase.

# Used to combine watershed attributes, from watersheds delineated using Arc Hydro
# Tools. Each site's watershed is stored as a separate feature classes in a geodatabase. 
# This pulls all of the site watershed attributes into one data frame.
# Part of a watershed land use/land cover characterization study.

library(plyr)
library(reshape2)
library(RODBC)
library(dplyr)
library(car)
library(tidyr)
library(foreign)

chan <- odbcConnectAccess("H:/xxx/geodat.mdb")

t <- as.data.frame(sqlTables(chan, tableType = 'TABLE'))

table.list <- sqlTables(chan, tableType = 'TABLE')

# ----------------- Acres -------------------------------------

sheds <- c("T2011_1st_level_sheds", "T2011_2nd_level_sheds", "T2011_3rd_level_sheds"
           , "T2011_4th_level_sheds", "T2011_5th_level_sheds", "T2011_6th_level_sheds")
sheds.ls = list(); 
for (i in 1:length(sheds)) { 
  sheds.ac.temp <- sheds[i]
  queryx <- paste("SELECT SITEYR, Altitude, Acres FROM ", sheds.ac.temp, sep = "")
  sheds.ls[[sheds.ac.temp]] <- sqlQuery(chan, paste(queryx))
  sheds.ls[[sheds.ac.temp]]$Provenance <- rep(sheds.ac.temp, nrow(sheds.ls[[sheds.ac.temp]]))
} 
sheds.df <- rbind.fill(sheds.ls)


# ----------------- Karst -------------------------------------
karst <- t[grep("_level_sheds_Dissolve$", t$TABLE_NAME), ]
string.karst <- as.character(unique(karst$TABLE_NAME))

karst.ls.s = list()

for (i in 1:length(string.karst)){
  karst.temp <- string.karst[i]
  queryx <- paste("SELECT SITEYR, KARST_TYPE, AcresKarst FROM ", karst.temp, sep = "")
  karst.ls <- as.data.frame(sqlQuery(chan, paste(queryx)))
  karst.full <- length(karst.ls$SITEYR)
  k.temp <- list(ifelse(karst.full >= 1, karst.temp, NA))
  karst.ls.s[karst.temp] <- k.temp 
  karst.ls.1 <- karst.ls.s[!is.na(karst.ls.s)]
  
}
karst.list <- as.character(unique(karst.ls.1))

karst.ac = list()  

for (j in 1:length(karst.list)) {
  karst.list.temp <- karst.list[j]
  query.y <- paste("SELECT SITEYR, KARST_TYPE, AcresKarst FROM ", karst.list.temp, sep = "")  
  karst.ac[[karst.list.temp]] <- sqlQuery(chan, paste(query.y))
  karst.ac[[karst.list.temp]]$Provenance <- rep(karst.list.temp, nrow(karst.ac[[karst.list.temp]]))
}

karst.df <- rbind.fill(karst.ac)
#--------------------------------------------------------------------
#01ISA
isa.01 <- t[grep("isa01", t$TABLE_NAME), ]
string.isa.01 <- as.character(unique(isa.01$TABLE_NAME))
isa.01.df = list() 
for (i in 1:length(string.isa.01)) { 
  isa.01.df[[i]] <- sqlFetch(chan, string.isa.01[[i]]) 
  isa.01.df[[i]]$Provenance <- rep(string.isa.01[[i]], nrow(isa.01.df[[i]]))
  isa.01.df[[i]]$NLCD_Raster <- rep("2001_ISA", nrow(isa.01.df[[i]]))
  isa.01.df[[i]]$Database <- rep("R1R2R3R4_gdb", nrow(isa.01.df[[i]]))
} 
isa.01.2 <- rbind.fill(isa.01.df)
str(isa.01.2, list.len = ncol(isa.01.2))



# Check and mark dupes
sheds.df.1 <- mutate(sheds.df, Dupe = ifelse(duplicated(sheds.df$SITEYR) | duplicated(sheds.df$SITEYR, fromLast = TRUE), "YES", "NO"))
sheds.df.1$GDB_Origin <- "FolderB_Gdb"

karst.df.1 <- mutate(karst.df, Dupe = ifelse(duplicated(karst.df$SITEYR) | duplicated(karst.df$SITEYR, fromLast = TRUE), "YES", "NO"))
karst.df.1$GDB_Origin <- "FolderB_Gdb"
write.csv(sheds.df.1, "FolderB_Acres.csv", row.names = FALSE)
write.csv(karst.df.1, "FolderB_Karst.csv", row.names = FALSE)