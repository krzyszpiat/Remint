setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

fullTable <- readRDS("Data/fullTable.RDS")
write.csv(fullTable, "Data/fullTable.csv", row.names = F)
