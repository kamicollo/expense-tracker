library(tabulizer)
library(data.table)
library(stringr)
library(zoo)
setwd('/home/aurimas/coding/R/expenses')
source("pdf_processing.R")
source("data_processing.R")

mid_area = c(202.65, 63.36, 800, 596)
first_area = c(384.4, 65, 800, 596)
checksums = c(5820.4, 3892.85, 2371.15, 4857.3, 1115.30, 2037.85, 5478.95, 3918, 5919.45, 3392.05, 2676.75, 2560.15, 2787.50, 4501.40)

rm(out)
if (!exists("out")) {
    files = paste("viseca/", dir("viseca"), sep="")
    out = lapply(files, function(x, first_area, mid_area) {
        print(x)
        output = extract_viseca_tables_manual(x, first_area, mid_area)
        data = preprocessData(output)
        data[, fileNo := which(files == x)]
        checks = rbind(
            data[,list(sum=sum(amountCHF, na.rm = T)), by=card],
            data[,list(card="Total", sum=sum(amountCHF, na.rm = T))],
            data[, list(card="Checksum", sum=checksums[fileNo[1]])],
            data[, list(card="Filename", sum=files[fileNo[1]])]
        )
        print(checks)
        return(output)
    }, first_area, mid_area)
    
}

#OK: 1, 2, 3, 6, 7, 
#8 - wrong card attribution
#7/8/10/11 wrong
#5, 4 wrong sums (missing transactions)


data = lapply(c(1:12), function(x) {
    data = preprocessData(out[[x]])
    data[, fileNo := x]
    output = rbind(
        data[,list(sum=sum(amountCHF, na.rm = T)), by=card],
        data[,list(card="Total", sum=sum(amountCHF, na.rm = T))],
        data[, list(card="Checksum", sum=checksums[fileNo[1]])],
        data[, list(card="Filename", sum=files[fileNo[1]])]
    )
    print(output)
    return(data)
})

nas = lapply(out, function(x) {preprocessData(x)[is.na(amountCHF), ]})
print(nas)

# result = lapply(data, function(x) {
#     rbind(
#         x[,list(sum=sum(amountCHF, na.rm = T)), by=card],
#         x[,list(card="Total", sum=sum(amountCHF, na.rm = T))],
#         x[, list(card="Checksum", sum=checksums[fileNo[1]])],
#         x[, list(card="Filename", sum=files[fileNo[1]])]
#     )
# })
# 
# print(result)

#for Testing duplicate text columns / empty columns
#setcolorder(page[, V9 := V4][, V10 := V5], c("V1", "V2", "V3", "V4", "V9", "V5", "V6", "V10","V7", "V8"))

#all_data[!(details %like% "settlement")][card %in% c('4627 2370 0032 7422', '4627 2370 0039 3150')][,list(total=sum(amountCHF, na.rm = T)), by = list(details)][order(total, decreasing = T)][1:50]