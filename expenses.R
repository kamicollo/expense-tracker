library(tabulizer)
library(data.table)
library(stringr)
library(zoo)
setwd('/home/aurimas/coding/R/expenses')
source("pdf_processing.R")
source("data_processing.R")

mid_area = c(202.65, 63.36, 800, 596)
first_area = c(384.4, 65, 800, 596)

checksums = new.env()
checksums[["viseca/14ea879b-8fec-4d47-a38d-8046dc3ad7ff.pdf"]] = 5503.75
checksums[["viseca/1e6a1e5b-6e58-4665-93a8-457b93e9af83.pdf"]] = 4687.05
checksums[["viseca/281df4d4-2673-4849-b782-df61b5696a14.pdf"]] = 5820.4
checksums[["viseca/2c33de6f-5227-44c9-b1e3-d698b47681d6.pdf"]] = 3892.85
checksums[["viseca/3c4384d3-dc1e-4409-8141-733bc17b42e2.pdf"]] = 2371.15
checksums[["viseca/45ffc156-efcc-40ba-a728-3abcee58054d.pdf"]] = 4857.3
checksums[["viseca/47224925-2da2-4f05-bf51-bd92a031eac6.pdf"]] = 1115.30
checksums[["viseca/615d0e40-31db-4cdc-b77b-d423b19c5c4b.pdf"]] = 2037.85
checksums[["viseca/883d5cc7-3cf9-4338-8f8a-d3fc37e0a10c.pdf"]] = 5478.95
checksums[["viseca/8f55070c-5688-49b8-ba28-a1be91de7894.pdf"]] = 3918
checksums[["viseca/96ae888a-2960-4ab7-b005-c9cd58164045.pdf"]] = 5919.45
checksums[["viseca/ae39213d-5cd6-4c0e-8198-a590fa263ca4.pdf"]] = 3392.05
checksums[["viseca/c1bead69-52e6-427b-a954-2e7efe4d4cc4.pdf"]] = 2676.75
checksums[["viseca/df7daa57-f099-44fa-98ec-42976a61d7ba.pdf"]] = 2560.15
checksums[["viseca/e5d95e14-974e-439b-ad41-9679199e07d4.pdf"]] = 2787.50
checksums[["viseca/f444ecf6-12f1-4172-855b-bfc9071f9c01.pdf"]] = 5964
checksums[["viseca/f8c8aa5d-f1dc-4dfd-b1af-a7d4ae02e64c.pdf"]] = 4501.40


### PARSING OF PDFs into LISTS
REPARSE = FALSE
if (REPARSE) { rm(out) }

if (!exists("out")) {
    files = paste("viseca/", dir("viseca"), sep="")
    out = lapply(files, function(x, first_area, mid_area) {
        print(x)
        output = extract_viseca_tables_manual(x, first_area, mid_area)
        return(output)
    }, first_area, mid_area)
}

## PARSING OF LISTS INTO DATA

#ToDo's:
# - automatically extract grand totals and check against them (extraction done)
# - automatically extract card subtotals and check against them
# - review all cases of empty amountCHF entries to understand what metadata was not processed

data = lapply(c(1:length(files)), function(x) {
    data = preprocessData(out[[x]])
    data[, fileNo := files[x]]
    output = rbind(
        data[, list(card="Filename", sum=fileNo[1])],
        data[, list(sum=sum(amountCHF, na.rm = T)), by=card],
        data[, list(card="Total", sum=sum(amountCHF, na.rm = T))],
        list(card=paste("Checksum", attr(data, "cardtotals")$card), sum=attr(data, "cardtotals")$amountCHF),
        data[, list(card="Checksum TOTAL", sum=attr(data, "total"))]
    )
    print(output)
    return(data)
})

# - review all cases of empty amountCHF entries to understand what metadata was not processed
nas = lapply(out, function(x) {preprocessData(x)[is.na(amountCHF), details]})
#print(nas)
