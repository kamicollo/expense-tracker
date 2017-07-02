#PDF processing based on areas to get the text per line and then relying on regex to parse columns
#works well, but more sensitive to new table elements / language / etc
extract_viseca_tables_manual = function(filename, first_area, mid_area) {
    #extract information from PDFs
    pages = get_n_pages(filename)
    extract = extract_tables(
            file=filename, 
            area = c(list(first_area), rep(list(mid_area), pages - 1)),
            guess=F
    )
    
    #extract columns from each page
    data = lapply(extract, function(page) {
        page = as.data.table(page)
        extract_viseca_columns(page[, tmp := do.call(paste, .SD)][, tmp])
    })
    
    #delete all rows after the final statement to avoid catching random information (only applicable to last page)
    data = delete_rows_after_total(data, "The amount due will be debited to your account")
    
    #return only the pages where there is data
    data[which(!sapply(data, is.null))]
    
}

#processes a vector to identify underlying columns of data based on regex patterns
extract_viseca_columns = function(v) {
    #setup a data table out of passed vector
    data = as.data.table(v)
    setnames(data, "details")
    data[, currency := NA]
    data[, amount := NA]
    data[, amountCHF := NA]
    
    #identify currency & number columns
    matches = 
        str_match(
            data[, details], 
            paste(
                "\\s+([A-Z]{3})?",
                "([0-9]{1,3}('[0-9]{1,3})*\\.[0-9]{2}( -)?)?",
                "([0-9]{1,3}('[0-9]{1,3})*\\.[0-9]{2}( -)?)?$",
                sep = "\\s*"
            )
        )
    #update number & currency columns accordingly
    data[, c("currency", "amount", "amountCHF") := list(matches[, 2], matches[, 3], matches[, 6])]
    #fix the case where no-currency amounts get into "amount" column, not "amountCHF"
    data[is.na(currency) & is.na(amountCHF) & !is.na(amount), c("amountCHF", "amount") := list(amount, NA)]
    
    #update detail column where there were matches to values, not only empty strings
    real_replacements = !is.na(matches[, 1]) & is.na(str_match(matches[,1], "^\\s+$"))[,1]
    data[real_replacements, details := str_replace(details, matches[real_replacements, 1], "")]
    
    #fix the case where currency alone was extracted (no amounts) - should be part of details
    data[
        !is.na(currency) & is.na(amountCHF) & is.na(amount), 
        c("details", "currency") := list(paste(details, currency), NA)
    ]
    
    return(data)
}

#This function runs through a list of data tables and delete all entries after a particular text is found
#used to eliminate unneccessary information captured after the last statement row
delete_rows_after_total = function(data, total_row) {
    
    #find the row (returns a list with row # per page - ie. all empty but one)
    last_row = sapply(data, function(x) {which(x[, details %like% total_row])})
    
    
    
    #make sure to return null if last_row was there already or a subset of rows if found now
    data = lapply(
        seq(1, length(data)), 
        function(index, d, r) {
            #check if row was found until this index - if so, return nothing
            if (index > 1 & sum(unlist(r[1:index-1]) > 0)) {
                return(NULL)
                #else check if it was not found yet - then return everything
            } else if (sum(r[[index]]) == 0) {
                return(d[[index]])
                #row was found in this dataset - return partially    
            } else {
                return(d[[index]][1:r[[index]]])
            }
        }, 
        data, 
        last_row
    )
    return(data)
}

#PDF processing taking into account page layout - relies on Tabula algorithms to infer columns
#no longer used due to Tabula not extracting columns perfectly all the time
extract_viseca_tables = function(filename, first_area, mid_area) {
    pages = get_n_pages(filename)
    extract = list()
    #for all pages except last, extract tables from a given area - usually works well
    if (pages > 1) {
        extract = extract_tables(
            file=filename, 
            pages=seq(1,pages)[-pages], 
            area = c(list(first_area), rep(list(mid_area), pages - 2)),
            guess=F
        )
    }
    #for last page, not possible to predict the bottom of the table - rely on Tabula to guess
    extract_last = extract_tables(filename, pages=pages, guess=T)
    return(c(extract, extract_last))
}