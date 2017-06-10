#wrapper to process multiple pages and add card identifiers / clean other irrelevant entries
preprocessData = function(PDFoutput) {
    
    #get the data from pages
    all_pages = do.call("rbind", 
        lapply( seq(1, length(PDFoutput)), 
                function(i) {
                    data = as.data.table(PDFoutput[[i]], stringsAsFactors = FALSE)
                    data = preprocessPage(data)
                    data[, pageID := i]
                    return(data)
                }
        )
    )
    
    #infer the associated card number, delete the entries afterwards
    all_pages[, card := str_match(details, '[0-9]{4} [0-9]{4} [0-9]{4} [0-9]{4}')]
    entries = which(all_pages[,!is.na(card)])
    if (length(entries) > 0) {
        all_pages[, card := card[1], by = cumsum(!is.na(card))]
        all_pages[, card := na.locf(card, fromLast = T)]
        all_pages = all_pages[-entries]
    } else {
        all_pages[, card := "unknown card #"]
    }
    
    #exclude subtotals (TODO - consider checking data against them)
    all_pages = all_pages[!(details == "Total carried forward" | details == "Card carried forward")]
    all_pages = all_pages[!(details == "Total interim amount" | details == "Card interim amount")]
    all_pages = all_pages[!(details == "Total balance in our favour" | details %like% "The amount due will be debited to your account")]
    
    #exclude other random entries
    all_pages = all_pages[!(details %like% "Card limit CHF " | details %like% "Card blocked")]
    
    #process metadata
    all_pages = process_metadata(all_pages)
}


#this function processes the data as parsed by tabularizer from PDF and normalizes it
preprocessPage = function(data) {
    
    #deal with incorrectly parsed PDF data
    #check if there are any empty columns
    empty = which(empty_columns(data))
    text = which(text_columns(data))
    num  = which(number_columns(data))
    curr = which(currency_columns(data))
    dates = which(date_columns(data))
    
    if (length(empty) > 0) {
        data[, (empty) := NULL]
        return(preprocessPage(data))
    }
    
    #check that we have 1 currency column & 2 number columns
    if (length(curr) == 1 & length(num) == 2) {
        #yes- then process the text/date columns
        text = preprocessText(data[, .SD, .SDcols = -c(curr, num)])
        values = setnames(data[, .SD, .SDcols = c(curr, num)], c("currency", "amount", "amountCHF"))
        data = cbind(text, values)
    } else if (length(curr) == 0 & length(num) == 1) {
        text = preprocessText(data[, .SD, .SDcols = -c(curr, num)])
        values = setnames(data[, .SD, .SDcols = num], "amountCHF")
        values[, c("currency", "amount") := NA]
        data = cbind(text, values)
    } else if (length(data) == 0) {
        return(NULL)
    } else {
        #TODO - if not currency column + 2 number columns
        print(data)
        stop("unrecognized input - unknown number of columns")
    }
    
    #apply general data conversions
    #deal with negative numbers
    data[amountCHF %like% " -", amountCHF := paste("-",str_replace(amountCHF, " -", ""), sep="")]
    data[amount %like% " -", amount := paste("-",str_replace(amount, " -", ""), sep="")]
    
    data[, amount := as.numeric(str_replace(amount, "'", ""))]
    data[, amountCHF := as.numeric(str_replace(amountCHF, "'", ""))]
    
    data$date = as.Date(data$date, "%d.%m.%y")
    data$valueDate = as.Date(data$valueDate, "%d.%m.%y")
    data$currency = str_trim(data$currency)
    
    data[, details := str_trim(details)]
    
    #deal with foreign currency entries
    # - find all foreign entries
    #which(data$currency != ""])
    data[, processing_fee := NA_real_]
    data[, fee_percentage := NA_real_]
    data[, converted_CHF := NA_real_]
    data[, conversion_rate := NA_real_]
    data[, conversion_date := as.Date(NA)]
    data[, type := "LOCAL"]
    
    return(data)
}

process_metadata = function(data) {
    all_triplets = which(data[, shift(substring(details, 0, 14), 2, type = "lead") == 'Processing fee' & shift(substring(details, 0, 13), 1, type = "lead") == 'Currency rate'])
    
    #record the processing fees appropriately
    data[all_triplets, processing_fee := as.double(data[all_triplets+2, amount])]
    data[all_triplets, fee_percentage := as.double(str_extract(data[all_triplets+2, details], "[0-9]+\\.[0-9]+"))]
    
    #record the conversion rate/date/amount
    data[all_triplets, converted_CHF := as.double(data[all_triplets+1, amount])]
    data[all_triplets, conversion_rate := as.double(str_extract(data[all_triplets+1, details], "[0-9]+\\.[0-9]{4}"))]
    data[all_triplets, conversion_date := as.Date(
        str_extract(data[all_triplets+1, details], "[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}")
        , "%d.%m.%y")
        ]
    #record type
    data[all_triplets, type := "FOREIGN"]
    
    #delete processed rows
    if (length(all_triplets) > 0) {
        data = data[-c(all_triplets+1, all_triplets+2)]
    }
    
    #deal with processing fees only
    fees = which(data[, shift(details, 1, type = "lead") %like% 'Processing fee'])
    
    if (length(fees)) {
        #add them to processing fee column, mark transaction type and remove the metadata-row
        data[fees, processing_fee := data[fees+1, amount]]
        data = data[-(fees+1)]
    }
    
    #deal with ATM comission fees in Switzerland
    #identify rows which are followed by commission amounts
    rows = which(
        data[, 
             shift(details, 1, type = "lead") == 'Debit commission for ATM in Switzerland' & 
                 (amountCHF - amount == shift(amountCHF, 1, type = "lead"))
             ]
    )
    if (length(rows)) {
        #add them to processing fee column, mark transaction type and remove the metadata-row
        data[rows, processing_fee := amountCHF - amount]
        data[rows, type := "ATM"]
        data = data[-(rows+1)]
    }
    
    #TODO: Catch all other meta data: 
    #data[!is.na(amountCHF), ID:=.I]
    #data[, ID := ID[1], by = cumsum(!is.na(ID))]
    #data[ID %in% data[is.na(amountCHF), ID], metadata := paste(details[-1], collapse = " "), by = ID]
    return(data)
}

preprocessText = function(data) {
    text = which(text_columns(data))
    dates = which(date_columns(data))
    if (length(text) == 1 & length(dates) == 2) {
        setcolorder(data, c(text, dates))
    } else {
        data[, temp := do.call(paste0, .SD)]
        data = extract_date_and_text(data[, temp])
        
    }
    setnames(data, c("details", "date", "valueDate"))
    return(data)
}

#This function extracts date information that got merged into the description field upon pdf extraction
extract_date_and_text = function(v) {
    #setup the column names
    data = as.data.table(v)
    setnames(data, "details")
    data[, c("date", "valueDate") := NA]
    
    #identify the dates in text
    matches = regexec("([0-9]{2}\\.[0-9]{2}\\.[0-9]{2}) ([0-9]{2}\\.[0-9]{2}\\.[0-9]{2}) (.+)", data[, details])
    extracted_matches = regmatches(data[, details], matches)
    
    #parse out the dates into a nice dataframe
    g = sapply(extracted_matches, function(x) {
        #if dates were found, put them into a vector
        if (length(x) == 4) {
            return(x[-1])
            #otherwise return an empty vector
        } else {
            return(c(NA, NA, NA))
        }
    })
    #the result is a character vector - transform
    result = transpose(as.data.table(g))
    setnames(result, c("date", "valueDate", "details"))
    
    #update the original dataset with extracted values
    affectedrows = which(!is.na(result$date))
    data$date[affectedrows] = result$date[affectedrows]
    data$valueDate[affectedrows] = result$valueDate[affectedrows]
    data$details[affectedrows] = result$details[affectedrows]
    
    setcolorder(data, c("details", "date", "valueDate"))
    return(data)
}

check_columns = function(dataframe, regex) {
    apply(dataframe, 2, function(x) { 
        sum(!is.na(str_match(x, regex)[, 1])) + sum(is.na(x)) + sum(x == "", na.rm = T) == length(x) &
            sum(!is.na(str_match(x, regex)[, 1])) > 0    
    })
}

number_columns = function (dataframe) {
    regex = "^\\s*[0-9]{1,3}('[0-9]{1,3})*\\.[0-9]{2}( -)?\\s*$"
    check_columns(dataframe, regex)
}

currency_columns = function (dataframe) {
    regex = "^\\s*[A-Z]{3}\\s*$"
    check_columns(dataframe, regex)
}

date_columns = function (dataframe) {
    regex = "^\\s*[0-9]{2}\\.[0-9]{2}\\.[0-9]{2}\\s*$"
    check_columns(dataframe, regex)
}

empty_columns = function (dataframe) {
    apply(dataframe, 2, function(x) { 
        sum(is.na(x)) + sum(x == "", na.rm = T) == length(x)
    })
}

text_columns = function(dataframe) {
    !(date_columns(dataframe) | currency_columns(dataframe) | number_columns(dataframe))
}