input_touren <- function(path){
  
  # Touren
  touren0 <- read_excel(paste(path, "data/touren.xls", sep = ""))
  
  # Hoehenmeter
  hm <- read_excel(paste(path, "data/hm.xlsx", sep = ""))
  hm$mum <- with(hm, pmax(mum_prov, mum_def, na.rm = TRUE))
  
  # merge the two and add time in hours
  touren <- (touren0 %>% left_join(hm[, c("ort", "mum")], by = c("start" = "ort")) 
             %>% rename(start_hm = mum) 
             %>% left_join(hm[, c("ort", "mum")], by = c("end" = "ort")) 
             %>% rename(end_hm = mum) 
             %>% mutate(time_h = as.numeric(hms(time, quiet = TRUE)) / 3600))
  
  # add Hm and Hm / h
  touren <- (touren %>% mutate(hm_diff = end_hm - start_hm, hm_h = round(hm_diff / time_h)))
  
  # overwrite if Hm manually given
  ind <- is.na(touren$hm_manual) == FALSE
  touren <- (touren %>% mutate(hm_diff = ifelse(ind, touren$hm_manual[ind], hm_diff), hm_h = round(hm_diff / time_h)))
  
  
  touren[ind, "hm_diff"] <- touren$hm_manual[ind]
  
  # add season
  cuts <- parse_date_time(x = "2000-06-30", orders = "ymd") + years(x = seq.int(from = 0, to = 50, by = 1))
  touren$season <- cut.POSIXt(x = parse_date_time(x = touren$date, orders = "ymd"), breaks = cuts, 
                              labels = c(paste("Winter 0", 0:8, "/0", 1:9, sep = ""), 
                                         "Winter 09/10", paste("Winter ", 10:49, "/", 11:50, sep = "")))
  touren <- (touren %>% mutate(date = as.Date(date, origin = "1899-12-30")))
  touren <- (touren %>% mutate(beg = paste(begleitung1, begleitung2, begleitung3, begleitung4)))

  touren <- as.data.frame(touren)
  char_cols <- c("sport", "region", "start", "via1", "via2", "end", "date", "time", "begleitung1", "begleitung2", 
                 "begleitung3", "begleitung4", "kommentar", "beg")
  touren[, char_cols][apply(touren[, char_cols], 1:2, function(x){is.na(x)})] <- ""
  
  return(touren)
}