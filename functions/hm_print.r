hm_print <- function(dat){
  
  cut <- 18
  t <- (dat 
  %>% select(start, end, date, hm_diff, time, hm_h, begleitung1) 
  %>% arrange(desc(hm_h)) 
  %>% mutate(start = substr(start, 1, cut), end = substr(end, 1, cut)))

  return(t)
}