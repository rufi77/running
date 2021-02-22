hm_print <- function(dat){
  
  cut <- 17
  t <- (dat 
  %>% select(start, end, date, hm_diff, time, hm_h) 
  %>% arrange(desc(hm_h)) 
  %>% mutate(start = substr(start, 1, cut), end = substr(end, 1, cut)))

  pander(t)
}