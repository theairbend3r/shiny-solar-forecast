preprocessEDA <- function(df, granularity ="Monthly", date_start, date_end) {
  if (granularity == "Hourly") gran = "1 hour" 
  if (granularity == "Daily") gran = "1 day"
  if (granularity == "Monthly") gran = "month"
  if (granularity == "Yearly") gran = "year"
  
  df <- df %>%
    mutate(date_time = date(date_time)) %>%
    filter(date_time >= date(date_start), date_time <= date(date_end)) %>%
    group_by(date_time = floor_date(date_time, gran)) %>%
    summarise_all(mean)
  # mutate_all(vars(-date_time), round(2))
  
  print(df)
  
  return (df)
}