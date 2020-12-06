library(tidyverse)
library(tidytext)
library(rvest)
library(httr)
library(lubridate)

records <- list()

# Scrapes archived versions of TD's top page. Warning: this takes a long time.
for (month in 11:11) {
  for (day in 12:31) {
    # Fetch Internet Archives JSON w dates.
    month_str <- str_pad(month, 2, pad = "0")
    day_str <- str_pad(day, 2, pad = "0")
    top_url <- str_interp('https://web.archive.org/__wb/calendarcaptures/2?url=https%3A%2F%2Fthedonald.win%2Ftop&date=2020${month_str}${day_str}')
    response <- GET(top_url)
    
    if (http_error(response) || http_type(response) != 'application/json') {
      print(str_interp('Skipping ${day_str}/${month_str}'))
      next
    }
    
    # Get page for each capture during the day.
    json <- content(response, as="parsed")
    for (item in json$items) {
      time_str <- str_pad(item[[1]], 6, pad = "0")
      url1 <- str_interp('https://web.archive.org/web/2020${month_str}${day_str}${time_str}/https://thedonald.win/top')
      # https://web.archive.org/web/20201111223748/https://thedonald.win/top
      # https://web.archive.org/web/20201111223748/https://thedonald.win/top
      print(str_interp('Fetching day ${day}/${month} ${time_str} url: ${url1}'))
      
      td <- read_html(url1)
      posts <- td %>% html_nodes(".title")
      
      # https://web.archive.org/web/20200909102030/https://thedonald.win/p/HENl5fAu/walked-away/
      # https://web.archive.org/web/20200909102030/https://thedonald.win/p/HENl5fAu/walked-away/
      
      # Get page for each post during that capture.
      for (post in posts[-1]) {
        td_url <- post %>% xml_attr("href") %>% str_replace_all("[\r\n]" , "")
        url2 <- str_interp('https://web.archive.org${td_url}')
        
        post_name <- post %>% html_text(trim = TRUE)
        print(str_interp('Fetching post ${post_name} url: ${url2}'))
        
        tryCatch(expr = {
          td2 <- read_html(url2)
          comments <- td2 %>% html_nodes(".body")
          
          for (comment in comments[-1]) {
            # Extract data.
            upvotes <- comment %>% html_node(".positive span") %>% html_text(trim = TRUE)
            downvotes <- comment %>% html_node(".negative span") %>% html_text(trim = TRUE)
            text <- comment %>% html_node(".content") %>% html_text(trim = TRUE)
            datetime <- comment %>% html_node("time") %>% xml_attr("datetime")
            
            # Append record.
            records <- append(records, list(data_frame(date = datetime, text = text, upvotes = upvotes, downvotes = downvotes)))
          }
        }, error = function(e) {
          print(str_interp('Could not find ${url2}'))
        })
      }
    }
  }
}

df <- bind_rows(records)
df$date <- ymd_hms(df$date)
df$upvotes <- as.integer(df$upvotes)
df$downvotes <- as.integer(df$downvotes)

write_csv(df, 'td_data.csv')
