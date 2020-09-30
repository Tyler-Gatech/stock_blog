#Transcript Analysis
library(rvest)
library(stringr)
library(lubridate)
library(tidyverse)
library(ggplot2)

script_url <- "https://www.rev.com/blog/transcripts/donald-trump-joe-biden-1st-presidential-debate-transcript-2020"

html <- read_html(script_url)

#define patterns for each speaker
cw <- "Chris Wallace: \\(([0-9]{2}:)?[0-9]{2}:[0-9]{2}\\)"
dt <- "President Donald J. Trump: \\(([0-9]{2}:)?[0-9]{2}:[0-9]{2}\\)"
jb <- "Vice President Joe Biden: \\(([0-9]{2}:)?[0-9]{2}:[0-9]{2}\\)"

time_pattern <- "([0-9]{2}:)?[0-9]{2}:[0-9]{2}" #find 6 digits
hour_pattern <- "[0-9]{2}(?=:[0-9]{2}:[0-9]{2})" #find two digits preceded by 4 digit
min_pattern <- "[0-9]{2}(?=:[0-9]{2}$)" #Find the two digits before the last two digits
sec_pattern <-  "[0-9]{2}$" #Find the last two digits 


#Chris Wallace time
cw_time <- script%>% 
  html_text() %>% 
  str_extract_all(cw) %>% 
  unlist() %>% 
  str_extract_all(time_pattern) %>% 
  unlist() %>% 
  tibble(time = .) %>% 
  mutate(speaker = 'cw',
         count_ = row_number(), 
         hour_ = as.numeric(str_extract(time, hour_pattern)), 
         min_ = as.numeric(str_extract(time, min_pattern)), 
         sec_ = as.numeric(str_extract(time, sec_pattern))) %>% 
  mutate(hour_ = replace_na(hour_, 0)) %>%
  #there is an error in the transcript time, need to add 25 minutes after the first 24
  mutate(min_ = if_else(count_ >= 44, min_ + 25, min_),
         real_time = hour_ * 3600 + min_*60 + sec_ ) 
  #use View() to find the error

#Donald Trump Time
dt_time <- script%>% 
  html_text() %>% 
  str_extract_all(dt) %>% 
  unlist() %>% 
  str_extract_all(time_pattern) %>% 
  unlist() %>% 
  tibble(time = .) %>% 
  mutate(speaker = 'dt',
         count_ = row_number(), 
         hour_ = as.numeric(str_extract(time, hour_pattern)), 
         min_ = as.numeric(str_extract(time, min_pattern)), 
         sec_ = as.numeric(str_extract(time, sec_pattern))) %>% 
  mutate(hour_ = replace_na(hour_, 0)) %>%
  #there is an error in the transcript time, need to add 25 minutes after the first 24
  mutate(min_ = if_else(count_ >= 74, min_ + 25, min_),
         real_time = hour_ * 3600 + min_*60 + sec_ ) 
  #you could readjust hours and minutes, but we are just calculating the real time, so it has no effect for our purposes
  
#Joe Biden Time
jb_time <- script%>% 
  html_text() %>% 
  str_extract_all(jb) %>% 
  unlist() %>% 
  str_extract_all(time_pattern) %>% 
  unlist() %>% 
  tibble(time = .) %>% 
  mutate(speaker = 'jb',
         count_ = row_number(), 
         hour_ = as.numeric(str_extract(time, hour_pattern)), 
         min_ = as.numeric(str_extract(time, min_pattern)), 
         sec_ = as.numeric(str_extract(time, sec_pattern))) %>% 
  mutate(hour_ = replace_na(hour_, 0)) %>%
  #there is an error in the transcript time, need to add 25 minutes after the first 24
  mutate(min_ = if_else(count_ >= 64, min_ + 25, min_),
         real_time = hour_ * 3600 + min_*60 + sec_ )
  #you could readjust hours and minutes, but we are just calculating the real time, so it has no effect for our purposes

#union all and sort by real time and calc the speaking length
all_time <- 
  cw_time %>% 
  union_all(dt_time) %>% 
  union_all(jb_time) %>% 
  arrange(real_time) %>% 
    mutate(diff_next = -(real_time - lead(real_time, default = first(real_time)))) %>% 
    mutate(next_speak = lead(speaker, default =first(speaker)))

#eventually, we should clean up that if the next speaker is the same person, then eliminate the rows


#Let's plot a few simple visuals

#who spoke the most?
all_time %>% 
  group_by(speaker) %>% 
  count() %>%
  arrange(-n) %>% 
  ggplot(aes(x = speaker, y = n, fill = speaker)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = n), 
            nudge_y = 20, 
            fill = "white") +
  scale_x_discrete("Speaker",
                   labels=c("Chris Wallace","Donald Trump","Joe Biden")) +
  scale_fill_manual(values = c("grey", "red", "blue")) +
  theme_bw() +
  ggtitle("Number of Turns Speaking")+
  theme(legend.position = "none")

#average speaking time?
all_time %>% 
  #removing the last row from chris wallace, has huge negative
  filter(diff_next >= 0) %>% 
  group_by(speaker) %>% 
  summarise(speak_ct = n(), 
            speak_length = sum(diff_next)) %>% 
  mutate(avg_time = speak_length / speak_ct)%>% 
  ggplot(aes(x = speaker, y = avg_time, fill = speaker)) + 
  geom_bar(stat = "identity") + 
  geom_label(aes(label = round(avg_time,2)), 
             nudge_y = 1, 
             fill = "white") +
  scale_x_discrete("Speaker",
                   labels=c("Chris Wallace","Donald Trump","Joe Biden")) +
  scale_fill_manual(values = c("grey", "red", "blue")) +
  theme_bw() +
  ggtitle("Average Speaking Length (in seconds)") +
  theme(legend.position = "none")
  
#create a grid of all possible times
time_grid <- rep(seq(1,max(all_time$real_time)),3)
speaker_grid <- c(rep("cw",max(all_time$real_time)),
                  rep("dt",max(all_time$real_time)),
                  rep("jb",max(all_time$real_time)))

full_grid <- tibble(
  time = time_grid, 
  speaker = speaker_grid
)

#plot number of TURNS spoken throughout the evening
#calc the turns by second throughout the evening
speak_times <- all_time %>% 
  group_by(speaker, real_time) %>% 
  summarize(count = n()) 
  
full_grid2 <- full_grid %>% 
left_join(speak_times, by = c("speaker" = "speaker", "time" = "real_time")) %>% 
  arrange(time, speaker) %>% 
  mutate(count = replace_na(count,0)) %>% 
  group_by(speaker) %>% 
  mutate(count_running = cumsum(count)) #drag the speaking counts

#plot by second as line 
full_grid2 %>% 
  ggplot(aes(x = time, y = count_running, group = speaker, color = speaker)) +
  geom_line() +
  scale_color_manual(values = c("grey", "red", "blue"),
                     labels = c("Chris Wallace", "Donald Trump", "Joe Biden")) +
  theme_bw() +
  ggtitle("Total Speaking Turns")+
  xlab("Actual Debate Time (in seconds)")


#same as above, except with seconds
#plot number of SECONDS spoken throughout the evening
#calc the turns by second throughout the evening
speak_length<- all_time %>% 
  filter(diff_next >= 0) %>% 
  group_by(speaker, real_time) %>% 
  summarize(speak_time = sum(diff_next)) 

full_grid3 <- full_grid %>% 
  left_join(speak_length, by = c("speaker" = "speaker", "time" = "real_time")) %>% 
  arrange(time, speaker) %>% 
  mutate(speak_time = replace_na(speak_time,0)) %>% 
  group_by(speaker) %>% 
  mutate(speak_time_running = cumsum(speak_time)) #drag the speaking counts

#plot by second as line 
full_grid3 %>% 
  ggplot(aes(x = time, y = speak_time_running, group = speaker, color = speaker)) +
  geom_line() +
  scale_color_manual(values = c("grey", "red", "blue"),
                     labels = c("Chris Wallace", "Donald Trump", "Joe Biden")) +
  theme_bw() +
  ggtitle("Total Speaking Length (in seconds)") +
  xlab("Actual Debate Time (in seconds)")



#plot total amount of speaking thorughout the evening






