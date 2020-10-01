#Transcript Analysis
library(rvest)
library(stringr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(gganimate)

install.packages("gifski")
library(gifski)

install.packages("extrafont")
library(extrafont)

fonts()

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
cw_time <- html%>% 
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
dt_time <- html%>% 
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
jb_time <- html%>% 
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

#Adding time stamp
all_time <- all_time %>% 
  mutate(time_stamp = ymd_hms("2020-09-29 09:00:00", tz = "EST") + dseconds(real_time), 
         time_stamp_5 = ymd_hms("2020-09-29 09:00:00", tz = "EST") + dseconds(floor(real_time/300)*300))

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
  
#speak turns
speak_turns <- all_time %>% 
  filter(diff_next >= 0) %>% 
  group_by(speaker, real_time, time_stamp_5) %>% 
  summarize(speak_turns = n()) %>% 
  rowwise() %>% 
  mutate(char_time = str_extract(time_stamp_5, "[0-9]{2}:[0-9]{2}")) %>% 
  ungroup()

View(  speak_turns %>%
         group_by(speaker) %>% 
         mutate(speak_turns_running = cumsum(speak_turns)))


#plot total amount of turns speaking thorughout the evening
p_turns <- 
  speak_turns %>%
  group_by(speaker) %>% 
  mutate(speak_turns_running = cumsum(speak_turns)) %>%  #drag the speaking counts
  ggplot() +
  geom_line(aes(x = real_time, y = speak_turns_running, color = speaker), size =1) +
  geom_text(x = 1500, y = 535, label = paste0(speak_turns$char_time, " EST"), 
            check_overlap = T, size = 10, family = "mono") +
#annotate(geom ="text",x = 500, y = 1000, label = ifelse(speak_turns$speaker == "dt", speak_turns$char_time,"")) +
#  geom_label(aes("8:00")) +
  scale_color_manual(values = c("grey", "red", "blue"),
                     labels = c("Moderator", "Donald Trump", "Joe Biden")) +
    theme(
      panel.background = element_rect(fill = "#eaeaea"),
      panel.border = element_blank(),
      panel.grid.major.y = element_line(size = 0.1, color = "black"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#eaeaea"),
      plot.title = element_text(size = 20, family = "mono", hjust = 0.5),
      legend.title=element_blank(),
      legend.background = element_rect(fill = "#eaeaea"),
      legend.position = "bottom",
      axis.title.x = element_text(family = "mono"),
      axis.title.y = element_text(color="#993333", size=14, face="bold"),
      axis.ticks = element_blank()
    ) + 
    #annotate("text", x = 1500, y = 535, label = paste0("Actual Time: 10:50", time_stamp_5), family =("mono")) +
    ggtitle("Presidential Debate: Sep 29 2020 \n # of Turns Speaking") +
    ylab("") +
    xlab("") +
    scale_x_discrete(breaks=c(0,2000, 4000, 6000))

animate(p_turns + 
          transition_reveal(real_time), fps = 20)


setwd("./Desktop/websites/stock_blog/img/debate1_2020")
anim_save("turns_animated.gif")

#plot total amount of speaking thorughout the evening
speak_length <- all_time %>% 
  filter(diff_next >= 0) %>% 
  group_by(speaker, real_time,time_stamp_5) %>% 
  summarize(speak_time = sum(diff_next)) %>% 
  rowwise() %>% 
  mutate(char_time = str_extract(time_stamp_5, "[0-9]{2}:[0-9]{2}")) %>% 
  ungroup()


p_time <- 
speak_length %>%
  group_by(speaker) %>% 
  mutate(speak_time_running = cumsum(speak_time)) %>%  #drag the speaking counts
  ggplot() +
  geom_line(aes(x = real_time, y = speak_time_running, color = speaker), size =1) +
  #geom_text(x = 1500, y = 1800, label = paste0(speak_turns$char_time, " EST"), 
   #         check_overlap = T, size = 10, family = "mono") +
  #annotate(geom ="text",x = 500, y = 1000, label = ifelse(speak_turns$speaker == "dt", speak_turns$char_time,"")) +
  #  geom_label(aes("8:00")) +
  scale_color_manual(values = c("grey", "red", "blue"),
                     labels = c("Moderator", "Donald Trump", "Joe Biden")) +
  theme(
    panel.background = element_rect(fill = "#eaeaea"),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#eaeaea"),
    plot.title = element_text(size = 20, family = "mono", hjust = 0.5),
    legend.title=element_blank(),
    legend.background = element_rect(fill = "#eaeaea"),
    legend.position = "bottom",
    axis.title.x = element_text(family = "mono"),
    axis.title.y = element_text(color="#993333", size=14, face="bold"),
    axis.ticks = element_blank()
  ) + 
  #annotate("text", x = 1500, y = 535, label = paste0("Actual Time: 10:50", time_stamp_5), family =("mono")) +
  ggtitle("Total Time Speaking") +
  ylab("") +
  xlab("") +
  scale_x_discrete(breaks=c(0,2000, 4000, 6000))


animate(p_time + 
          transition_reveal(real_time), fps = 20)

anim_save("time_animated.gif")



