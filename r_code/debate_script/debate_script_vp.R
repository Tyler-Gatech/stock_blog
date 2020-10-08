#Transcript Analysis
library(rvest)
library(stringr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(gganimate)
library(gifski)
library(extrafont)
library(zoo)
library(transformr)
library(rvest)

script_url <- "https://www.rev.com/blog/transcripts/kamala-harris-mike-pence-2020-vice-presidential-debate-transcript"

html <- read_html(script_url)
View( html%>% 
        html_text())

#define patterns for each speaker
sp <- "Susan Page\\s?: \\(([0-9]{2}:)?[0-9]{2}:[0-9]{2}\\)"
kh <- "Kamala Harris\\s?: \\(([0-9]{2}:)?[0-9]{2}:[0-9]{2}\\)"
mp <- "Mike Pence\\s?: \\(([0-9]{2}:)?[0-9]{2}:[0-9]{2}\\)"

time_pattern <- "([0-9]{2}:)?[0-9]{2}:[0-9]{2}" #find 6 digits
hour_pattern <- "[0-9]{2}(?=:[0-9]{2}:[0-9]{2})" #find two digits preceded by 4 digit
min_pattern <- "[0-9]{2}(?=:[0-9]{2}$)" #Find the two digits before the last two digits
sec_pattern <-  "[0-9]{2}$" #Find the last two digits 


#Susan Page time
sp_time <- html%>% 
  html_text() %>% 
  str_extract_all(sp) %>% 
  unlist() %>% 
  str_extract_all(time_pattern) %>% 
  unlist() %>% 
  tibble(time = .) %>% 
  mutate(speaker = 'sp',
         count_ = row_number(), 
         hour_ = as.numeric(str_extract(time, hour_pattern)), 
         min_ = as.numeric(str_extract(time, min_pattern)), 
         sec_ = as.numeric(str_extract(time, sec_pattern)))%>% 
  mutate(hour_ = replace_na(hour_, 0))%>% 
  #the html is repeating , so i only want to grab single instance
  distinct(time, .keep_all = T) %>% 
  #there is an error in the transcript time, need to add 36 minutes after the first 46 turns for susan page
  mutate(min_ = if_else(count_ >= 46, min_ + 36, min_),
         real_time = hour_ * 3600 + min_*60 + sec_ ) 
#use View() to find the error
View(sp_time)


#Mike Pence
mp_time <- html%>% 
  html_text() %>% 
  str_extract_all(mp) %>% 
  unlist() %>% 
  str_extract_all(time_pattern) %>% 
  unlist() %>% 
  tibble(time = .) %>% 
  mutate(speaker = 'mp',
         count_ = row_number(), 
         hour_ = as.numeric(str_extract(time, hour_pattern)), 
         min_ = as.numeric(str_extract(time, min_pattern)), 
         sec_ = as.numeric(str_extract(time, sec_pattern)))%>% 
  mutate(hour_ = replace_na(hour_, 0)) %>% 
  #the html is repeating , so i only want to grab single instance
  distinct(time, .keep_all = T) %>% 
  #there is an error in the transcript time, need to add 36 minutes after the first 48 turns for Mike Pence
  mutate(min_ = if_else(count_ >= 48, min_ + 36, min_),
         real_time = hour_ * 3600 + min_*60 + sec_ ) 
  
View(mp_time)
#Kamala Harris
kh_time <- html%>% 
  html_text() %>% 
  str_extract_all(kh) %>% 
  unlist() %>% 
  str_extract_all(time_pattern) %>% 
  unlist() %>% 
  tibble(time = .) %>% 
  mutate(speaker = 'kh',
         count_ = row_number(), 
         hour_ = as.numeric(str_extract(time, hour_pattern)), 
         min_ = as.numeric(str_extract(time, min_pattern)), 
         sec_ = as.numeric(str_extract(time, sec_pattern))) %>% 
  mutate(hour_ = replace_na(hour_, 0)) %>% 
  #the html is repeating , so i only want to grab single instance
  distinct(time, .keep_all = T) %>% 
  #there is an error in the transcript time, need to add 25 minutes after the first 24
  mutate(min_ = if_else(count_ >= 44, min_ + 36, min_),
                real_time = hour_ * 3600 + min_*60 + sec_ ) 
  
View(kh_time)

#union all and sort by real time and calc the speaking length
all_time <- 
  sp_time %>% 
  union_all(mp_time) %>% 
  union_all(kh_time) %>% 
  arrange(real_time) %>% 
  mutate(diff_next = -(real_time - lead(real_time, default = first(real_time)))) %>% 
  mutate(next_speak = lead(speaker, default =first(speaker)))

#eventually, we should clean up that if the next speaker is the same person, then eliminate the rows

#Adding time stamp
all_time <- all_time %>% 
  mutate(time_stamp = ymd_hms("2020-10-07 09:00:00", tz = "EST") + dseconds(real_time), 
         time_stamp_5 = ymd_hms("2020-10-07 09:00:00", tz = "EST") + dseconds(floor(real_time/300)*300))

#Let's plot a few simple visuals
View(all_time)

#ordering for chart
all_time$speaker <- factor(all_time$speaker,levels = c("sp", "mp", "kh"))

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
                   labels=c("Susan Page","Mike Pence","Kamala Harris")) +
  scale_fill_manual(values = c("grey", "red", "blue")) +
  theme_bw() +
  ggtitle("Number of Turns Speaking")+
  theme(legend.position = "none")

View(all_time)

#average speaking time?
all_time %>% 
  #removing the last row from susan Page, has huge negative
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
                   labels=c("Susan Page","Mike Pence","Kamala Harris")) +
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
  geom_text(x = 1000, y = 100, label = paste0(speak_turns$char_time, " EST"), 
            check_overlap = T, size = 10, family = "mono") +
  #annotate(geom ="text",x = 500, y = 1000, label = ifelse(speak_turns$speaker == "dt", speak_turns$char_time,"")) +
  #  geom_label(aes("8:00")) +
  scale_color_manual(values = c("grey", "red", "blue"),
                     labels = c("Moderator", "Mike Pence", "Kamala Harris")) +
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
  ggtitle("# of Turns Speaking") +
  ylab("") +
  xlab("") +
  scale_x_discrete(breaks=c(0,2000, 4000, 6000))

animate(p_time +
          transition_reveal(real_time), fps = 15, duration = 10)


setwd("./Desktop/websites/stock_blog/img/debate_vp_2020")
anim_save("vp_turns_animated.gif")

#plot total amount of speaking thorughout the evening
speak_length <- all_time %>% 
  filter(diff_next >= 0) %>% 
  group_by(speaker, real_time,time_stamp_5) %>% 
  summarize(speak_time = sum(diff_next)) %>% 
  rowwise() %>% 
  mutate(char_time = str_extract(time_stamp_5, "[0-9]{2}:[0-9]{2}")) %>% 
  ungroup() %>% 
  #the goal for this next section is within each 5 minute interval to calc who has the most speaking time
  #and by how much 
  group_by(speaker) %>% 
  mutate(speak_time_running = cumsum(speak_time)) %>% 
  ungroup() %>% 
  group_by(speaker, time_stamp_5) %>% 
  mutate(max_seconds = max(speak_time_running)) %>% 
  ungroup() %>% 
  filter(speaker != "sp") %>%   #filterint out sp for this piece, will union back in
  #grab mp and kh time for each segment, fill up down 
  mutate(mp_time = ifelse(speaker == "mp", max_seconds, NA), 
         kh_time = ifelse(speaker == "kh", max_seconds, NA)) %>% 
  arrange(time_stamp_5, speaker, real_time) %>% 
  fill(mp_time, kh_time) %>% 
  mutate(mp_time = replace_na(mp_time, 0)) %>% 
  mutate(kh_time = replace_na(kh_time, 0)) %>% 
  #this is just to fix some of the dragging, i only want 1 value per 5 min interval for each speaker
  group_by(time_stamp_5) %>% 
  mutate(mp_time = max(mp_time), 
         kh_time = max(kh_time)) %>% 
  ungroup() %>% 
  mutate(leader = if_else(mp_time > kh_time, "mp", "kh"), 
         lead_time = if_else(mp_time > kh_time, mp_time - kh_time, kh_time - mp_time)) %>% 
  rowwise() %>% 
  mutate(kh_lead = max(kh_time - mp_time, 0),
         mp_lead = max(mp_time - kh_time, 0)) %>% 
  ungroup() %>% 
  mutate(
         max_lead_time = max(lead_time), 
         min_lead_time = min(lead_time), 
         scale_lead = lead_time/max_lead_time * 1400 + 1200, 
         scale_lead_kh = kh_lead/max_lead_time * 1400 + 1200,
         scale_lead_mp = mp_lead/max_lead_time * 1400 + 1200, 
         mp_lead_time_char = if_else(mp_lead == 0 ,"", paste0(floor(mp_lead/60),":",str_pad(mp_lead%%60,2, pad = 0))),
         kh_lead_time_char = if_else(kh_lead == 0 ,"", paste0(floor(kh_lead/60),":",str_pad(kh_lead%%60,2, pad = 0)))
         )


View(speak_length %>% 
       arrange(real_time))


p_time <- 
  speak_length %>%
  # group_by(speaker) %>% 
  # mutate(speak_time_running = cumsum(speak_time)) %>%  #drag the speaking counts
  ggplot() +
  geom_line(aes(x = real_time, y = speak_time_running, color = speaker), size =1) +
  geom_text(x = 1000, y = 2200, label = "Running Time Difference", check_overlap = T) + 
  geom_text(x = 500, y = 1800, label = paste0(speak_length$mp_lead_time_char, ""), 
           check_overlap = T, size = 8, family = "mono") +
  geom_text(x = 500, y = 1900, label = paste0(speak_length$kh_lead_time_char, ""), 
            check_overlap = T, size = 8, family = "mono") +
  #annotate(geom ="text",x = 500, y = 1000, label = ifelse(speak_turns$speaker == "dt", speak_turns$char_time,"")) +
  #  geom_label(aes("8:00")) +
  scale_color_manual(values = c("blue", "blue", "red", "red"),
                     labels = c("Mike Pence", "Kamala Harris")) +
  geom_segment(aes(x = 1200, y = 1900, xend = scale_lead_kh, yend = 1900, color = "blue", size = 2)) +
  geom_segment(aes(x = 1200, y = 1800, xend = scale_lead_mp, yend = 1800, color = "red", size = 2)) +
  #geom_segment(aes(x = 1200, y = 1900, xend = 2600, yend = 1900, color = "blue", size = 2, alpha = 0.3))+
  #geom_point(aes(x = 750, y = 1900, shape = 18, size  = 3)) + 
  theme(
    panel.background = element_rect(fill = "#eaeaea"),
    panel.border = element_blank(),
    panel.grid.major.y = element_line(size = 0.1, color = "black"),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#eaeaea"),
    plot.title = element_text(size = 20, family = "mono", hjust = 0.5),
    legend.title=element_blank(),
    legend.background = element_rect(fill = "#eaeaea"),
    legend.position = "none",
    axis.title.x = element_text(family = "mono"),
    axis.title.y = element_text(color="#993333", size=14, face="bold"),
    axis.ticks = element_blank()
  ) + 
  #annotate("text", x = 1500, y = 535, label = paste0("Actual Time: 10:50", time_stamp_5), family =("mono")) +
  ggtitle("Total Time Speaking") +
  ylab("") +
  xlab("") +
  scale_x_discrete(breaks=c(0,2000, 4000, 6000))


#For production
animate(p_time +
         transition_reveal(real_time), fps = 15, duration = 10)

#For testing
animate(p_time + 
          transition_reveal(real_time), fps = 5, duration = 4)



#try 2 with smoother
p_time2 <- 
  speak_length %>%
  group_by(time_stamp_5, speaker) %>% 
    mutate(rank_row = rank(real_time)) %>% 
    ungroup() %>% 
    filter(rank_row ==1) %>% 
  # group_by(speaker) %>% 
  # mutate(speak_time_running = cumsum(speak_time)) %>%  #drag the speaking counts
  ggplot() +
  geom_line(aes(x = real_time, y = speak_time_running, color = speaker, group = NA), size =1) +
  #geom_text(x = 1500, y = 1800, label = paste0(speak_turns$char_time, " EST"), 
  #         check_overlap = T, size = 10, family = "mono") +
  #annotate(geom ="text",x = 500, y = 1000, label = ifelse(speak_turns$speaker == "dt", speak_turns$char_time,"")) +
  #  geom_label(aes("8:00")) +
  scale_color_manual(values = c("red", "blue", "black"),
                     labels = c("Mike Pence", "Kamala Harris")) +
  geom_segment(aes(x = 800, y = 1900, xend = scale_lead_kh, yend = 1900, color = "blue")) +
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

speak_length %>%
  group_by(time_stamp_5, speaker) %>% 
  mutate(rank_row = rank(real_time)) %>% 
  ungroup() %>% 
  filter(rank_row ==1) %>% 
  View()

p_time2 +
  transition_states(states = time_stamp_5, transition_length = 2, state_length = 1) + 
  ease_aes('linear')+
  enter_fade()+
  exit_fade()




anim_save("vp_time_animated.gif")



