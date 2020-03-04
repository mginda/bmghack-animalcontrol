
data <- interact %>% 
            filter(year(interactions$date)>=2017) %>% 
            group_by(year=year(date), week = week(date), type) %>%
            arrange(type, date)

d <- data %>% dcast(week + type ~ year, length) 

data %>% dcast(type + interaction ~ year, length) %>% 

  
  unique(interactions$interaction)

interactions$v <- 1

inter_ct <- interactions %>% 
  group_by(type) %>% 
  summarize(count = sum(v))



ggplot(inter_ct, aes(y=count,x=interaction)) +
  geom_col(aes(fill=type))




ggplot(animals_s,aes(interactions)) +
  geom_histogram(binwidth=2) +
  facet_wrap(~speciesname)


ggplot(animals_s,aes(speciesname)) +
  geom_bar() 





#OLD

#Some durations have values of NA after calcultion, but are still in the shelter.
#Case: returned or intake animal has not been moved or died
tmp <- interact %>% 
  filter(type %in% c("intake","return") & 
           !(interaction %in% c("DOA","Cremation")) &
           is.na(duration)) %>% 
  select(id,date) %>% 
  mutate(duration=as.numeric(unclass(Sys.Date()-date)))
#Update durations
interact[interact$type %in% c("intake","return") & 
           !(interact$interaction %in% c("DOA","Cremation")) &
           is.na(interact$duration),]$duration <- tmp$duration
#Case: Foster animals are still in the system but not in the shelter
tmp <- interact %>% 
  filter(interaction == c("Foster") &
           is.na(duration)) %>% 
  select(id,date) %>% 
  mutate(duration=as.numeric(unclass(Sys.Date()-date)))