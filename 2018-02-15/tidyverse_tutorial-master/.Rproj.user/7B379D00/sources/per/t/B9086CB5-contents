#February 15, 2018 eco data science club
library(tidyverse)

rawdat <- read_csv("UNghgdata.csv")

dim(rawdat)

cropdat <- rawdat[1:28]

savecols <- data.frame(cropdat$Party, cropdat$'Last Inventory Year (2015)')

names(savecols) <- c('Party', '2015')

savecols$rank2015 <- rank(-savecols$'2015') # - in rank is for descending order

top10df <- savecols[savecols$rank2015 <= 10, ]

basedat <- cropdat[cropdat$Party %in% top10df$Party,]

## Start Tidy code


dropcols <- select(rawdat, 1, 3:28) 

namecols <- rename(dropcols, '2015' = 'Last Inventory Year (2015)')

tidydat <- gather(namecols, key = 'year', 'emissions', -1) # - 1 gets rid of first column

groupdat <- group_by(tidydat, year) #gives you top 10 emissions per year
top10s <- top_n(groupdat, 10, emissions)
top10_2015 <- filter(top10s, year ==2015)

plotdat <- filter(ungroup(groupdat), Party %in% top10_2015$Party)

ghgplot <- ggplot(plotdat, aes(x=year, y=emissions, color=Party, group=Party)) +
  geom_point() +
  geom_line()

%>%  # passing in thing on the left to the thing in the right
  
sample(1:1e8, 10) %>% mean()

dat2015 <- rawdat %>% 
  select(1, 3:28) %>% 
  rename("2015" = 'Last Inventory Year (2015)') %>% 
  gather(key = 'year', value = 'emissions', -1) %>% 
  group_by(year) %>%
  mutate(annualrank = rank(-emissions)) %>% 
  filter(year == 2015, annualrank <= 10)


pipedat <- rawdat %>% 
  select(1, 3:28) %>% 
  rename("2015" = 'Last Inventory Year (2015)') %>% 
  gather(key = 'year', value = 'emissions', -1) %>% 
  filter(Party %in% dat2015$Party)

pipedat %>% ggplot( aes(x=year, y=emissions, color=Party, group=Party)) +
  geom_point() +
  geom_line()



fakeGDP <- data.frame(party = unique(pipedat$Party), fakeGDP = sample(1e4:1e5, length(unique(pipedat$Party)))) %>% 
#  rename(Party = "unique.pipedat.Party.", fakeGDP = 'sample.10000.1e.05..length.unique.pipedat.Party...')

joindat <- pipedat %>% 
  left_join(fakeGDP, by = "Party")

