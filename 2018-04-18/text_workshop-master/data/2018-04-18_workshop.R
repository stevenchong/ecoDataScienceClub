library(tidyverse)
library(stringr)

coral_spp_narratives <- read_csv('iucn_narratives.csv')
View(coral_spp_narratives)

coral_spp_info <- read_csv('coral_spp_info.csv')
View(coral_spp_info)


coral_habs_raw <- coral_spp_narratives %>%
		left_join(coral_spp_info, by = "iucn_sid") %>%
		select(iucn_sid, sciname, habitat)

coral_habs_raw$habitat[1]

x <- "Everybody's got something to hide except for me and my monkey"

tools::toTitleCase(x) 
tolower(x)
str_split(x, 'hide')
str_replace(x, 'except for' , 'including')
str_replace_all(x, " ", '_')
str_detect(x, 't')
str_match(x, 't')
str_match_all(x, 't')
str_extract(x, 't')
str_extract_all(x, 't')
str_locate_all(x, 't')


coral_habs <- coral_habs_raw %>%
	mutate(hab_cut = str_split(habitat, '\\. ') ) %>%
	unnest(hab_cut)	%>%
	filter(str_detect(hab_cut, '1') |str_detect(hab_cut, '[0-9]'))

coral_depth <- coral_habs %>%
		filter (str_detect(hab_cut, '[0-9] m')) %>%
		mutate (depth = str_extract( hab_cut, '[0-9-]+ m')) %>%
		mutate (depth_num = str_split(depth, "[^0-9]")) %>%
		unnest(depth_num) %>%
		filter(depth_num != '') %>%
		mutate(depth_num = as.numeric(depth_num) )

coral_habs$hab_cut[1]


years <- coral_habs %>%
		mutate(year = str_extract(hab_cut, '[0-9]{4}'))

coral_threats <- coral_spp_narratives %>%
		select(iucn_sid, threats) %>%
		mutate (threats = tolower(threats),
						threats_cut = str_split(threats, '\\. ')) %>%
		unnest(threats_cut) %>%
		filter(str_detect(threats_cut, '^a.*n$'))

crappy_colname <- 'Per-capita income ($US) (2015 dollars)'
tolower(crappy_colname) %>%
		str_replace_all('[^a-z0-9]+', '_')

y <- 'one fish two fish red fish blue fish'

y %>% str_locate('(?<=two) fish')
y %>% str_locate('fish ? (?=blue)')

