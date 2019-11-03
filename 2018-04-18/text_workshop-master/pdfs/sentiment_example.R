library(tidyverse)
library(stringr)
library(pdftools)
library(tidytext)

got <- pdf_text('got.pdf')

got_df <- data.frame(text = got) %>%
	mutate(page = 1:n()) %>%
	filter(page %in% 6:731)

main_text <- got_df %>%
	mutate(text = str_split(text, '\\n')) %>%
	unnest(text)

chapter_list <- main_text %>%
	group_by(page) %>%
	mutate(ch = ifelse(str_detect(first(text), 'previous.+table of contents'), 
										 nth(text, 2), NA))
	filter(text == first(text)) %>%
	filter(!is.na(ch)) %>%
	ungroup() %>%
	select(ch, page)
	
text_w_ch <- main_text %>%
	select(page, text) %>%
	left_join(chapter_list, by = 'page') %>%
	fill(ch, .direction = 'down') %>%
	mutate(text = tolower(text))

text_words <- text_w_ch %>%
	tidytext::unnest_tokens(output = word, input = text, token = 'words') %>%
	inner_join(sentiments, by = 'word')


	
	