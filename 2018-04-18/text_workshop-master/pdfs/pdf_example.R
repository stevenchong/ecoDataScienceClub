library(tidyverse)
library(stringr)
library(pdftools)

pdf_smith <- pdf_text('smith_wilen_2003.pdf')

smith_df <- data.frame (text = pdf_smith) %>%
	mutate(page = 1:n()) %>%
	mutate(text_sep = str_split(text, '\\n')) %>%
	unnest() %>%
	group_by(page) %>%
	mutate(line = 1:n()) %>%
	ungroup()

column_labels <- c('n_patches', paste0('y', 1988:1999))
table1_df <- smith_df %>%
	filter (page == 8 & line %in% 8:18) %>%
	separate(text_sep, column_labels, ' +') %>%
	select(-text, -page, -line)