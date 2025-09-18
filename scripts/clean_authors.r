
# clean raw input file into csv tables

library(here)
library(dplyr)
library(readxl)

fpath <- here("data", "Earth_MRI_Parent_Child_Publications_Citations_wAuthors.xlsx")
shts <- excel_sheets(f_path)

for(sh in shts){
  x <- read_xlsx(fpath, sheet = sh)
  assign(gsub('\\(|\\)', "", gsub(" ","_", tolower(sh))),x)
}
rm(sh,x,shts)

## Author Directory -----------
# seems pretty good already, but lets split the name into first/last
names <- stringr::str_split(author_directory$author_name, ' |, ')
max(unlist(lapply(names, FUN = length))) # max length = 7
min(unlist(lapply(names, FUN = length))) # min length = 1

author_directory$last_name <- unlist(lapply(names, FUN=function(x){x[[1]]}))
author_directory$first_name <- unlist(lapply(names, FUN=function(x){
  if(length(x)>1){ return(gsub("\\.","", x[[2]]))
    }else{ return(NA)
        }}))

author_directory$middle_name <- unlist(lapply(names, FUN=function(x){
  if(length(x)>2){ return(gsub("\\.","", paste(x[3:length(x)], collapse = " ")))
  }else{ return(NA)
  }}))

manual_rev <- author_directory %>% filter(grepl(" ", middle_name))
dir.create(here("data","tables", "manual_review"), recursive = TRUE)
#write.csv(manual_rev, here("data","tables","manual_review","authors.csv"))

# After the manual review, join back to the authors
manual_rev <- read.csv(here("data","tables","manual_review","authors.csv")) %>%
  select(-X) %>% full_join(author_directory, by=c("author_id", "author_name","canonical_key"))%>% 
  mutate(
  last_name = ifelse(is.na(last_name.x), last_name.y, last_name.x),
  first_name = ifelse(is.na(first_name.x), first_name.y, first_name.x),
  middle_name = ifelse(is.na(middle_name.x), middle_name.y, middle_name.x),
  extra = ifelse(extra=="", NA, extra)
) %>% select(-last_name.x, - last_name.y, -first_name.x, -first_name.y,
             -middle_name.x, -middle_name.y) %>% arrange(author_id)

# there are some duplicated authors (using first_name, middle_name, last_name & extra)
# not sure how to combine since they have multiple canonical_key values, leaving as is
# save author table

write.csv(manual_rev, here("data","tables","authors.csv"), row.names = FALSE)


