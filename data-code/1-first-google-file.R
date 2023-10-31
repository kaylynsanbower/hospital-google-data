# clear
rm(list=ls())

# Import Data -------------------------------------------------------------

# google reviews 
df <- read.csv(file="data/input/google_reviews.csv", header=T, skipNul = T, na.strings=c(""," ","NA"))


# summary of raw data -----------------------------------------------------

# # only needed to review data -- no need to run to get output dataset.
# 
# # rating
# tbl <- table(df$rating)
# tbl <- cbind(tbl,prop.table(tbl))
# tbl <- tbl %>% 
#   as.data.frame() %>%
#   rename(pct_lang = V2) %>% 
#   mutate(pct_lang = round(pct_lang*100,2))
# 
# 
# # language and reviews 
# tbl <- table(df$language)
# tbl <- cbind(tbl,prop.table(tbl))
# tbl <- tbl %>% 
#   as.data.frame() %>%
#   rename(pct_lang = V2) %>% 
#   mutate(pct_lang = round(pct_lang*100,2))



# Prep variables  ---------------------------------------------------------

df <- df %>% 
  mutate(text = ifelse(language=="en", text, NA)) %>% # only keep the reviews that are english, but keep all observations 
  mutate(datetime = anydate(datetime)) %>% # modify date format
  mutate(year = year(datetime)) %>% 
  mutate(hosp_name = str_trim(tolower(hosp_name))) %>%
  mutate(hosp_address = str_trim(tolower(hosp_address))) %>% 
  drop_na(rating)

  

# match hospital number ---------------------------------------------------

hosp <- df %>% 
  subset(select=c(hosp_id, hosp_name, year)) %>% 
  unique() %>% 
  group_by(hosp_name) %>% 
  mutate(mult_profiles = length(unique(hosp_id))) 

# drop the single hospital that has more than one google profile
to_drop <- hosp %>% 
  filter(mult_profiles>1) %>% 
  subset(select=c(hosp_id)) %>% 
  unique()

df <- df %>% filter(!hosp_id %in% c(to_drop$hosp_id))

# only keep hospitals in the list of hospitals that have one profile per year
hosp <- hosp %>%
  filter(mult_profiles==1) %>% 
  subset(select=-c(mult_profiles)) %>% 
  unique()

# create a list of unique hospital names on google 
uniq_hosp <- hosp %>%
  subset(select=c(hosp_name, hosp_id)) %>% 
  unique()


# map AHA data to google reviews ------------------------------------------

# want to have the year, medicare record number, and AHA id all available with the relevant google profile

aha <- read.csv(file="data/input/aha_data.csv", header=T, na.strings=c(""," ","NA"), encoding = "UTF-8") %>%
  subset(select=c(ID, MNAME, year, MCRNUM)) %>% # only keep variables we're currently interested in adding
  unique() %>% 
  drop_na(MCRNUM) %>% # only keep hospitals with valid medicare record numbers
  mutate(MNAME = str_trim(tolower(MNAME))) %>% # get rid of any problematic formatting that could make it hard to match AHA data to reviews
  group_by(MNAME, year) %>% # group by the two variables that we'll use to match AHA to google data 
  mutate(nobs = n()) %>% # if great than 1, we're going to have multiple IDs and MCRNUMs to match to a single hospital/year 
  filter(MNAME %in% c(uniq_hosp$hosp_name)) %>% # limit to hospitals that appear in the google reviews
  group_by(MNAME, year) %>% 
  mutate(mcrnums = length(unique(MCRNUM))) %>% # id hospitals with multiple AHA profiles 
  group_by(MNAME) %>% 
  mutate(mcrnums=max(mcrnums)) %>% 
  filter(mcrnums==1)


# match AHA data to google data 
aha_hosp <- left_join(hosp, aha, by=c("hosp_name"="MNAME", "year"="year")) %>%
  unique() %>% 
  filter(year<max(aha$year)) %>% 
  subset(select=-c(nobs, mcrnums))



# add hospital ID variables to reviews ------------------------------------

df <- left_join(df, aha_hosp, by=c("hosp_name", "year", "hosp_id")) 
  


# export ------------------------------------------------------------------

write.csv(df, file = "data/output/google_reviews_mcrnum.csv", row.names = FALSE)

  
  
  
  
  
  
  
  
  






  