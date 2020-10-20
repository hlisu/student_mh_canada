# Load the data dictionary and the raw data and correct the variable names
# Both labels.txt and dictionary.txt are adapted from STATA file.
raw_data <- read_csv("AAmSmnfd.csv") 
dict <- read_lines("dictionary.txt", skip = 18) # skip is because of preamble content
# Now we need the labels because these are the actual responses that we need
labels_raw <- read_file("labels.txt")


#### Set-up the dictionary  ####
# What we want is a variable name and a variable definition
variable_descriptions <- as_tibble(dict) %>% 
  filter(value!="}") %>% 
  mutate(value = str_replace(value, ".+%[0-9].*f[ ]{2,}", "")) %>% 
  mutate(value = str_remove_all(value, "\"")) %>% 
  rename(variable_description = value) %>% 
  bind_cols(tibble(variable_name = colnames(raw_data)[-1]))

# Now we want a variable name and the possible values
labels_raw_tibble <- as_tibble(str_split(labels_raw, ";")[[1]]) %>% 
  filter(row_number()!=1) %>% 
  mutate(value = str_remove(value, "\nlabel define ")) %>% 
  mutate(value = str_replace(value, "[ ]{2,}", "XXX")) %>% 
  mutate(splits = str_split(value, "XXX")) %>% 
  rowwise() %>% 
  mutate(variable_name = splits[1], cases = splits[2]) %>% 
  mutate(cases = str_replace_all(cases, "\n [ ]{2,}", "")) %>%
  select(variable_name, cases) %>% 
  drop_na()

# Now we have the variable name and the different options e.g. age and 0-9, 10-19, etc.
labels_raw_tibble <- labels_raw_tibble %>% 
  mutate(splits = str_split(cases, "[ ]{0,}\"[ ]{0,}"))

# The function sets up the regex (I know, I know, but eh: https://xkcd.com/208/)
add_cw_text <- function(x, y){
  if(!is.na(as.numeric(x))){
    x_new <- paste0(y, "==", x,"~")
  }
  else{
    x_new <- paste0("\"",x,"\",")
  }
  return(x_new)
}

# The function will be in the row, but it'll get the job done
cw_statements <- labels_raw_tibble %>% 
  rowwise() %>% 
  mutate(splits_with_cw_text = list(modify(splits, add_cw_text, y = variable_name))) %>% 
  mutate(cw_statement = paste(splits_with_cw_text, collapse = "")) %>% 
  mutate(cw_statement = paste0("case_when(", cw_statement,"TRUE~\"NA\")")) %>% 
  mutate(cw_statement = str_replace(cw_statement, ",\"\",",",")) %>% 
  select(variable_name, cw_statement)
# So for every variable we now have a case_when() statement that will convert 
# from the number to the actual response.

# Just do some finally cleanup of the regex.
cw_statements <- 
  cw_statements %>% 
  mutate(variable_name = str_remove_all(variable_name, "\\r")) %>% 
  mutate(cw_statement = str_remove_all(cw_statement, "\\r"))

#### Apply that dictionary to the raw data ####
# Pull out a bunch of variables and then apply the case when statement for the categorical variables
gss <- raw_data %>%
  select(CASEID, 
         srh_115,
         agegr10,
         sex,
         immstat,
         mar_110,
         vismin,
         ddis_fl) %>%
  mutate_at(vars(srh_115:ddis_fl), .funs = funs(ifelse(.>=96, NA, .))) %>% 
  mutate_at(.vars = vars(srh_115:ddis_fl),
            .funs = funs(eval(parse(text = cw_statements %>%
                                      filter(variable_name==deparse(substitute(.))) %>%
                                      select(cw_statement) %>%
                                      pull()))))
# rename the variables 
gss <- gss %>%
  clean_names() %>%
  rename(self_rated_mental_health = srh_115,
         age_old = agegr10,
         immigration_status_cat = immstat,
         main_activity_past_year = mar_110,
         visible_minority_cat = vismin,
         disability_status = ddis_fl)

#### Clean up ####
gss <- gss %>% 
  mutate_at(vars(self_rated_mental_health:disability_status), 
            .funs = funs(ifelse(.=="Valid skip"|.=="Refusal"|.=="Don't know"|.=="Not stated", "NA", .))) 


dis_sta <- raw_data %>%
  mutate(disability_status = case_when(
    dhea_fl==1 ~ "Other Disability",
    dphy_fl==1 ~ "Other Disability",
    dmen_fl==1 ~ "Mental/Psy/Learning Disability",
    dvis_fl==1 ~ "Other Disability",
    dunk_fl==1 ~ "Other Disability",
    dcog_fl==1 ~ "Mental/Psy/Learning Disability",
    ddis_fl==2 ~ "Other situation",
    TRUE ~ "Other situation")) %>%
  select(disability_status) %>%
  pull()

vis_min <- gss %>%
  mutate(visible_minority = case_when(
    visible_minority_cat == "Not a visible minority" ~ 0,
    visible_minority_cat == "Visible minority" ~ 1)) %>%
  select(visible_minority) %>%
  pull()

imm_sta <- gss %>% 
  mutate(immigration_status = case_when(
    immigration_status_cat == "Immigrant" ~ 1L,
    immigration_status_cat == "Non immigrant" ~ 0L
  )) %>%
  select(immigration_status) %>%
  pull()

men_hea <- raw_data %>%
  mutate(self_rated_mental_health = case_when(
    srh_115 == 1 ~ 1L,
    srh_115 == 2 ~ 1L,
    srh_115 == 3 ~ 1L,
    srh_115 == 4 ~ 0L,
    srh_115 == 5 ~ 0L,
    TRUE~ NA_integer_)) %>%
  select(self_rated_mental_health) %>%
  pull()

age_grp <- gss %>%
  mutate(age = case_when(
    age_old == "15 to 24 years" ~ "15-24",
    age_old == "25 to 34 years" ~ "25-34",
    age_old == "35 to 44 years" ~ "35-44",
    age_old == "45 to 54 years" ~ "45-54",
    age_old == "55 to 64 years" ~ "55-64",
    age_old == "65 to 74 years" ~ "65-74",
    age_old == "75 years and over" ~ "75+")) %>%
  select(age) %>%
  pull()

gss <- gss %>% mutate(disability_status = dis_sta, 
                      self_rated_mental_health = men_hea,
                      visible_minority = vis_min,
                      immigration_status = imm_sta,
                      age = age_grp) %>% select(-age_old, -immigration_status_cat, -visible_minority_cat)

write.csv(gss, "gss.csv")