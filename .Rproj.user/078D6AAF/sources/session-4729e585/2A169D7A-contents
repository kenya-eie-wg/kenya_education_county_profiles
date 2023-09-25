library(rmarkdown)
library(tidyverse)
library(knitr)
library(flexdashboard)
library(tinytex)


asal_county_list <- read_csv("./data/counties.csv") %>%  
  mutate(county = recode(county, 
                         "national" = "National",
                         "Tharaka Nithi" = "Tharaka-Nithi")) %>% 
  filter(county != "National") %>% 
  distinct(county) %>% pull()

for (i in asal_county_list) {
  rmarkdown::render(input = "chart_generator.Rmd",
                    params = list(county_selection = i),
                    output_file = paste0("county_profile_", i, ".html"))
}


# Change the folder to the correct one
# The files will be read from here 
html_files <- list.files(path = "C:/Users/seanywng/Documents/R/kenya_education_county_profiles/docs", 
                           recursive = TRUE,  
                           full.names = TRUE)

# Does not work due to flexdashboard formatting
for (file in html_files){
  pagedown::chrome_print(file)
}
