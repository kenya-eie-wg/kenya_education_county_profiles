library(rmarkdown)
library(knitr)
library(flexdashboard)


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
