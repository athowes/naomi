devtools::load_all("C:/Users/esra/Documents/GitHub/naomi")
library(ggplot2)
library(sf)
library(cowplot)
library(tidyverse)

#' ## 1. Load demo data and subset
filter <- c("MWI_4_8_demo","MWI_4_9_demo","MWI_4_10_demo","MWI_4_11_demo",
            "MWI_4_12_demo", "MWI_2_2_demo")

areas <- read_sf(system.file("extdata/demo_areas.geojson", package = "naomi"))%>%
  dplyr::filter(area_id %in% filter)

ggplot() +
  geom_sf(data = areas %>% filter(area_id != "MWI_2_2_demo"),
          colour = NA, aes(fill = area_id)) +
  geom_sf(data = areas %>% filter(area_id == "MWI_2_2_demo"),
          colour = "black", fill = NA)


area_merged <- areas %>%
  mutate(area_id = gsub("MWI_2_2_demo", "MWI", area_id),
         area_level = gsub(2, 0, area_level),
         area_level = gsub(4, 1, area_level),
         area_level = as.numeric(area_level),
         parent_area_id = case_when(area_level == 1 ~ "MWI"),
         area_sort_order = 1:6,
         area_level_label = recode(area_level,
                                   `0` = "Country",
                                   `1` = "District + Metro"))

keep <- c("MWI_4_8_demo","MWI_4_9_demo","MWI_4_10_demo","MWI_4_11_demo",
          "MWI_4_12_demo", "MWI")

pop_agesex <- read_csv(system.file("extdata/demo_population_agesex.csv", package = "naomi")) %>%
  filter(area_id %in% keep)

survey_hiv_indicators <- read_csv(system.file("extdata/demo_survey_hiv_indicators.csv", package = "naomi")) %>%
  filter(area_id %in% keep)

art_number <- read_csv(system.file("extdata/demo_art_number.csv", package = "naomi")) %>%
  filter(area_id %in% keep)

anc_testing <- read_csv(system.file("extdata/demo_anc_testing.csv", package = "naomi")) %>%
  filter(area_id %in% keep)

pjnz <- system.file("extdata/demo_mwi2019.PJNZ", package = "naomi")
spec <- extract_pjnz_naomi(pjnz)



#' ## 2. Options and data
scope <- "MWI"
level <- 1
calendar_quarter_t1 <- "CY2016Q1"
calendar_quarter_t2 <- "CY2018Q3"
calendar_quarter_t3 <- "CY2019Q4"

prev_survey_ids  <- c("DEMO2016PHIA", "DEMO2015DHS")
artcov_survey_ids  <- "DEMO2016PHIA"
vls_survey_ids <- NULL
recent_survey_ids <- "DEMO2016PHIA"

artnum_calendar_quarter_t1 <- "CY2016Q1"
artnum_calendar_quarter_t2 <- "CY2018Q3"

anc_clients_year2 <- 2018
anc_clients_year2_num_months <- 9

anc_prevalence_year1 <- 2016
anc_prevalence_year2 <- 2018

anc_art_coverage_year1 <- 2016
anc_art_coverage_year2 <- 2018

art_attend_t2 <- TRUE


#' ## 4. Prepare model inputs
naomi_mf <- naomi_model_frame(area_merged,
                              pop_agesex,
                              spec,
                              scope = scope,
                              level = level,
                              calendar_quarter_t1,
                              calendar_quarter_t2,
                              calendar_quarter_t3)



naomi_data <- select_naomi_data(naomi_mf,
                                survey_hiv_indicators,
                                anc_testing,
                                art_number,
                                prev_survey_ids,
                                artcov_survey_ids,
                                recent_survey_ids,
                                vls_survey_ids,
                                artnum_calendar_quarter_t1,
                                artnum_calendar_quarter_t2,
                                anc_prevalence_year1,
                                anc_prevalence_year2,
                                anc_art_coverage_year1,
                                anc_art_coverage_year2)

#' 5. Fit model

# Fit model with default ART attending method: area of residence as a pull factor
tmb_inputs_method_attend <- prepare_tmb_inputs(naomi_data)
fit_method_attend <- fit_tmb(tmb_inputs_method_attend)
out_method_attend <- output_package(fit_method_attend, naomi_data)

# Fit model with default ART attending method: area of residence as a push factor
tmb_inputs_method_reside <- prepare_tmb_inputs(naomi_data, artattend_method = "reside")
fit_method_reside <- fit_tmb(tmb_inputs_method_reside)
out_method_reside <- output_package(fit_method_reside, naomi_data)

# colour scheme
cols <- c(MWI_4_10_demo = "#608099", # blue
          MWI_4_11_demo = "#6ea28f", #green
          MWI_4_12_demo = "#eccc2c", #yellow
          MWI_4_8_demo = "#d88431", #orange
          MWI_4_9_demo = "#ca5636") #red


art_attend_summary <- function(outputs, year, fill) {
 outputs$art_attendance %>%
    filter(calendar_quarter == calendar_quarter_t1) %>%
    ggplot(aes(x = tidytext::reorder_within(reside_area_id, -artnum_mode, attend_area_id),
               y = artnum_mode,
               fill = reside_area_id)) +
    geom_col() +
    facet_grid(~attend_area_id,
               scales = "free_x",
               space = "free_x",
               switch = "x") +
    scale_fill_manual(values = fill) +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank())
}

p1 <- art_attend_summary(out_method_attend, calendar_quarter_t1, cols) +
  labs(x = "attend_area_id") +
  ggtitle("Area of residence positively determining attractiveness factor")

p2 <- art_attend_summary(out_method_reside, calendar_quarter_t1, cols) +
  labs(x = "attend_area_id") +
  ggtitle("Area of residence negatively determining attractiveness factor")

map <- ggplot() +
  geom_sf(data = areas %>% filter(area_id != "MWI_2_2_demo"),
          colour = NA, aes(fill = area_id)) +
  scale_fill_manual(values = cols)

cowplot::plot_grid(map, p1, p2, ncol = 1, nrow = 3)
