# map https://medium.com/@traffordDataLab/lets-make-a-map-in-r-7bd1d9366098
# and https://medium.com/@traffordDataLab/pushing-the-boundaries-with-the-open-geography-portal-api-4d70637bddc3
# https://geoportal.statistics.gov.uk/datasets/local-authority-districts-april-2019-boundaries-uk-bfc/data?where=UPPER(lad19nm)%20like%20%27%25SALFORD%25%27
# get boundaries and join by name there
# england only boundaries for CCG http://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2017-full-clipped-boundaries-in-england-v4

# libraries ---------------------------------------------------------------

library(tidyverse)
library(tidyr)  ## devtools::install_github("tidyverse/tidyr")
library(httr)
library(jsonlite)
library(sf)
library(rcartocolor)
library(ggthemes)
library(cowplot)
library(lubridate)
library(ggforce)
library(ggrepel)
library(scales)
library(glue)
library(patchwork)
library(Cairo)
library(extrafont)

# get data ----------------------------------------------------------------

# either
# download.file("https://github.com/nhs-r-community/NHSRdatasets/blob/master/data/ae_attendances.RData?raw=true",
#               destfile = "./ae_attendances.RData")
# load("./ae_attendances.RData")

# or
load(url("https://github.com/nhs-r-community/NHSRdatasets/blob/master/data/ae_attendances.RData?raw=true"))

# via org_code take hospital info through Organisation Endpoint from the ODS API https://digital.nhs.uk/services/organisation-data-service/guidance-for-developers/organisation-endpoint
ods_codes <- unique(ae_attendances$org_code)

path <- "https://directory.spineservices.nhs.uk/ORD/2-0-0/organisations/"

list_api_req <- list()

for (i in seq_along(ods_codes)) {
  
request <- GET(url = paste0(path, ods_codes[i]))
response <- content(request, as = "text", encoding = "UTF-8")
df_org <- fromJSON(response, flatten = TRUE)
df_org <- enframe(unlist(df_org)) 
df_org <- df_org[grep("PostCode|Town|(Organisation\\.OrgId\\.extension)|(Organisation\\.Name)", df_org$name), ]
df_org$name <- c("name", "org_code", "town", "postcode")
list_api_req[[i]] <- df_org

}

tidy_ords <- bind_rows(list_api_req) %>% 
  mutate(ind = rep(1:(nrow(.)/4), each = 4)) %>%
  pivot_wider(names_from = "name", values_from = "value") %>% 
  select(-ind)

ae_api <- left_join(ae_attendances, tidy_ords, by = c("org_code"))
ae_api <- modify_if(ae_api, is.character, ~ tolower(.))

sf_gb <- st_read("https://opendata.arcgis.com/datasets/604fc1fbe022460eaac4758c7521c3e7_0.geojson", stringsAsFactors = FALSE)
sf_gb <- modify_if(sf_gb, is.character, ~ tolower(.))
 
# post code local authority code lookup https://opendata.camden.gov.uk/Maps/National-Statistics-Postcode-Lookup-UK/tr8t-gqz7/data
# using API https://dev.socrata.com/foundry/opendata.camden.gov.uk/tr8t-gqz7

ae_api$postcode <- toupper(ae_api$postcode)

ae_pcodes <- unique(ae_api$postcode)

path_lu <- "https://opendata.camden.gov.uk/resource/tr8t-gqz7.json?postcode_3="

list_lu_req <- list()

for (i in seq_along(ae_pcodes)) {

  request <- GET(url = paste0(path_lu, sub(" ", "%20", ae_pcodes[i])))
  response <- content(request, as = "text", encoding = "UTF-8")
  df_org <- fromJSON(response, flatten = TRUE)
  df_org <- enframe(unlist(df_org))
  df_org <- df_org[grep("(postcode_1)|(local_authority_code)|(lsoa_code)", df_org$name), ]

  if (nrow(df_org) != 0) {

  df_org$name <- c("postcode", "lad18cd", "lsoa_code")
  list_lu_req[[i]] <- df_org

  }

  else {
    next
  }

}

tidy_pc <- bind_rows(list_lu_req) %>%
  mutate(ind = rep(1:(nrow(.)/3), each = 3)) %>%
  pivot_wider(names_from = "name", values_from = "value") %>%  ## two missing post codes
  select(-ind) %>% 
  mutate(postcode = str_replace_all(postcode, " ", ""))

ae_api_lacd <- left_join(ae_api %>% mutate(postcode = str_replace_all(postcode, " ", "")),
                    tidy_pc, 
                    by = "postcode")

# index multiple deprivation lookup http://geoportal.statistics.gov.uk/datasets/index-of-multiple-deprivation-december-2015-lookup-in-england
imd <- read_csv("https://opendata.arcgis.com/datasets/da3b33dd44d94f48a9628a3391957505_0.csv") %>% 
  setNames(., c("lsoa_code", "lsoa_nm", "imd15", "id"))
  select(-id)

merged_imd <- inner_join(ae_api_lacd, imd, by = "lsoa_code")

# deprivation deciles as of guideline https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/579151/English_Indices_of_Deprivation_2015_-_Frequently_Asked_Questions_Dec_2016.pdf
imd_quntiles <- c(1, quantile(1:32844, probs = seq(.1, 1, by = .1)))
  
merged_imd$imd_dec <- cut(as.numeric(merged_imd$imd15), 
                          breaks = imd_quntiles, 
                          include.lowest = T,
                          labels = c("1-10", "10-20", "20-30", "30-40", "40-50", "50-60", "60-70", "70-80", "80-90", "90-100"))

# plots ---------------------------------------------------------------

# theme
theme_session <- 
  theme_minimal(base_family = "Ubuntu Mono") +
  theme(
    text = element_text(family = "Ubuntu Mono", color = "white"),
    axis.text = element_text(colour = "grey85"),
    axis.title = element_blank(),
    axis.ticks = element_line(color = "grey85"),
    strip.text = element_text(colour = "white", size = 12),
    plot.background = element_rect(fill = "grey20"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "cm"),
    panel.grid.major.y = element_line(color = alpha("grey25", .5)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(margin = margin(t = .5, unit = "cm"), size = 20, hjust = .5),
    plot.subtitle = element_text(margin = margin(t = .5, b = 2, unit = "cm"), size = 12, hjust = .5),
    plot.caption = element_text(margin = margin(t = 2, unit = "cm"))
    )

theme_set(theme_session)

# cumsum and map ----------------------------------------------------------

# cummulative attendances
top_5_att <- 
  merged_imd %>% 
  filter(period < "2018-04-01") %>%  ## many orgs with missing data after this date
  select(name, attendances) %>% 
  group_by(name) %>% 
  summarise(sum_att = sum(attendances, na.rm = TRUE)) %>% 
  top_n(5) %>% 
  pull(name)

cummulative_all_plot <- 
  merged_imd %>% 
  select(org_code, attendances, period) %>% 
  filter(period < "2018-04-01") %>%
  group_by(org_code, period) %>% 
  summarise(att = sum(attendances, na.rm = TRUE)) %>% 
  arrange(period) %>% 
  mutate(csum_att = cumsum(att)) %>% 
  left_join(., select(merged_imd, org_code, name)) %>% 
  distinct() %>% 
  mutate(
        color = case_when(
          name == top_5_att[1] ~ "#b58900",
          name == top_5_att[2] ~ "#cb4b16",
          name == top_5_att[3] ~ "#dc322f",
          name == top_5_att[4] ~ "#6c71c4",
          name == top_5_att[5] ~ "#2aa198",
          TRUE ~ "grey35"
        ),
        alpha = if_else(name %in% top_5_att, 1, 0.2)) %>% 
  ggplot(aes(x = period, y = csum_att, group = org_code)) +
  geom_step(aes(color = color, alpha = alpha)) +
  scale_color_identity(guide = FALSE) +
  geom_text(data = . %>% filter(name %in% top_5_att, period == last(period)) %>% 
              ungroup() %>% 
              arrange(-csum_att) %>% 
              mutate(position = c(590000, 560000, 530000, 500000, 470000)),
            x = as.Date("2016-06-01"), 
            aes(y = position, label = glue::glue("{str_to_title(name)}: {scales::comma(csum_att)}"), color = color),
            family = "Ubuntu Mono",
            hjust = 0) +
  scale_alpha_identity() +
  scale_x_date(date_labels = "%y-%b", 
               breaks = seq.Date(as.Date("2016-04-01"), as.Date("2018-03-01"), "month"), 
               limits = c(as.Date("2016-04-01"), as.Date("2018-03-01"))) +
  scale_y_continuous(labels = scales::comma_format(), 
                     expand = c(0, 0),
                     limits = c(0, NA)) +
  theme_session +
  theme(axis.text.x = element_text(angle = 90),
        panel.border = element_rect(color = "grey85", fill = NA))

# map 
map_att <- 
  inner_join(sf_gb %>% mutate(lad18cd = toupper(lad18cd)), by = c("lad18cd"),
          merged_imd %>% 
            filter(period < "2018-04-01") %>%
            group_by(lad18cd) %>% 
            summarise(adm = sum(admissions, na.rm = TRUE)) %>% 
            left_join(., merged_imd[, c("imd_dec", "lad18cd")])) %>%
  ggplot() +
  geom_sf(aes(fill = adm), size = .1, color = "gray85") +
  rcartocolor::scale_fill_carto_c(name = NULL,
                                  palette = "Emrld",
                                  labels = scales::comma_format(),
                                  guide = guide_legend(title.position = "top",
                                                       title.hjust = .5,
                                                       label.position = "bottom")) +
  coord_sf(datum = NA) +
  theme_session +
  theme(legend.position = c(.5, 0),
        legend.direction = "horizontal",
        legend.key.height = unit(1, "mm"),
        legend.key.width = unit(5, "mm"))

# assemble
((cummulative_all_plot + map_att) + 
    plot_layout(widths = c(1, .9)) +
    plot_annotation(title = "Accumulated A&E attendances by hospital and aggregated by Local Authority",
                  subtitle = "Map does not show Local Authorities from London area",
                  caption = "Data: NHS England-A&E Attendances | Graphic: @pabrodez\nContains ONS data")
  )
ggsave(filename = "cummulative_all_map.png", width = 17, height = 9, dpi = "retina")

# attendances by month and by A&E type -------------------------------------

select(merged_imd, org_code, type, attendances, period, breaches, attendances) %>% 
  filter(period < "2018-04-01") %>%
  group_by(org_code, type) %>% 
  arrange(period) %>% 
  mutate(prop_breach = breaches / attendances) %>%
  ggplot(aes(x = period, y = attendances, group = org_code)) +
  geom_line(aes(color = prop_breach), size = .5) +
  facet_wrap(~ type) +
  scale_x_date(date_labels = "%y-%b", 
               breaks = seq.Date(as.Date("2016-04-01"), as.Date("2018-03-01"), "month"), 
               limits = c(as.Date("2016-04-01"), as.Date("2018-03-01")),
               expand = c(0, 0)) +
  scale_y_continuous(labels = scales::comma_format(), 
                     expand = c(0, 0),
                     limits = c(0, NA)) +
  rcartocolor::scale_color_carto_c(palette = "TealGrn", direction = -1,
                                   name = "Prop. breach of attendances [0,1]",
                                   guide = guide_legend(title.position = "top",
                                                        title.hjust = .5,
                                                        label.position = "bottom",
                                                        nrow = 1,
                                                        override.aes = list(size = 1))) +
  labs(title = "Attendances and proportion of >4 hour breaches by month and type of A&E dept.",
       caption = "Data: NHS England-A&E Attendances | Graphic: @pabrodez") +
  theme(axis.ticks = element_line(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        strip.text = element_text(margin = margin(b = 1, unit = "cm")),
        panel.spacing.x = unit(1, "cm"),
        panel.grid.major.x = element_line(color = alpha("grey25", .5)),
        legend.position = "bottom",
        legend.margin = margin(t = 1, unit = "cm"),
        legend.key.width = unit(1, "cm"),
        plot.title = element_text(margin = margin(t = 1, b = 2, unit = "cm")))

ggsave(filename = "./breach_type.png", width = 17, height = 9, dpi = "retina")


# same as above and faceted by IMD decile ---------------------------------

select(merged_imd, imd_dec, org_code, type, attendances, period, breaches) %>% 
  filter(period < "2018-04-01") %>%
  group_by(org_code, type) %>% 
  arrange(period) %>% 
  mutate(prop_breach = breaches / attendances) %>%
  ungroup() %>% 
  ggplot(aes(x = period, y = attendances, group = org_code)) +
  geom_line(aes(color = prop_breach), size = .5) +
  facet_grid(type ~ imd_dec, scales = "free_y", labeller = ) +
  scale_x_date(date_labels = "%y %b", 
               breaks = seq.Date(as.Date("2016-04-01"), as.Date("2018-03-01"), "3 months"), 
               limits = c(as.Date("2016-04-01"), as.Date("2018-03-01")),
               expand = c(0, 0)) +
  scale_y_continuous(labels = scales::comma_format(), 
                     expand = c(0, 0),
                     limits = c(0, NA)) +
  rcartocolor::scale_color_carto_c(palette = "TealGrn", direction = -1,
                                   name = "Prop. breach of attendances [0,1]",
                                   guide = guide_legend(title.position = "top",
                                                        title.hjust = .5,
                                                        label.position = "bottom",
                                                        nrow = 1,
                                                        override.aes = list(size = 1))) +
  labs(title = "Attendances and proportion of >4 hour breaches by month, type of A&E dept. and the IMD decile of its LSOA",
       caption = "Data: NHS England-A&E Attendances | Graphic: @pabrodez\nContains ONS data") +
  theme(axis.ticks = element_line(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.spacing.x = unit(1, "cm"),
        panel.spacing.y = unit(1, "cm"),
        strip.text.y = element_text(margin = margin(l = .5, unit = "cm")),
        strip.text.x = element_text(margin = margin(b = .5, unit = "cm")),
        panel.grid.major.x = element_line(color = alpha("grey25", .5)),
        legend.position = "bottom",
        legend.margin = margin(t = 1, unit = "cm"),
        legend.key.width = unit(1, "cm"),
        plot.title = element_text(margin = margin(t = 1, b = 2, unit = "cm"), size = 16))

ggsave(filename = "./breach_type_imd.png", width = 17, height = 11, dpi = "retina")

# TODO: attendances by IMD ------------------------------------------------------

merged_imd %>% 
  filter(period < "2018-04-01") %>%
  select(type, imd_dec, attendances) %>% 
  group_by(imd_dec, type) %>% 
  summarise(sum_att = sum(attendances, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(imd_dec) %>% 
  mutate(imd_att = sum(sum_att)) %>% 
  ungroup() %>%
  ggplot(aes(y = imd_dec, color = imd_dec)) +
  geom_point(aes(x = sum_att, shape = type), size = 4) +
  geom_point(aes(x = imd_att), size = 2.5, shape = 21) +
  geom_point(aes(x = imd_att), size = 5, shape = 21) +
  geom_vline(aes(xintercept = mean(imd_att)), linetype = 2, color = "grey 40") +
  rcartocolor::scale_color_carto_d(palette = "Antique", guide = FALSE)


