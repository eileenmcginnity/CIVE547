library(tidyverse)
library(hms)
library(trend)
library(ggthemes)
library(knitr)
library(grid)
library(gtable)
library(broom)

co_hail_storms <- read_csv("data/co_hail_data.csv") %>% 
  mutate(month = factor(month(begin_date_time, label = TRUE), levels = month.abb))
annual_season_length <- read_csv("data/annual_season_length.csv")



## RANDOM STATS ##

storm_times <- co_hail_storms %>% 
  group_by(hour(begin_date_time)) %>% 
  count() %>% 
  print(n = 24)

storm_counts <- co_hail_storms %>%
  group_by(episode_id) %>%
  count() %>% 
  arrange(desc(n))
## each storm code

co_hail_storms %>%
  group_by(event_id) %>%
  count() %>% 
  arrange(desc(n))
## each individual hail report, could be within the same storm code
## each value should be 1

co_hail_storms %>% 
  mutate(daymonth = format(as.Date(begin_date_time), "%d-%m")) %>% 
  mutate(ydm_date = paste(1900, daymonth, sep = "-")) %>% 
  mutate(ydm_date = ydm(ydm_date)) %>% 
  summarise(mean_hail_occurance = mean(ydm_date))
## mean hail occurance date

co_hail_storms %>%
  group_by(magnitude) %>% 
  count()
## number of reports per size

mean_med_size <- co_hail_storms %>%
  summarise(mean = as.numeric(mean(magnitude)),
            median = as.numeric(median(magnitude)))

annual_hail_days <- co_hail_storms %>%
  group_by(year) %>% 
  mutate(date = as.Date(begin_date_time)) %>% 
  summarise(hail_days = n_distinct(date))
## hail days per year
var(annual_hail_days$hail_days)

county_annual_hail_days <- co_hail_storms %>% 
  group_by(year, cz_name) %>% 
  mutate(date = as.Date(begin_date_time)) %>% 
  summarise(hail_days = n_distinct(date))
## hail days per year per county
var(county_annual_hail_days$hail_days)

monthly_hail_days <- co_hail_storms %>% 
  group_by(month) %>% 
  mutate(date = as.Date(begin_date_time)) %>%
  summarise(hail_days = n_distinct(date)) %>%
  ungroup() %>% 
  add_row(month = "Jan", hail_days = 0) %>% 
  add_row(month = "Feb", hail_days = 0) %>% 
  add_row(month = "Nov", hail_days = 0) %>% 
  add_row(month = "Dec", hail_days = 0) 
monthly_hail_days <- monthly_hail_days %>% 
  mutate(month = factor(month, levels = month.abb))
## monthly hail days

annual_monthly_hail_days <- co_hail_storms %>% 
  group_by(month, year) %>% 
  mutate(date = as.Date(begin_date_time)) %>%
  summarise(hail_days = n_distinct(date)) %>%
  ungroup() %>% 
  add_row(month = "Jan", year = 2003, hail_days = 0) %>% 
  add_row(month = "Feb", year = 2003, hail_days = 0) %>% 
  add_row(month = "Nov", year = 2003, hail_days = 0) %>% 
  add_row(month = "Dec", year = 2003, hail_days = 0) 
annual_monthly_hail_days <- annual_monthly_hail_days %>% 
  mutate(month = factor(month, levels = month.abb)) %>% 
  group_by(month) %>% 
  complete(year = 2003:2022) %>%
  replace_na(list(hail_days = 0))
## annual monthly number of hail days

var(annual_monthly_hail_days$hail_days)

march_hail_days <- annual_monthly_hail_days %>% 
  filter(month == "Mar")
april_hail_days <- annual_monthly_hail_days %>%
  filter(month == "Apr")
may_hail_days <- annual_monthly_hail_days %>% 
  filter(month == "May")
june_hail_days <- annual_monthly_hail_days %>% 
  filter(month == "Jun")
july_hail_days <- annual_monthly_hail_days %>% 
  filter(month == "Jul")
august_hail_days <- annual_monthly_hail_days %>% 
  filter(month == "Aug")
september_hail_days <- annual_monthly_hail_days %>% 
  filter(month == "Sep")
october_hail_days <- annual_monthly_hail_days %>% 
  filter(month == "Oct")

monthly_hail_days_slope_pval <- annual_monthly_hail_days %>%
  group_by(month) %>%
  do({
    # Fit linear regression for each county
    lm_model <- lm(hail_days ~ year, data = .)
    
    # Extract slope and calculate standard error, t-value, and p-value
    tidy(lm_model) %>%
      filter(term == "year") %>%
      mutate(se_slope = std.error,
             t_value = estimate / std.error,
             p_value = 2 * pt(abs(t_value), df = lm_model$df.residual, lower.tail = FALSE)) %>%
      select(month = unique(.$month), slope = estimate, se_slope, t_value, p_value)
  }) %>%
  ungroup() %>% 
  filter(!is.nan(p_value))

monthly_hail_days_slope_pval$slope <- round(monthly_hail_days_slope_pval$slope, digits = 2)

monthly_hail_days_slope_pval %>% 
  select(month, slope, p_value) %>% 
  kable(format = "simple")
monthly_hail_days_slope_pval %>%
  filter(slope == 0)
  




co_hail_storms %>% 
  group_by(cz_name) %>%
  count() %>% 
  arrange(desc(n)) %>% 
  print(n = 54)
## count per county

monthly_reports <- co_hail_storms %>%
  group_by(month = month(begin_date_time, label = TRUE, abbr = FALSE)) %>% 
  count(name = "reports", .drop = FALSE)
## reports per month

monthly_reports <- mutate(monthly_reports, month = as.character(month))

missing_months <- tibble(
  month = c("November", "December", "January", "February"),
  reports = 0)

monthly_reports <- full_join(monthly_reports, missing_months)

monthly_reports$month <- fct_expand(monthly_reports$month, levels(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))) %>% 
  fct_relevel(levels(c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")))

annual_monthly_reports <- co_hail_storms %>% 
  group_by(year, month = month(begin_date_time, label = TRUE, abbr = FALSE)) %>% 
  count(name = "reports", .drop = FALSE)
## annual monthly reports

var(annual_monthly_reports$reports)

percent_years_with_hail <- annual_monthly_reports %>% 
  group_by(month) %>% 
  summarise(years_with_reports = sum(reports > 0)) %>% 
  mutate(percent_years_with_reports= (years_with_reports/20)*100) %>% 
  select(month, percent_years_with_reports)

completed_annual_monthly_reports <- annual_monthly_reports %>%
  group_by(month) %>%
  complete(year = 2003:2022) %>%
  replace_na(list(reports = 0))
# Complete df of annual reports by county name

march_reports <- annual_monthly_reports %>% 
  filter(month == "March")
march_reports <- march_reports %>%
  ungroup() %>% 
  add_row(year = 2003, month = "March", reports = 0) %>% 
  add_row(year = 2004, month = "March", reports = 0) %>% 
  add_row(year = 2005, month = "March", reports = 0) %>% 
  add_row(year = 2006, month = "March", reports = 0) %>% 
  add_row(year = 2008, month = "March", reports = 0) %>% 
  add_row(year = 2009, month = "March", reports = 0) %>% 
  add_row(year = 2010, month = "March", reports = 0) %>% 
  add_row(year = 2011, month = "March", reports = 0) %>% 
  add_row(year = 2012, month = "March", reports = 0) %>% 
  add_row(year = 2013, month = "March", reports = 0) %>% 
  add_row(year = 2014, month = "March", reports = 0) %>% 
  add_row(year = 2015, month = "March", reports = 0) %>% 
  add_row(year = 2016, month = "March", reports = 0) %>% 
  add_row(year = 2017, month = "March", reports = 0) %>% 
  add_row(year = 2018, month = "March", reports = 0) %>% 
  add_row(year = 2020, month = "March", reports = 0) %>% 
  add_row(year = 2021, month = "March", reports = 0) %>% 
  add_row(year = 2022, month = "March", reports = 0) %>% 
  arrange(year)

april_reports <- annual_monthly_reports %>% 
  filter(month == "April")
april_reports <- april_reports %>%
  ungroup() %>% 
  add_row(year = 2011, month = "April", reports = 0) %>% 
  add_row(year = 2014, month = "April", reports = 0) %>% 
  add_row(year = 2017, month = "April", reports = 0) %>% 
  add_row(year = 2020, month = "April", reports = 0) %>% 
  add_row(year = 2022, month = "April", reports = 0) %>% 
  arrange(year)

may_reports <- annual_monthly_reports %>%
  filter(month == "May")

june_reports <- annual_monthly_reports %>%
  filter(month == "June")

july_reports <- annual_monthly_reports %>%
  filter(month == "July")

august_reports <- annual_monthly_reports %>%
  filter(month == "August")

september_reports <- annual_monthly_reports %>%
  filter(month == "September")
september_reports <- september_reports %>%
  ungroup() %>% 
  add_row(year = 2010, month = "September", reports = 0) %>% 
  add_row(year = 2018, month = "September", reports = 0) %>% 
  add_row(year = 2020, month = "September", reports = 0) %>% 
  add_row(year = 2022, month = "September", reports = 0) %>% 
  arrange(year)

october_reports <- annual_monthly_reports %>%
  filter(month == "October")
october_reports <- october_reports %>%
  ungroup() %>% 
  add_row(year = 2003, month = "October", reports = 0) %>% 
  add_row(year = 2004, month = "October", reports = 0) %>% 
  add_row(year = 2009, month = "October", reports = 0) %>% 
  add_row(year = 2011, month = "October", reports = 0) %>% 
  add_row(year = 2012, month = "October", reports = 0) %>% 
  add_row(year = 2014, month = "October", reports = 0) %>% 
  add_row(year = 2016, month = "October", reports = 0) %>% 
  add_row(year = 2019, month = "October", reports = 0) %>% 
  add_row(year = 2020, month = "October", reports = 0)


monthly_county_reports <- co_hail_storms %>% 
  group_by(cz_name, month = month(begin_date_time, label = TRUE, abbr = FALSE)) %>% 
  count(name = "reports", .drop = FALSE)
## reports per county per month

annual_reports <- co_hail_storms %>% 
  group_by(year) %>% 
  count(name = "reports")
## reports per year
var(annual_reports$reports)

annual_season_length
## length of the hail season per year (number of days between the first hail of the year to last hail of the year)
mean(annual_season_length$season_length)

avg_annual_size <- co_hail_storms %>%
  group_by(year) %>% 
  summarise(avg_magnitude = mean(magnitude))
## average hail size per year
var(avg_annual_size$avg_magnitude)

avg_monthly_size <- co_hail_storms %>% 
  group_by(month) %>% 
  summarise(avg_magnitude = mean(magnitude))
## average hail size per year

avg_annual_monthly_size <- co_hail_storms %>% 
  group_by(year, month = month(begin_date_time, abbr = FALSE, label = TRUE)) %>% 
  summarise(avg_magnitude = mean(magnitude)) %>%
  ungroup()
## avg size per month per year

var(avg_annual_monthly_size$avg_magnitude)

months_nomar <- c("April", "May", "June", "July", "August", "September", "October")

monthly_size_slope_pval <- avg_annual_monthly_size %>%
  filter(month != "March") %>% 
  group_by(month) %>%
  nest() %>%
  mutate(model = map(data, ~ lm(avg_magnitude ~ year, data = .)),
         results = map(model, tidy)) %>%
  unnest(results) %>%
  filter(term == "year") %>%
  select(month, slope = estimate, p_value = p.value) %>%
  arrange(month)

monthly_size_slope_pval$slope <- round(monthly_size_slope_pval$slope, digits = 2)

monthly_size_slope_pval %>% 
  select(month, slope, p_value) %>% 
  kable(format = "simple")
monthly_size_slope_pval %>%
  filter(slope < 0)


march_avg_size <- avg_annual_monthly_size %>% 
  group_by(year, month) %>% 
  filter(month == "March")

april_avg_size <- avg_annual_monthly_size %>% 
  group_by(year, month) %>% 
  filter(month == "April")

may_avg_size <- avg_annual_monthly_size %>% 
  group_by(year, month) %>% 
  filter(month == "May")

june_avg_size <- avg_annual_monthly_size %>% 
  group_by(year, month) %>% 
  filter(month == "June")

july_avg_size <- avg_annual_monthly_size %>% 
  group_by(year, month) %>% 
  filter(month == "July")

august_avg_size <- avg_annual_monthly_size %>% 
  group_by(year, month) %>% 
  filter(month == "August")

september_avg_size <- avg_annual_monthly_size %>% 
  group_by(year, month) %>% 
  filter(month == "September")

october_avg_size <- avg_annual_monthly_size %>% 
  group_by(year, month) %>% 
  filter(month == "October")

avg_county_size <- co_hail_storms %>% 
  group_by(cz_name) %>% 
  summarise(avg_magnitude = mean(magnitude))
## average hail size per county

avg_annual_county_size <- co_hail_storms %>% 
  group_by(year, cz_name) %>% 
  summarise(avg_magnitude = mean(magnitude))
## annual avg hail size per county
var(avg_annual_county_size$avg_magnitude)

slopes_by_county_size <- avg_annual_county_size %>%
  group_by(cz_name) %>%
  do(tidy(lm(avg_magnitude ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(cz_name, slope = estimate)

slopes_by_county_size %>% 
  filter(slope == 0 & is.na(slope))


county_size_slope_pval <- avg_annual_county_size %>%
  group_by(cz_name) %>%
  do({
    # Fit linear regression for each county
    lm_model <- lm(avg_magnitude ~ year, data = .)
    
    # Extract slope and calculate standard error, t-value, and p-value
    tidy(lm_model) %>%
      filter(term == "year") %>%
      mutate(se_slope = std.error,
             t_value = estimate / std.error,
             p_value = 2 * pt(abs(t_value), df = lm_model$df.residual, lower.tail = FALSE)) %>%
      select(cz_name = unique(.$cz_name), slope = estimate, se_slope, t_value, p_value)
  }) %>%
  ungroup() %>% 
  filter(!is.na(slope)) %>% 
  filter(!cz_name %in% c("Dolores", "San Miguel", "Rio Grande"))
## SLOPE AND PVALUES PER COUNTY

county_size_slope_pval$slope <- round(county_size_slope_pval$slope, digits = 2)

county_size_slope_pval %>% 
  select(cz_name, slope, p_value) %>% 
  kable(format = "simple")
county_size_slope_pval %>%
  filter(slope == 0)
  

filtered_counties_size <- avg_annual_county_size %>% 
  group_by(cz_name) %>% 
  summarise(num_years = n_distinct(year)) %>% 
  filter(num_years > 3) %>% 
  select(cz_name)

annual_county_reports <- co_hail_storms %>% 
  group_by(year, cz_name) %>% 
  count(name = "reports")
## annual reports per county
var(annual_county_reports$reports)

filtered_counties <- annual_county_reports %>%
  group_by(cz_name) %>%
  summarise(num_years = n_distinct(year)) %>%
  filter(num_years > 3) %>%
  select(cz_name)

filtered_avg_annual_county_size <- avg_annual_county_size %>% 
  filter(cz_name %in% filtered_counties$cz_name) %>% 
  group_by(cz_name)

list_of_county_avg_size <- group_split(filtered_avg_annual_county_size, .keep = FALSE)

for (i in 1:length(list_of_county_avg_size)) {
  cz_name <- unique(filtered_avg_annual_county_size$cz_name)[i]
  object_name <- paste0(cz_name, "_annual_avg_size")
  assign(object_name, list_of_county_avg_size[[i]])
}
## makes dfs for each county per avg annual size


filtered_annual_county_reports <- annual_county_reports %>%
  filter(cz_name %in% filtered_counties$cz_name)

annual_county_reports <- annual_county_reports %>%
  group_by(cz_name) %>%
  complete(year = 2003:2022) %>%
  replace_na(list(reports = 0))

grouped_county_reports <- filtered_annual_county_reports %>%
  group_by(cz_name) %>%
  complete(year = 2003:2022) %>%
  replace_na(list(reports = 0))
# Grouping by county name

slopes_by_county <- annual_county_reports %>%
  group_by(cz_name) %>%
  do(tidy(lm(reports ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(cz_name, slope = estimate)

slopes_by_county %>% filter(slope > 0)

county_reports_slope_pval <- annual_county_reports %>%
  group_by(cz_name) %>%
  do({
    # Fit linear regression for each county
    lm_model <- lm(reports ~ year, data = .)
    
    # Extract slope and calculate standard error, t-value, and p-value
    tidy(lm_model) %>%
      filter(term == "year") %>%
      mutate(se_slope = std.error,
             t_value = estimate / std.error,
             p_value = 2 * pt(abs(t_value), df = lm_model$df.residual, lower.tail = FALSE)) %>%
      select(cz_name = unique(.$cz_name), slope = estimate, se_slope, t_value, p_value)
  }) %>%
  ungroup()

county_reports_slope_pval$slope <- round(county_reports_slope_pval$slope, digits = 2)

county_reports_slope_pval %>% 
  kable(format = "simple")
county_reports_slope_pval %>%
  filter(slope < 0)



complete_county_annual_hail_days <- county_annual_hail_days %>% 
  group_by(cz_name) %>%
  complete(year = 2003:2022) %>%
  replace_na(list(hail_days = 0))
## complete annual hail days per county
var(complete_county_annual_hail_days$hail_days)



filtered_hail_days_county <- complete_county_annual_hail_days %>% 
  filter(cz_name %in% filtered_counties$cz_name) %>%
  group_by(cz_name)

list_of_county_hail_days <- group_split(filtered_hail_days_county, .keep = FALSE)

for (i in 1:length(list_of_county_hail_days)) {
  cz_name <- unique(filtered_hail_days_county$cz_name)[i]
  object_name <- paste0(cz_name, "_annual_hail_days")
  assign(object_name, list_of_county_hail_days[[i]])
}


slopes_by_county_hail_days <- complete_county_annual_hail_days %>%
  group_by(cz_name) %>%
  do(tidy(lm(hail_days ~ year, data = .))) %>%
  filter(term == "year") %>%
  select(cz_name, slope = estimate)

slopes_by_county_hail_days %>%
  filter(slope > 0)


county_hail_days_slope_pval <- county_annual_hail_days %>%
  group_by(cz_name) %>%
  do({
    # Fit linear regression for each county
    lm_model <- lm(hail_days ~ year, data = .)
    
    # Extract slope and calculate standard error, t-value, and p-value
    tidy(lm_model) %>%
      filter(term == "year") %>%
      mutate(se_slope = std.error,
             t_value = estimate / std.error,
             p_value = 2 * pt(abs(t_value), df = lm_model$df.residual, lower.tail = FALSE)) %>%
      select(cz_name = unique(.$cz_name), slope = estimate, se_slope, t_value, p_value)
  }) %>%
  ungroup() %>% 
  filter(!is.na(slope)) %>% 
  filter(!is.nan(p_value))

county_hail_days_slope_pval$slope <- round(county_hail_days_slope_pval$slope, digits = 2)
## COUNTY HAIL DAYS SLOPE AND PVAL

county_hail_days_slope_pval %>% 
  kable(format = "simple")
county_hail_days_slope_pval %>%
  filter(slope == 0)


list_of_county_reports <- group_split(grouped_county_reports, .keep = FALSE)

for (i in 1:length(list_of_county_reports)) {
  cz_name <- unique(grouped_county_reports$cz_name)[i]
  object_name <- paste0(cz_name, "_annual_reports")
  assign(object_name, list_of_county_reports[[i]])
}
## perfect, generates df for each county


  
  



## EXPLORATORY DATA ANALYSIS (EDA) ##
max_reports <- co_hail_storms %>%
  group_by(year, month) %>%
  count() %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(maximum_reports = max(n), min_month = min(month), max_month = max(month))

max_reports$max_month[max_reports$max_month == "Oct"] <- "Nov"
max_reports$max_month[max_reports$max_month == "Sep"] <- "Oct"
max_reports$max_month[max_reports$max_month == "Aug"] <- "Sep"
  ## only used for the below graph

max_reports <- max_reports %>% 
  left_join(y = annual_season_length, join_by(year))

co_hail_storms %>% 
  group_by(year, month = month(begin_date_time,
                               label = TRUE,
                               abbr = TRUE)) %>% 
  count(name = "reports", .drop = FALSE) %>% 
  ungroup() %>% 
  ggplot() +
  geom_col(aes(x = month,y = reports)) +
  geom_errorbar(data = max_reports,
                 aes(xmin = min_month,
                     xmax = max_month,
                     y = maximum_reports + 25,
                     color = "season length"),
                width = 25, position = position_nudge(x = -.5)) +
  geom_text(data = max_reports,
            aes(x = 6.5,
                y = maximum_reports + 50),
            label = paste(max_reports$season_length, "days"),
            color = "red",
            size = 4) +
  scale_color_manual(values = "red", guide = guide_legend(title = NULL)) +
  scale_x_discrete(drop = FALSE, labels = function(x) substr(x, 1, 1)) +
  facet_wrap(~ year, scales = "free_x") +
  labs(y = "number of hailstone reports",
       title = "Colorado Monthly Hail Reports and Hail Season Length per Year (2003-2022)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "right")
  ## largest chart known to man
  ## Monthly hail reports and season length per year

year_range <- range(co_hail_storms$year, na.rm = TRUE)

full_year_span <- seq(year_range[1], year_range[2], by = 1)



max_reports_county <- co_hail_storms %>% 
  group_by(cz_name) %>% 
  summarise(total_years = n_distinct(year)) %>% 
  filter(total_years > 2) %>%
  inner_join(co_hail_storms, by = "cz_name") %>% 
  group_by(cz_name, year) %>% 
  count(name = "yearly_reports") %>%
  group_by(cz_name) %>% 
  summarise(max_yearly_reports = max(yearly_reports, na.rm = FALSE)) %>%
  mutate(max_yearly_reports = max_yearly_reports + 1)
  ## only used for below graph



annual_county_reports %>% 
  ggplot() +
  geom_point(aes(x = year, y = reports),
             color = "black") + 
  geom_smooth(aes(x = year,
                  y = reports,
                  color = "linear regression"),
              method = "lm") +
  scale_color_manual(values = "blue", guide = guide_legend(title = NULL)) +
  facet_wrap(~ cz_name, scales = "free", ncol = 8) +
  scale_x_continuous(minor_breaks = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "right",
        legend.margin = margin(t = -30),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8)) +
  labs(y = "number of hailstone reports",
       title = "Colorado Counties' Annual Hail Reports (2003–2022)") +
  geom_blank(data = max_reports_county, aes(y = max_yearly_reports)) +
  ylim(0, NA) +
  expand_limits(x = c(2003, 2022))
## hail reports per county


annual_county_reports %>% 
  ggplot() +
  geom_boxplot(aes(y = reports)) + 
  facet_wrap(~ cz_name, scale = "free_y", ncol = 8) +
  geom_hline(aes(yintercept = 1,
                 color = "Median of counties experiencing hail"),
             alpha = .5,
             linetype = "dashed",
             size = .75) +
  geom_hline(aes(yintercept = 5.72,
                 color = "Mean of counties experiencing hail"),
             alpha = .5,
             linetype = "dashed",
             size = .75) +
  scale_color_manual(name = "hlines",
                     values = c("Mean of counties experiencing hail" = "blue",
                                "Median of counties experiencing hail" = "red"),
                     guide = guide_legend(title = NULL,
                                          nrow = 2)) +
  labs(title = "Colorado Counties' Annual Hail Reports (2003–2022)",
       x = NULL,
       y = "number of reports") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.justification = "right",
        legend.margin = margin(t = -30))
## annual reports boxplots per county


complete_county_annual_hail_days %>% 
  ggplot() +
  geom_point(aes(x = year, y = hail_days),
             color = "black") + 
  geom_smooth(aes(x = year,
                  y = hail_days,
                  color = "linear regression"),
              method = "lm") +
  scale_color_manual(values = "blue", guide = guide_legend(title = NULL)) +
  facet_wrap(~ cz_name, scale = "free", ncol = 8) +
  scale_x_continuous(minor_breaks = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "right",
        legend.margin = margin(t = -30),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8)) +
  labs(y = "number of hail days",
       title = "Colorado Counties' Annual Hail Days (2003-2022)") +
  expand_limits(x = c(2003, 2022))
## county annual hail days


complete_county_annual_hail_days %>% 
  ggplot() +
  geom_boxplot(aes(y = hail_days)) + 
  geom_hline(aes(yintercept = 1,
                 color = "Median of counties experiencing hail"),
             alpha = .5,
             linetype = "dashed",
             size = .75) +
  geom_hline(aes(yintercept = 2.36,
                 color = "Mean of counties experiencing hail"),
             alpha = .5,
             linetype = "dashed",
             size = .75) +
  scale_color_manual(name = "hlines",
                     values = c("Mean of counties experiencing hail" = "blue",
                                "Median of counties experiencing hail" = "red"),
                     guide = guide_legend(title = NULL,
                                          nrow = 2)) +
  facet_wrap(~ cz_name, scale = "free_y", ncol = 8) +
  labs(title = "Colorado Counties' Annual Hail Days (2003-2022)",
       x = NULL,
       y = "number of hail days") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.justification = "right",
        legend.margin = margin(t = -30)) +
  expand_limits(y = c(0, NA))
## annual hail days boxplots per county



avg_annual_county_size %>% 
  ggplot() +
  geom_point(aes(x = year, y = avg_magnitude),
             color = "black") + 
  geom_smooth(aes(x = year,
                  y = avg_magnitude,
                  color = "linear regression"),
              method = "lm") +
  scale_color_manual(values = "blue", guide = guide_legend(title = NULL)) +
  facet_wrap(~ cz_name, scales = "free", ncol = 8) +
  scale_x_continuous(minor_breaks = NULL) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.justification = "right",
        legend.margin = margin(t = -30),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 8)) +
  labs(y = "average hailstone size (inches)",
       title = "Colorado Counties' Average Annual Hailstone Size (2003–2022)") +
  expand_limits(x = c(2003, 2022))
## hail reports per county linear trend



avg_annual_county_size %>% 
  ggplot() +
  geom_boxplot(aes(y = avg_magnitude)) + 
  geom_hline(aes(yintercept = 1.25,
                 color = "Colorado median"),
             alpha = .5,
             linetype = "dashed",
             size = .75) +
  geom_hline(aes(yintercept = 1.3678,
                 color = "Colorado mean"),
             alpha = .5,
             linetype = "dashed",
             size = .75) +
  scale_color_manual(name = "hlines",
                     values = c("Colorado mean" = "blue",
                                "Colorado median" = "red"),
                     guide = guide_legend(title = NULL,
                                          nrow = 2)) +
  facet_wrap(~ cz_name,
             scale = "free_y",
             ncol = 8) +
  labs(title = "Colorado Counties' Average Hailstone Size (2003-2022)",
       x = NULL,
       y = "diameter (inches)") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.justification = "right",
        legend.margin = margin(t = -30))
## hail size boxplots per county


## LM ##
march_reports %>% 
  lm(reports ~ year, data = .) %>% 
  coef() %>% 
  .["year"]
april_reports %>% 
  lm(reports ~ year, data = .) %>% 
  coef() %>% 
  .["year"]
may_reports %>% 
  lm(reports ~ year, data = .) %>% 
  coef() %>% 
  .["year"]
june_reports %>% 
  lm(reports ~ year, data = .) %>% 
  coef() %>% 
  .["year"]
july_reports %>% 
  lm(reports ~ year, data = .) %>% 
  coef() %>% 
  .["year"]
august_reports %>% 
  lm(reports ~ year, data = .) %>% 
  coef() %>% 
  .["year"]
september_reports %>% 
  lm(reports ~ year, data = .) %>% 
  coef() %>% 
  .["year"]
october_reports %>%
  lm(reports ~ year, data = .) %>%
  tidy() %>%
  filter(term == "year") %>%
  select(slope = estimate, p_value = p.value)

monthly_reports_slope_pval <- map2_dfr(monthly_data_frames, months, function(df, month) {
  model <- lm(reports ~ year, data = df)
  tidy_model <- tidy(model)
  
  if (month %in% unique(df$month)) {
    tidy_model %>%
      filter(term == "year") %>%
      summarise(slope = first(estimate), p_value = first(p.value)) %>%
      mutate(month = month)
  } else {
    tibble(month = month, slope = NA, p_value = NA)
  }
})

monthly_reports_slope_pval$slope <- round(monthly_reports_slope_pval$slope, digits = 2)

monthly_reports_slope_pval %>% 
  select(month, slope, p_value) %>% 
  kable(format = "simple")
monthly_reports_slope_pval %>%
  filter(slope < 0)



avg_annual_size %>%
  lm(avg_magnitude ~ year, data = .) %>%
  tidy() %>%
  filter(term == "year") %>%
  select(slope = estimate, p_value = p.value)

annual_hail_days %>%
  lm(hail_days ~ year, data = .) %>%
  tidy() %>%
  filter(term == "year") %>%
  select(slope = estimate, p_value = p.value)

annual_reports %>%
  lm(reports ~ year, data = .) %>%
  tidy() %>%
  filter(term == "year") %>%
  select(slope = estimate, p_value = p.value)
  

annual_season_length %>%
  lm(season_length ~ year, data = .) %>%
  tidy() %>%
  filter(term == "year") %>%
  select(slope = estimate, p_value = p.value)


## MANN-KENDALL TESTS ##


mk.test(march_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK reports March

mk.test(april_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK reports April

mk.test(may_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK reports May

mk.test(june_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK reports June

mk.test(july_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK reports July

mk.test(august_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK reports August

mk.test(september_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK reports September
mk.test(october_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK reports October



# Performing Mann-Kendall tests and storing results for March to October
results <- tibble(
  Month = c("March", "April", "May", "June", "July", "August", "September", "October"),
  MK_Results = list(
    mk.test(march_reports$reports, alternative = "two.sided", continuity = TRUE),
    mk.test(april_reports$reports, alternative = "two.sided", continuity = TRUE),
    mk.test(may_reports$reports, alternative = "two.sided", continuity = TRUE),
    mk.test(june_reports$reports, alternative = "two.sided", continuity = TRUE),
    mk.test(july_reports$reports, alternative = "two.sided", continuity = TRUE),
    mk.test(august_reports$reports, alternative = "two.sided", continuity = TRUE),
    mk.test(september_reports$reports, alternative = "two.sided", continuity = TRUE),
    mk.test(october_reports$reports, alternative = "two.sided", continuity = TRUE)))



extract_values <- function(test_result) {
  tibble(
    S = test_result$estimate[["S"]],
    varS = test_result$estimate[["varS"]],
    tau = test_result$estimate[["tau"]],
    z = if ("z" %in% names(test_result$statistic)) test_result$statistic[["z"]] else NA,
    n = test_result$parameter[["n"]],
    p_value = test_result$p.value
  )
}

# Applying the extraction function to MK_Results
MK_reports_results <- results %>%
  mutate(
    extracted = map(MK_Results, extract_values)
  ) %>%
  unnest_wider(extracted) %>% 
  select(-MK_Results)

# Display the final extracted values as separate columns
print(MK_reports_results)
kable(MK_reports_results,
                       format = "simple",
                       caption = "Mann-Kendall Trend Test Output Results for Annual Number of Reports per Month")




  
mk.test(april_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
## MK avg size April

mk.test(may_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
## MK avg size May

mk.test(june_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
## MK avg size June

mk.test(july_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
## MK avg size July

mk.test(august_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
## MK avg size August

mk.test(september_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
## MK avg size September

mk.test(october_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
## MK avg size October

avg_size_results <- tibble(
  Month = c("April", "May", "June", "July", "August", "September", "October"),
  MK_Avg_Size_Results = list(
    mk.test(april_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE),
    mk.test(may_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE),
    mk.test(june_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE),
    mk.test(july_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE),
    mk.test(august_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE),
    mk.test(september_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE),
    mk.test(october_avg_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
  )
)


extract_values_size <- function(test_result) {
  tibble(
    S = test_result$estimate[["S"]],
    varS = test_result$estimate[["varS"]],
    tau = test_result$estimate[["tau"]],
    z = if ("z" %in% names(test_result$statistic)) test_result$statistic[["z"]] else NA,
    n = test_result$parameter[["n"]],
    p_value = test_result$p.value
  )
}

# Applying the extraction function to MK_Results
MK_size_results <- avg_size_results %>%
  mutate(
    extracted = map(MK_Avg_Size_Results, extract_values_size)
  ) %>%
  unnest_wider(extracted) %>% 
  select(-MK_Avg_Size_Results)

# Display the final extracted values as separate columns
print(MK_size_results)
MK_size_results %>% 
  select(Month, S, tau, p_value) %>% 
  kable(format = "simple",
        caption = "Mann-Kendall Trend Test Output Results for Average Hailstone Size per Month")





hail_days_results <- tibble(Month = c("March", "April", "May", "June", "July", "August", "September", "October"), MK_haildays_results = list(
                                 mk.test(march_hail_days$hail_days, alternative = "two.sided", continuity = TRUE),
                                 mk.test(april_hail_days$hail_days, alternative = "two.sided", continuity = TRUE),
                                 mk.test(may_hail_days$hail_days, alternative = "two.sided", continuity = TRUE),
                                 mk.test(june_hail_days$hail_days, alternative = "two.sided", continuity = TRUE),
                                 mk.test(july_hail_days$hail_days, alternative = "two.sided", continuity = TRUE),
                                 mk.test(august_hail_days$hail_days, alternative = "two.sided", continuity = TRUE),
                                 mk.test(september_hail_days$hail_days, alternative = "two.sided", continuity = TRUE),
                                 mk.test(october_hail_days$hail_days, alternative = "two.sided", continuity = TRUE)))

extract_values_hail_days <- function(test_result) {
  tibble(
    S = test_result$estimate[["S"]],
    varS = test_result$estimate[["varS"]],
    tau = test_result$estimate[["tau"]],
    z = if ("z" %in% names(test_result$statistic)) test_result$statistic[["z"]] else NA,
    n = test_result$parameter[["n"]],
    p_value = test_result$p.value
  )
}

# Applying the extraction function to MK_Results
MK_hail_days_results <- hail_days_results %>%
  mutate(
    extracted = map(MK_haildays_results, extract_values_hail_days)
  ) %>%
  unnest_wider(extracted) %>% 
  select(-MK_haildays_results)

# Display the final extracted values as separate columns
MK_hail_days_results %>% 
  select(Month, S, tau, p_value) %>% 
  kable(format = "simple")




mk.test(avg_annual_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)
## MK avg magnitude per year

mk.test(annual_reports$reports, alternative = "two.sided", continuity = TRUE)
## MK annual reports

mk.test(annual_season_length$season_length, alternative = "two.sided", continuity = TRUE)
## season_days per year

mk.test(monthly_reports$reports, alternative = "two.sided", continuity = TRUE)
## monthly reports 

mk.test(avg_monthly_size$avg_magnitude, alternative = "two.sided", continuity = TRUE)

mk.test(annual_hail_days$hail_days, alternative = "two.sided", continuity = TRUE)

mk.test(monthly_hail_days$hail_days, alternative = "two.sided", continuity = TRUE)





### START ###
report_objects <- ls(pattern = "_annual_reports")
annual_reports_list <- mget(report_objects)

county_names <- lapply(annual_reports_list, function(df) unique(df$county_name))


perform_mk_test <- function(df) {
  tryCatch(
    {
      mk.test(df$reports, alternative = "two.sided", continuity = TRUE)
    },
    error = function(e) {
      NA
    }
  )
}

county_MK_results <- lapply(annual_reports_list, perform_mk_test)

county_results <- tibble(
  County = names(annual_reports_list),
  MK_Results = county_MK_results
)

extract_values <- function(result_list) {
  tibble(
    S = result_list$estimate[["S"]],
    VarS = result_list$estimates["varS"],
    tau = result_list$estimates["tau"],
    z = ifelse("z" %in% names(result_list$statistic), result_list$statistic["z"], NA),
    n = result_list$parameter["n"],
    p = result_list$p.value
  )
}

extracted_values <- county_results %>%
  mutate(Values = map(MK_Results, extract_values)) %>%
  select(County, Values) %>%
  unnest(Values)

extracted_values %>% 
  select(County, S, tau, p) %>% 
  kable(format = "simple")

extracted_values %>% 
  select(County, S, tau, p) %>% 
  filter(S == 0)
####### MK RESULTS FOR COUNTY REPORTS PER YEAR #######



### START ###
report_objects_hail_days <- ls(pattern = "_annual_hail_days")
annual_hail_days_list <- mget(report_objects_hail_days)

county_names_hd <- lapply(annual_hail_days_list, function(df) unique(df$county_name))


perform_mk_test_hd <- function(df) {
  tryCatch(
    {
      mk.test(df$hail_days, alternative = "two.sided", continuity = TRUE)
    },
    error = function(e) {
      NA
    }
  )
}

county_MK_hd_results <- lapply(annual_hail_days_list, perform_mk_test_hd)

county_hd_results <- tibble(
  County = names(annual_hail_days_list),
  MK_Results = county_MK_hd_results
)

extract_values <- function(result_list) {
  tibble(
    S = result_list$estimate[["S"]],
    VarS = result_list$estimates["varS"],
    tau = result_list$estimates["tau"],
    z = ifelse("z" %in% names(result_list$statistic), result_list$statistic["z"], NA),
    n = result_list$parameter["n"],
    p = result_list$p.value
  )
}

extracted_values_hd <- county_hd_results %>%
  mutate(Values = map(MK_Results, extract_values)) %>%
  select(County, Values) %>%
  unnest(Values)

extracted_values_hd %>% 
  select(County, S, tau, p) %>% 
  kable(format = "simple")

extracted_values_hd %>% 
  select(County, S, tau, p) %>% 
  filter(S < 0)
####### MK RESULTS FOR COUNTY HAIL DAYS PER YEAR #######


### START ###
size_objects <- ls(pattern = "_annual_avg_size")
annual_size_list <- mget(size_objects)

county_names_size <- lapply(annual_size_list, function(df) unique(df$county_name))

perform_mk_test_size <- function(df) {
  tryCatch(
    {
      mk.test(df$avg_magnitude, alternative = "two.sided", continuity = TRUE)
    },
    error = function(e) {
      NA
    }
  )
}

county_MK_results_size <- lapply(annual_size_list, perform_mk_test_size)

county_results_size <- tibble(
  County = names(annual_size_list),
  MK_Results = county_MK_results_size
)

extract_values <- function(result_list) {
  tibble(
    S = result_list$estimate[["S"]],
    VarS = result_list$estimates["varS"],
    tau = result_list$estimates["tau"],
    z = ifelse("z" %in% names(result_list$statistic), result_list$statistic["z"], NA),
    n = result_list$parameter["n"],
    p = result_list$p.value
  )
}

# Extracting values from MK_Results column
extracted_values_size <- county_results_size %>%
  mutate(Values = map(MK_Results, extract_values)) %>%
  select(County, Values) %>%
  unnest(Values)

extracted_values_size %>% 
  select(County, S, tau, p) %>% 
  kable(format = "simple",
        digits = 2)
###### MK RESULTS COUNTY AVG SIZE PER YEAR ######
extracted_values_size %>% 
  filter(S == 0)


## ANOVA ##
aov(avg_county_size$avg_magnitude ~ avg_county_size$cz_name)
## anova showing difference in magnitude between counties
