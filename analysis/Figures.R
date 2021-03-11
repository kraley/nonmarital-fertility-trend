# Non-marital fertility - RKR & KLB

# Read in data from the NCHS and use it to plot non-marital fertility rate among
# women aged 15-29 between the years 2004-2015 by race/ethnicity.
# This will be Figure 1 for our manuscript.

# Read in data
# These data come from the NCHS vital stats, downloaded raw data from the following website: 
# https://healthdata.gov/dataset/nchs-birth-rates-unmarried-women-age-race-and-hispanic-origin-united-states
nmfr <- read.csv(file = (paste0(original, "/cohab_fertility/Tables/NCHS_-_Birth_Rates_for_Unmarried_Women_by_Age__Race__and_Hispanic_Origin__United_States.csv")), 
                 header=TRUE, sep=",")
# Check data
head(nmfr)

# Rename variables for ease of use
nmfr <- nmfr %>%
          rename(year = Year, 
          age = Age.Group,
          race = Race.or.Hispanic.Origin,
          nmfrate = Birth.Rate)

# Filter to get just desired race/ethnicity categories, years, and age groups for plotting
nmfr <- nmfr %>%
          filter(race == "Black total" | race == "Non-Hispanic white" | race == "Hispanic") %>%
          filter(year >= 2003) %>%
          filter(age == "15-19 years" | age == "20-24 years" | age == "25-29 years")

# Manipulate race/ethnicity variable so that it is a factor var that is reordered for faceting in plot
nmfr$race = factor(nmfr$race)
levels(nmfr$race)
# Flag for follow- up: Black total includes or doesn't include B & Hispanic?
# It seems that Black doesn't mean NH Black according to documentation in Table 15 & 16 in Martin et al 2017
# "Rates cannot be computed for unmarried non-Hispanic black women or for American Indian or Alaska
# Native women because the necessary populations are not available"
nmfr$race <- recode_factor(nmfr$race, "Black total" = "Black",
                                      "Non-Hispanic white" = "Non-Hispanic White")
levels(nmfr$race)
# Reorder levels of factor var
nmfr$race <- factor(nmfr$race, levels=c("Non-Hispanic White", "Black", "Hispanic"))
levels(nmfr$race)


# Generate and save plot, faceteted by race
nmfr %>%
  ggplot(aes(x=year, y = nmfrate, group = age)) +
  geom_line(aes(linetype=age)) +
  facet_wrap(~race) +
  ylab("Non-marital fertility rate per 1,000 unmarried women") +
  xlab("Year") +
  labs(linetype = "Age",
       caption = "Source: Martin JA, Hamilton BE, Osterman MJK, et al. Births: Final data for 2015. \nNational vital statistics reports; vol 66 no 1. Hyattsville, MD: National Center for Health Statistics. 2017.") +
  ggtitle("Figure 1. Trends in non-marital fertility rate by race/ethnicity \nand age of mother, 2003-2015") +
  scale_linetype_manual(values=c("solid", "longdash", "dotdash")) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(2003, 2015, by = 3)) +
  scale_y_continuous(breaks = seq(0, 160, by = 20)) +
  ggsave(paste0(results, "/Figure1.nmfr.sepbyrace.pdf"), device = "pdf")

# # Generate and save plot, faceteted by age - I think my preference is for the plot fac
# nmfr %>%
#   ggplot(aes(x=year, y = nmfrate, group = race)) +
#   geom_line(aes(linetype=race)) +
#   facet_wrap(~age) +
#   ylab("Non-marital fertility rate per 1,000 unmarried women") +
#   xlab("Year") +
#   labs(linetype = "Race") +
#   ggtitle("Figure 1. Trends in non-marital fertility rate by race/ethnicity \nand age of mother, 2003-2015") +
#   scale_linetype_manual(values=c("solid", "longdash", "dotdash")) +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   theme(legend.position = "bottom") +
#   scale_x_continuous(breaks = seq(2003, 2015, by = 3)) +
#   scale_y_continuous(breaks = seq(0, 160, by = 20)) +
#   ggsave(paste0(results, "/Figure1.nmfr.sepbyage.pdf"), device = "pdf")


##############################################################
## Figure of pregnancy rates by cp method over study period ##
##############################################################

# line graph plotting pregnancy rate for each method during periods 1, 2, 3
# faceted by age

ratesbymethod <- read_excel(paste0(NSFGKeep, "/pregratesbymethod.xlsx"))
head(ratesbymethod)

# Assign some labels
ratesbymethod$age <- factor(ratesbymethod$age,
                            levels = c(1,2),
                            labels = c("Teens", "Twenties"))
ratesbymethod$period <- factor(ratesbymethod$period,
                               levels = c(1, 2, 3),
                               labels = c("03-06", "07-10", "11-15"))
ratesbymethod$method <- factor(ratesbymethod$method,
                               levels = c(0, 2, 3, 4),
                               labels = c("Overall", "LARC and Hormonal", "Less-effective methods", "No method"))

# FLAG: these are rounding; why? Probably doesn't matter since these aren't really close up graphs, but...
ratesbymethod <- ratesbymethod %>%
                    mutate(ann_pregrate = pregrate * 12 * 1000)
ratesbymethod <- ratesbymethod %>%
                    mutate(ann_se_pregrate = (sqrt(var_pregrate))*12*1000)
ratesbymethod <- ratesbymethod %>%
                    mutate(ann_ci_pregrate = ann_se_pregrate * 1.96)

# Generate plot
ratesbymethod %>%
  ggplot(aes(x=period, y=ann_pregrate, group=method)) + 
  geom_line(aes(linetype=method))+
  labs(linetype = "") +
  ggtitle("Figure 2. Fertile pregnancy rates with standard errors by contraceptive method \namong sexually active Black and Hispanic women, 2003-2015") +
  facet_wrap(~age) +
  ylab("Pregnancy rate per 1,000 women") +
  xlab("Study period") +
  geom_pointrange(aes(ymin=ann_pregrate-ann_se_pregrate, ymax=ann_pregrate+ann_se_pregrate)) +
  scale_linetype_manual(values=c("solid", "longdash", "dotdash", "dotted")) +
  scale_shape_manual(0) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "bottom") +
  ggsave(paste0(results,"/Figure2.pregratesbymethod.sepbyage.pdf"), device = "pdf")

