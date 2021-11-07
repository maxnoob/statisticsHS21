# Author: Fabian Bürki, Pia Kupferschmied
# Title: Projekt "Deskriptive Statistik und Wahrscheinlichkeitsrechnug"" HS20/21 Medizininformatik
# Date: 14.11.2021

# Load libraries (have to be installed first)
library(tidyverse)
library(readxl)
library(hrbrthemes)
library(ggplot2)
library(viridis)
library(shadowtext)
library(ggrepel)

# Read in list of years from provided Excel file (adjust file path accordingly)
sheet_years <- excel_sheets("~/Desktop/BFH/Deskriptive Statistik/Project/Todesursachen - Männer.xlsx")

# Create list of data frames of both genders of all the years (adjust file paths accordingly)
male_allyears <- lapply(sheet_years, function(x) {as.data.frame(read_excel("~/Desktop/BFH/Deskriptive Statistik/Project/Todesursachen - Männer.xlsx", sheet = x))})
female_allyears <- lapply(sheet_years, function(x) {as.data.frame(read_excel("~/Desktop/BFH/Deskriptive Statistik/Project/Todesursachen - Frauen.xlsx", sheet = x))}) 

# Rename the data frames to years
names(male_allyears) <- sheet_years
names(female_allyears) <- sheet_years

# Clean data frame 1994 (Define new header and rename the old header, clear unnecessary rows)
tbl_hdr <- c("DeathCauses","0-1[","1-14","15-44","45-64","65-84","85+","Total")
names(male_allyears$'1994') <- tbl_hdr
names(female_allyears$'1994') <- tbl_hdr
male_allyears$'1994' <- male_allyears$'1994' %>% filter(!is.na(Total))  # remove all rows with no usable information; needs tidyverse
female_allyears$'1994' <- female_allyears$'1994' %>% filter(!is.na(Total))

# Get subsets of main causes of death
male_main_causes_rel_94 <- male_allyears$'1994'[c(32,35,41,42,47,52:56),c(1:7)]
female_main_causes_rel_94 <- female_allyears$'1994'[c(32,35,41,42,47,52:56),c(1:7)]

# Convert one table (in this case the male one) to long format for ggplot
male_main_causes_rel_94.molten <- melt( male_main_causes_rel_94, id.vars="DeathCauses", value.name="DeathCount", variable.name="AgeGroup" )
male_main_causes_rel_94.molten <- transform(male_main_causes_rel_94.molten, DeathCount = as.numeric(DeathCount)) # convert DeathCount to numeric!!
# not sure if necessary: male_main_causes_rel_94.molten$AgeGroup <- factor(male_main_causes_rel_94.molten$AgeGroup, levels = c(rev((names(male_main_causes_rel_94))[2:8]))) # rearanges AgeGroup

# Draw a stacked bar chart (Graphic 2)
ggplot(data = male_main_causes_rel_94.molten, aes(x = DeathCauses, y = DeathCount, fill = AgeGroup)) +
geom_bar(stat="identity") + coord_flip() + scale_fill_brewer(palette = 12) +
labs(title = "Grafik 2  Verhältnisse von Altersgruppen nach Todesursachen von Männern 1994", y = "Todesfälle pro 100'000 Personen", x = "Todesursachen", fill = "Altersgruppen") +
geom_label_repel(aes(label=DeathCount), show.legend = FALSE, color="white", size=3.5, position=position_stack(vjust = 0.5))

# Convert other table (female) from wide to long format
female_main_causes_rel_94.molten <- melt( female_main_causes_rel_94, id.vars="DeathCauses", value.name="DeathCount", variable.name="AgeGroup" )
female_main_causes_rel_94.molten <- transform(female_main_causes_rel_94.molten, DeathCount = as.numeric(DeathCount)) # convert DeathCount to numeric

# Connect both data frames from 1994 (male & female)
male_main_causes_rel_94.molten$Gender <- 'Male' # adds column for merging
female_main_causes_rel_94.molten$Gender <- 'Female'
female_main_causes_rel_94.molten <- data.frame(lapply(female_main_causes_rel_94.molten, function(x) {
  gsub("Bösartige Tumoren", "Krebskrankheiten (bösartige)", x)})) # replaces strings, because the death cause names are not the same for male & female!
both_rel_94.molten <- merge(female_main_causes_rel_94.molten, male_main_causes_rel_94.molten, all = TRUE) # performs outer merge
both_rel_94.molten <- transform(both_rel_94.molten, DeathCount = as.numeric(DeathCount)) # convert DeathCount to numeric (pervious merging seems to cast char)

# Summarize death causes by age group to compare percentages
unisex_sum_94 <- both_rel_94.molten %>%
  group_by(DeathCauses, AgeGroup) %>%
  summarise(DeathCount = sum(DeathCount))

# Draw a stacked bar chart (Graphic 1)
ggplot(data = unisex_sum_94) +
  geom_col(aes(x= AgeGroup, y = DeathCount, fill= DeathCauses), position = 'fill') + coord_flip() +  scale_fill_brewer(palette="Spectral") +
  labs(title = "Grafik 1  Verhältnisse von Todesursachen nach Altersgruppen von Männern und Frauen 1994", y = "Prozentuale Todesfälle nach Altersgruppe pro 100'000 Personen", x = "Altersgruppen", fill = "Todesursachen")

# Summarize death causes by gender to compare totals
sum_94 <- both_rel_94.molten %>%
  group_by(DeathCauses, Gender) %>%
  summarise(DeathCount = sum(DeathCount))

# Draw a bar chart to compare total male & female death counts  (Graphic 3)
ggplot(data = sum_94, aes(x = DeathCauses, y = DeathCount, fill = factor(Gender))) +
  geom_bar(stat="identity", position = "dodge") + coord_flip() +
  labs(title = "Grafik 3  Unterschied zwischen Frauen und Männer 1994", y = "Todesfälle pro 100'000 Personen", x = "Todesursachen", fill = "Geschlecht") +
  geom_label_repel(aes(label=DeathCount), show.legend = FALSE, color="white", size=3.5, direction="x") +
  scale_fill_discrete(limits = c("Female", "Male"), labels = c("weiblich", "männlich"))

