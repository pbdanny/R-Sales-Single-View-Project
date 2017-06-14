load("os_s_view_da.RData")
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyr)
# EDA on each features ----
# Month on book 

ggplot(data = os_agent_s_view) +
  geom_histogram(mapping = aes(x = mob), fill = 'blue', binwidth = 1)

g1 <- ggplot(data = os_agent_s_view, aes(x = mob)) +
  geom_histogram(mapping = aes(x = mob), fill = 'blue', alpha = 0.6, binwidth = 12)
g2 <- ggplot(data = os_agent_s_view) +
  geom_histogram(mapping = aes(x = mob/12), fill = 'blue', alpha = 0.6, binwidth = 1)

grid.arrange(g1, g2)

# Overlapping mob by SourceCode
ggplot(data = os_agent_s_view) +
  geom_histogram(mapping = aes(x = mob, fill = factor(substr(Source_Code, 1, 1))),
                 alpha = 0.6, binwidth = 1, position = 'identity')

# Cut Source Code start with 'O', summarized count by mob and pointplot by size each mob 
os_agent_s_view %>%
  filter(substr(Source_Code, 1, 1) == 'O') %>%
  select(Source_Code, mob) %>%
  group_by(Source_Code, mob) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_point(mapping = aes(x = mob, y = n, color = Source_Code, size = n))

# Cut Source Code start with 'O', summarized count by mob and pointplot by size each mob 
# Add faceting
os_agent_s_view %>%
  filter(substr(Source_Code, 1, 1) == 'O') %>%
  select(Source_Code, mob) %>%
  group_by(Source_Code, mob) %>%
  summarise(n = n()) %>%
  ggplot() +
  geom_point(mapping = aes(x = mob, y = n, color = Source_Code, size = n)) +
  facet_wrap( ~ Source_Code, nrow = 3)

# Select only source Code OSS
# histogram plot with adjusted axis x ticks
os_agent_s_view %>%
  filter(Source_Code == 'OSS') %>%
  ggplot() +
  geom_histogram(mapping = aes(x = mob), binwidth = 3, color = 'white') +
  scale_x_continuous(name = 'Month On Book', breaks = seq(0, 150, 3))
# Found mob peak @ every 9, 18, 27 mob

# Further investigation on straight month
os_agent_s_view %>%
  filter(Source_Code == 'OSS') %>%
  ggplot() +
  geom_boxplot(mapping = aes(y = max_straight_mo, x = 'max_stra')) +  
  geom_boxplot(mapping = aes(y = avg_straight_mo, x = 'avg_stra')) + 
  geom_boxplot(mapping = aes(y = min_straight_mo, x = 'min_stra')) +
  scale_y_continuous(name = 'Straight Month') +
  coord_cartesian(ylim = c(0, 8))
# found avg str mth not diff from max stra month

# Histogram of avg stra moth and max stra month
# Since data in different column then use grid extra
m <- os_agent_s_view %>%
  filter(Source_Code == 'OSS') %>%
  ggplot() +
  geom_histogram(mapping = aes(x = max_straight_mo), binwidth = 1, fill = 'blue', alpha = 0.6) +
  scale_x_continuous(breaks = seq(1, 40, 1))

a <- os_agent_s_view %>%
  filter(Source_Code == 'OSS') %>%
  ggplot() +
  geom_histogram(mapping = aes(x = avg_straight_mo), binwidth = 1, fill = 'green', alpha = 0.6) +
  scale_x_continuous(breaks = seq(1, 40, 1))

grid.arrange(m, a)

# plot avg stra mo vs mob
os_agent_s_view %>%
  filter(Source_Code == 'OSS') %>%
  ggplot() +
  geom_point(mapping = aes(x = mob, y = avg_straight_mo))

# Find switching profile of MOB > 24
os_agent_s_view %>%
  filter(mob >= 24 & Source_Code == 'OSS') %>%
  ggplot() +
  geom_freqpoly(mapping = aes(x = avg_straight_mo), color = 'blue', binwidth = 1) +
  geom_freqpoly(mapping = aes(x = min_straight_mo), color = 'green',binwidth = 1) +
  geom_freqpoly(mapping = aes(x = max_straight_mo), color = 'red', binwidth = 1) +
  scale_x_continuous(breaks = seq(1, 150, 1))

# Better plot us tidyr help create grouping filter large count straight time >= 4 times
os_agent_s_view %>%
  filter(mob >= 24 & Source_Code == 'OSS' & count_straight_time >= 4) %>%
  select(Agent_Code, min_straight_mo, avg_straight_mo, max_straight_mo) %>%
  gather(str_type, mo, max_straight_mo, min_straight_mo, avg_straight_mo) %>%
  ggplot() +
  geom_freqpoly(mapping = aes(x = mo, color = str_type), binwidth = 1) +
  scale_x_continuous(breaks = seq(1, 150, 1)) +
  coord_cartesian(xlim = c(0, 20))

# Data show after exclude times submite to >= 4 avg_straight_mo show high concentration on range 2 - 4