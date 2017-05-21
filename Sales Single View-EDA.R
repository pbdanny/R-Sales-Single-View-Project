load("os_s_view_da.RData")
library(ggplot2)
library(dplyr)
library(gridExtra)

ggplot(data = os_agent_s_view, aes(x = mob)) +
  geom_histogram(fill = 'blue', alpha = 0.6, binwidth = 5)
table(os_agent_s_view$mob)
c <- os_agent_s_view[os_agent_s_view$mob == 59, ]

ggplot(data = os_agent_s_view, aes(x = mob, color = substr(Source_Code, 1, 1))) +
  geom_histogram(alpha = 0.6, binwidth = 5, fill = NA)


ggplot(data = os_agent_s_view, aes(x = mob, y = avg_straight_mo)) +
  geom_jitter()
