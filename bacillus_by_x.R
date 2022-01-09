
## Load libraries

library(rstan)
library(coda)
library(tidyverse)
library(readxl)
library(broom)

## Load data

read_excel("data/Inactivation pathogens2008.xls", sheet = "Bacillus (2)") %>%
    select(Odd, D_val = "D (min)", temp = "T (°C)") %>%
    mutate(logD = log10(D_val)) %>%
    ggplot(aes(x = temp, y = logD, colour = Odd)) +
    geom_point() +
    geom_smooth(method = "lm")


my_data <- read_excel("data/Inactivation pathogens2008.xls", sheet = "Bacillus (2)") %>%
    select(Odd, D_val = "D (min)", temp = "T (°C)") %>%
    filter(Odd == "FALSE") %>%
    mutate(logD = log10(D_val))%>%
    select(temp, logD) %>% 
    na.omit()

## Fit the models

left <- min(my_data$temp, na.rm = TRUE)
right <- max(my_data$temp, na.rm = TRUE)

models_full <- seq(0, 7, length = 5) %>%
    set_names(., .) %>%
    map(.,
        ~ list(lhs = lm(logD ~ temp,
                        data = filter(my_data, temp >= (left + .))
        ),
        rhs = lm(logD ~ temp,
                 data = filter(my_data, temp <= (right - .))
        )
        )
    ) %>%
    map(.,
        ~ map(., tidy)
    ) %>%
    map(.,
        ~ imap_dfr(., ~ mutate(.x, side = .y))
    ) %>%
    imap_dfr(.,
             ~ mutate(.x, size = as.numeric(.y))
    )

# rhs_models <- seq(120, 110, length = 10) %>%
#     set_names(., .) %>%
#     map(.,
#         ~ filter(my_data, temp < .)
#     ) %>%
#     map(.,
#         ~ nls(logD ~ logDref - (temp-103)/z,
#               data = .,
#               start = list(logDref = 0, z = 5)
#         )
#     )
# 
# lhs_models <- seq(85, 95, length = 10) %>%
#     set_names(., .) %>%
#     map(.,
#         ~ filter(my_data, temp > .)
#     ) %>%
#     map(.,
#         ~ nls(logD ~ logDref - (temp-103)/z,
#               data = .,
#               start = list(logDref = 0, z = 5)
#         )
#     )

##

## Figure 6

models_full %>%
    filter(term == "temp") %>%
    ggplot(aes(x = size, y = estimate, colour = side)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error), 
                  width = .1) +
    ylab("Estimated slope (1/ºC)") + xlab("Size of the shift (ºC)") +
    theme_bw() +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12))
    
