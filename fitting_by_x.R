
## Load libraries

library(rethinking)
library(rstan)
library(coda)
library(tidyverse)
library(readxl)
library(broom)
library(cowplot)

## Plots of the linear model

n_sims <- 500
left <- 50
right <- 70
ref <- (right + left)/2

set.seed(1241)

my_sims <- tibble(temperature = runif(n_sims, left, right),
                  logD = 0  - (temperature-ref)/5 + rnorm(n_sims)) %>%
    mutate(sampled_1 = between(logD, -1.5, 1.5),
           sampled_2 = between(logD, -8, 1.5),
           sampled_3 = between(logD, -2.5, 2.5),
           sampled_4 = between(logD, -1.5, 8)) 

p0 <- ggplot(my_sims) +
    geom_point(aes(x = temperature, y = logD), shape = 1) +
    scale_colour_manual(values = c("black")) +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
    ) +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")


p1 <- ggplot(my_sims) +
    geom_point(aes(x = temperature, y = logD, colour = sampled_1), shape = 1) +
    scale_colour_manual(values = c("grey", "black")) +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)
          ) +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")
    
    
p2 <- ggplot(my_sims) +
    geom_point(aes(x = temperature, y = logD, colour = sampled_2), shape = 1)  +
    scale_colour_manual(values = c("grey", "black")) +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")

p3 <- ggplot(my_sims) +
    geom_point(aes(x = temperature, y = logD, colour = sampled_3), shape = 1) +
    scale_colour_manual(values = c("grey", "black")) +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")

p4 <- ggplot(my_sims) +
    geom_point(aes(x = temperature, y = logD, colour = sampled_4), shape = 1) +
    scale_colour_manual(values = c("grey", "black")) +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")



plot_grid(p1, p2, p3, p4, labels = "AUTO")

## Models for the whole dataset

models_full <- seq(0, 3, length = 10) %>%
    set_names(., .) %>%
    map(.,
        # ~ list(lhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature > (left + .)),
        #                  start = list(logDref = 0, z = 5)
        #                  ),
        #        rhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature < (right - .)),
        #                  start = list(logDref = 0, z = 5)
        #        )
        #        )
        ~ list(lhs = lm(logD ~ temperature,
                        data = filter(my_sims, temperature >= (left + .))
        ),
        rhs = lm(logD ~ temperature,
                 data = filter(my_sims, temperature <= (right - .))
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

## Models for the data set I

models_I <- seq(0, 3, length = 10) %>%
    set_names(., .) %>%
    map(.,
        # ~ list(lhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature > (left + .)),
        #                  start = list(logDref = 0, z = 5)
        #                  ),
        #        rhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature < (right - .)),
        #                  start = list(logDref = 0, z = 5)
        #        )
        #        )
        ~ list(lhs = lm(logD ~ temperature,
                        data = filter(my_sims, 
                                      temperature >= (left + .),
                                      sampled_1)
        ),
        rhs = lm(logD ~ temperature,
                 data = filter(my_sims, 
                               temperature <= (right - .),
                               sampled_1)
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

## Models for the data set II

models_II <- seq(0, 3, length = 10) %>%
    set_names(., .) %>%
    map(.,
        # ~ list(lhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature > (left + .)),
        #                  start = list(logDref = 0, z = 5)
        #                  ),
        #        rhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature < (right - .)),
        #                  start = list(logDref = 0, z = 5)
        #        )
        #        )
        ~ list(lhs = lm(logD ~ temperature,
                        data = filter(my_sims, 
                                      temperature >= (left + .),
                                      sampled_2)
        ),
        rhs = lm(logD ~ temperature,
                 data = filter(my_sims, 
                               temperature <= (right - .),
                               sampled_2)
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

## Models for the data set III

models_III <- seq(0, 3, length = 10) %>%
    set_names(., .) %>%
    map(.,
        # ~ list(lhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature > (left + .)),
        #                  start = list(logDref = 0, z = 5)
        #                  ),
        #        rhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature < (right - .)),
        #                  start = list(logDref = 0, z = 5)
        #        )
        #        )
        ~ list(lhs = lm(logD ~ temperature,
                        data = filter(my_sims, 
                                      temperature >= (left + .),
                                      sampled_3)
        ),
        rhs = lm(logD ~ temperature,
                 data = filter(my_sims, 
                               temperature <= (right - .),
                               sampled_3)
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

## Models for the data set IV

models_IV <- seq(0, 3, length = 10) %>%
    set_names(., .) %>%
    map(.,
        # ~ list(lhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature > (left + .)),
        #                  start = list(logDref = 0, z = 5)
        #                  ),
        #        rhs = nls(logD ~ logDref - (temperature-60)/z,
        #                  data = filter(my_sims, temperature < (right - .)),
        #                  start = list(logDref = 0, z = 5)
        #        )
        #        )
        ~ list(lhs = lm(logD ~ temperature,
                        data = filter(my_sims, 
                                      temperature >= (left + .),
                                      sampled_4)
        ),
        rhs = lm(logD ~ temperature,
                 data = filter(my_sims, 
                               temperature <= (right - .),
                               sampled_4)
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


## Compare the results

list(full_data = models_full,
     truncation_1 = models_I,
     truncation_2 = models_II,
     truncation_3 = models_III,
     truncation_4 = models_IV) %>%
    imap_dfr(.,
             ~ mutate(.x, data = .y)
             ) %>%
    filter(term == "temperature") %>%
    ggplot(aes(x = size, y = estimate, colour = side)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error)) +
    facet_wrap("data", scales = "free")

p01 <- models_full %>%
    filter(term == "temperature") %>%
    ggplot(aes(x = size, y = estimate, colour = side)) +
    geom_point(shape = 1) +
    geom_line() +
    geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error)) +
    xlab("Size of the shift (ºC)") +
    theme_bw() +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    scale_y_continuous(name = "Estimated slope (1/ºC)",
                       breaks = function(a, b) {
                           seq(a[1], a[2], length = 4)
                       },
                       labels = function(x) format(x, digits = 3, scientific = FALSE)
                       )


p11 <- models_I %>%
    filter(term == "temperature") %>%
    ggplot(aes(x = size, y = estimate, colour = side)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error),
                  width = .1) +
    xlab("Size of the shift (ºC)") +
    theme_bw() +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    scale_y_continuous(name = "Estimated slope (1/ºC)",
                       breaks = function(a, b) {
                           seq(a[1], a[2], length = 4)
                       },
                       labels = function(x) format(x, digits = 2, scientific = FALSE)
    )

p21 <- models_II %>%
    filter(term == "temperature") %>%
    ggplot(aes(x = size, y = estimate, colour = side)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error),
                  width = .1) +
    xlab("Size of the shift (ºC)") +
    theme_bw() +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    scale_y_continuous(name = "Estimated slope (1/ºC)",
                       breaks = function(a, b) {
                           seq(a[1], a[2], length = 4)
                       },
                       labels = function(x) format(x, digits = 3, scientific = FALSE)
    )

p31 <- models_III %>%
    filter(term == "temperature") %>%
    ggplot(aes(x = size, y = estimate, colour = side)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error),
                  width = .1) +
    xlab("Size of the shift (ºC)") +
    theme_bw() +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    scale_y_continuous(name = "Estimated slope (1/ºC)",
                       breaks = function(a, b) {
                           seq(a[1], a[2], length = 4)
                       },
                       labels = function(x) format(x, digits = 3, scientific = FALSE)
    )

p41 <- models_IV %>%
    filter(term == "temperature") %>%
    ggplot(aes(x = size, y = estimate, colour = side)) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = estimate-std.error, ymax = estimate+std.error),
                  width = .1) +
    xlab("Size of the shift (ºC)") +
    theme_bw() +
    theme(legend.position = "none", 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 12)) +
    scale_y_continuous(name = "Estimated slope (1/ºC)",
                       breaks = function(a, b) {
                           seq(a[1], a[2], length = 4)
                       },
                       labels = function(x) format(x, digits = 3, scientific = FALSE)
    )

## Figure 4

plot_grid(p0, p01,
          p1, p11,
          p3, p31, 
          p2, p21,
          p4, p41,
          ncol = 2,
          labels = c("A", NA, "B", NA, "C", NA, "D", NA, "E", NA)
          )





