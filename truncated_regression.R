
## Load libraries

library(rethinking)
library(rstan)
library(coda)
library(tidyverse)
library(readxl)
library(broom)
library(cowplot)

## Plots of the linear model

set.seed(1241)

n_sims <- 500

my_sims <- tibble(temperature = runif(n_sims, 50, 70),
       logD = 0  - (temperature-60)/5 + rnorm(n_sims)) %>%
    mutate(sampled_narrow = between(logD, -1.5, 1.5),
           sampled_low = between(logD, -8, 1.5),
           sampled_wide = between(logD, -2.5, 2.5),
           sampled_high = between(logD, -1.5, 8)) 

0 +60/5

p_narrow <- my_sims %>%
    ggplot(aes(x = temperature, y = logD)) +
        geom_point(aes(colour = sampled_narrow), shape = 1) +
        # geom_smooth(method = "lm", se = FALSE, linetype = 5,
        #             colour = "darkgrey", size = 1.5) +
    # geom_abline(slope = -1/5, intercept = 12,
    #             linetype = 2, size = 1.5) +
        geom_smooth(method = "lm", se = FALSE, linetype = 1,
                    data = filter(my_sims, sampled_narrow), size = 1.5) +
        scale_colour_manual(values = c("grey", "black")) +
        theme(legend.position = "none") +
        xlab("Temperature (ºC)") + ylab("D-value (log min)")
    

p_wide <- my_sims %>%
    ggplot(aes(x = temperature, y = logD)) +
    geom_point(aes(colour = sampled_wide), shape = 1) +
    # geom_smooth(method = "lm", se = FALSE, linetype = 5,
    #             colour = "darkgrey", size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, linetype = 1,
                data = filter(my_sims, sampled_wide), size = 1.5) +
    scale_colour_manual(values = c("grey", "black")) +
    theme(legend.position = "none") +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")

p_low <- my_sims %>%
    ggplot(aes(x = temperature, y = logD)) +
    geom_point(aes(colour = sampled_low), shape = 1) +
    # geom_smooth(method = "lm", se = FALSE, linetype = 5,
    #             colour = "darkgrey", size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, linetype = 1,
                data = filter(my_sims, sampled_low), size = 1.5) +
    scale_colour_manual(values = c("grey", "black")) +
    theme(legend.position = "none") +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")

p_high <- my_sims %>%
    ggplot(aes(x = temperature, y = logD)) +
    geom_point(aes(colour = sampled_high), shape = 1) +
    # geom_smooth(method = "lm", se = FALSE, linetype = 5,
    #             colour = "darkgrey", size = 1.5) +
    geom_smooth(method = "lm", se = FALSE, linetype = 1,
                data = filter(my_sims, sampled_high), size = 1.5) +
    scale_colour_manual(values = c("grey", "black")) +
    theme(legend.position = "none") +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")

plot_grid(p_narrow, p_wide, p_low, p_high, labels = "AUTO", nrow = 2)

ggplot(my_sims, aes(x = temperature, y = logD)) +
    geom_point() +
    geom_smooth()

# ggplot(aes(x = temperature, y = logD)) +
#     geom_point(aes(colour = sampled)) +
#     geom_smooth(method = "lm", se = FALSE) +
#     geom_smooth(aes(colour = sampled), method = "lm", se = FALSE)

## Parameter estimates

my_sims %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
    start = list(logDref = 0, z = 5)) %>% 
    summary()

my_sims %>%
    filter(sampled_narrow) %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5)) %>% 
    summary()

my_sims %>%
    filter(sampled_wide) %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5)) %>% 
    summary()

my_sims %>%
    filter(sampled_low) %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5)) %>% 
    summary()


my_sims %>%
    filter(sampled_high) %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5)) %>% 
    summary()

## Model predictions

my_sims %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5))  %>%
    predict(., newdata = list(temperature = 70)) %>%
    10^.*60

my_sims %>%
    filter(sampled_narrow) %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5))  %>%
    predict(., newdata = list(temperature = 70)) %>%
    10^.*60


my_sims %>%
    filter(sampled_wide) %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5))  %>%
    predict(., newdata = list(temperature = 70)) %>%
    10^.*60

my_sims %>%
    filter(sampled_high) %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5))  %>%
    predict(., newdata = list(temperature = 70)) %>%
    10^.*60

my_sims %>%
    filter(sampled_low) %>%
    nls(logD ~ logDref -  (temperature -60)/z, data = ., 
        start = list(logDref = 0, z = 5))  %>%
    predict(., newdata = list(temperature = 70)) %>%
    10^.*60

10^(-10/5)

## Truncated models

# mutate(sampled_1 = between(logD, -1.5, 1.5),
#        sampled_2 = between(logD, -8, 1.5),
#        sampled_3 = between(logD, -2.5, 2.5),
#        sampled_4 = between(logD, -1.5, 8)) 

set.seed(120341)

d_wide <- my_sims %>%
    filter(between(logD, -2.5, 2.5))

trung_model_wide <- stan(file = "truncated_model.stan",
                     data = list(temperature = d_wide$temperature,
                                 logD = d_wide$logD,
                                 refTemp = 60,
                                 N = nrow(d_wide),
                                 L = -2.5, U = 2.5),
                     iter = 4000, chains = 1, cores = 1
                     )

d_narrow <- my_sims %>%
    filter(between(logD, -1.5, 1.5))

trung_model_narrow <- stan(file = "truncated_model.stan",
                     data = list(temperature = d_narrow$temperature,
                                 logD = d_narrow$logD,
                                 refTemp = 60,
                                 N = nrow(d_narrow),
                                 L = -1.5, U = 1.5),
                     iter = 4000, chains = 1, cores = 1
)

d_low <- my_sims %>%
    filter(between(logD, -8, 1.5))

this_L <- min(d_low$logD)

trung_model_low <- stan(file = "truncated_model.stan",
                     data = list(temperature = d_low$temperature,
                                 logD = d_low$logD,
                                 refTemp = 60,
                                 N = nrow(d_low),
                                 L = this_L, U = 1.5),
                     iter = 4000, chains = 1, cores = 1
)


d_high <- my_sims %>%
    filter(between(logD, -1.5, 8))

this_U <- max(d_high$logD)

trung_model_high <- stan(file = "truncated_model.stan",
                     data = list(temperature = d_high$temperature,
                                 logD = d_high$logD,
                                 refTemp = 60,
                                 N = nrow(d_high),
                                 L = -1.5, U = this_U),
                     iter = 4000, chains = 1, cores = 1
)


trung_model_wide
trung_model_narrow
trung_model_high
trung_model_low

##

p_trunc_wide <- summary(trung_model_wide)$summary %>%
    as_tibble(rownames = "par") %>%
    select(par, mean) %>%
    pivot_wider(names_from = "par", values_from = "mean")

p_trunc_narrow <- summary(trung_model_narrow)$summary %>%
    as_tibble(rownames = "par") %>%
    select(par, mean) %>%
    pivot_wider(names_from = "par", values_from = "mean")

p_trunc_high <- summary(trung_model_high)$summary %>%
    as_tibble(rownames = "par") %>%
    select(par, mean) %>%
    pivot_wider(names_from = "par", values_from = "mean")

p_trunc_low <- summary(trung_model_low)$summary %>%
    as_tibble(rownames = "par") %>%
    select(par, mean) %>%
    pivot_wider(names_from = "par", values_from = "mean")


p_wide <- p_wide + 
    geom_segment(aes(x = 50, xend = 70,
                     y = p_trunc_wide$logDref - (50-60)/p_trunc_wide$z,
                     yend = p_trunc_wide$logDref - (70-60)/p_trunc_wide$z),
                 size = 1.5, colour = "darkred"
                 ) +
    geom_abline(slope = -1/5, intercept = 12,
                linetype = 5, size = 1.5,
                colour = "black")

p_narrow <- p_narrow + 
    geom_segment(aes(x = 50, xend = 70,
                     y = p_trunc_narrow$logDref - (50-60)/p_trunc_narrow$z,
                     yend = p_trunc_narrow$logDref - (70-60)/p_trunc_narrow$z),
                 size = 1.5, colour = "darkred"
    ) +
    geom_abline(slope = -1/5, intercept = 12,
                linetype = 5, size = 1.5,
                colour = "black") 

p_high <- p_high + 
    geom_segment(aes(x = 50, xend = 70,
                     y = p_trunc_high$logDref - (50-60)/p_trunc_high$z,
                     yend = p_trunc_high$logDref - (70-60)/p_trunc_high$z),
                 size = 1.5, colour = "darkred"
    ) +
    geom_abline(slope = -1/5, intercept = 12,
                linetype = 5, size = 1.5,
                colour = "black") 

p_low <- p_low + 
    geom_segment(aes(x = 50, xend = 70,
                     y = p_trunc_low$logDref - (50-60)/p_trunc_low$z,
                     yend = p_trunc_low$logDref - (70-60)/p_trunc_low$z),
                 size = 1.5, colour = "darkred"
    ) +
    geom_abline(slope = -1/5, intercept = 12,
                linetype = 5, size = 1.5,
                colour = "black") 

# 
#     # geom_smooth(method = "lm", se = FALSE, linetype = 5,
#     #             colour = "black", size = 1.5)
# 
# 
# # p2 <- p2 + geom_segment(aes(x = 50, xend = 70,
# #                       y = -0.02 - (50-60)/5.3,
# #                       yend = -0.02 - (70-60)/5.3),
# #                   size = 1.5, colour = "darkred"
# #                   ) +
# #     geom_smooth(method = "lm", se = FALSE, linetype = 5,
# #                 colour = "black", size = 1.5) 
# 
# p1 <- p1 + geom_segment(aes(x = 50, xend = 70,
#                             y = -0.11 - (50-60)/5.69,
#                             yend = -0.11 - (70-60)/5.69),
#                         size = 1.5, colour = "darkred"
#                         ) +
#     geom_abline(slope = -1/5, intercept = 12,
#                 linetype = 2, size = 1.5) 
#     # geom_smooth(method = "lm", se = FALSE, linetype = 5,
#     #             colour = "black", size = 1.5) 
# 
# p3 <- p3 + geom_segment(aes(x = 50, xend = 70,
#                       y = -0.08 - (50-60)/5.34,
#                       yend = -0.08 - (70-60)/5.34),
#                   size = 1.5, colour = "darkred"
#                   ) +
#     geom_smooth(method = "lm", se = FALSE, linetype = 5,
#                 colour = "black", size = 1.5) 
# 
# p4 <- p4 + geom_segment(aes(x = 50, xend = 70,
#                       y = -0.08 - (50-60)/4.84,
#                       yend = -0.08 - (70-60)/4.84),
#                   size = 1.5, colour = "darkred"
#                   ) +
#     geom_smooth(method = "lm", se = FALSE, linetype = 5,
#                 colour = "black", size = 1.5) 

## Figure 1

plot_grid(p_narrow, p_wide, p_low, p_high, labels = "AUTO") 

# plot_grid(p1, p2, p3, p4, labels = "AUTO") 

# ## Truncated regression with simulated data
# 
# sim_data <- tibble(temperature = runif(1000, 40, 60),
#        logD = 10  - temperature/5 + rnorm(1000)) %>%
#     mutate(sampled = between(logD, -2, 2))
# 
# 
# sim_data %>%
#     ggplot(aes(x = temperature, y = logD)) +
#         geom_point(aes(colour = sampled)) +
#         geom_smooth(method = "lm", se = FALSE) +
#         geom_smooth(aes(colour = sampled), method = "lm", se = FALSE)
# 
# ## Linear model to the actual dataset
# 
# lm_all_data <- stan(file = "linear_model.stan", 
#                  data = list(temperature = sim_data$temperature,
#                                                          logD = sim_data$logD,
#                                                          refTemp = 50,
#                                                          N = 1000),
#                  iter = 1000, chains = 3, cores = 3
#                  )
# 
# plot(lm_all_data)
# 
# post_lm <- As.mcmc.list(lm_all_data)
# plot(post_lm)
# pairs(lm_all_data)
# print(lm_all_data)
# 
# rstan::extract(lm_all_data)
# aa <- as.array(lm_all_data)
# dim(aa)
# 
# # aa[,1,] %>%
# #     as.data.frame() %>%
# #     sample_n(100) %>%
# #     mutate(sim = row_number()) %>%
# #     split(.$sim) %>%
# #     map(., ~ tibble(temp = seq(40, 60, length = 100),
# #                     logD = .[["logDref"]] - (temp - 50)/.[["z"]])) %>%
# #     imap_dfr(., ~ mutate(.x, sim = .y)) %>%
# #     ggplot() +
# #         geom_line(aes(x = temp, y = logD, colour = factor(sim))) +
# #         theme(legend.position = "none") +
# #         geom_point(aes(x = temperature, y = logD), data = sim_data, inherit.aes = FALSE)
# 
# ## Fit of the lm with the truncated data
# 
# trunc_data <- sim_data %>%
#     filter(sampled)
# 
# trunc_N <- nrow(trunc_data)
# 
# lm_trunc_data <- stan(file = "linear_model.stan", 
#                     data = list(temperature = trunc_data$temperature,
#                                 logD = trunc_data$logD,
#                                 refTemp = 50,
#                                 N = trunc_N),
#                     iter = 1000, chains = 3, cores = 3
#                     )
# print(lm_trunc_data)
# plot(lm_trunc_data)
# 
# post_trunc_lm <- As.mcmc.list(lm_trunc_data)
# plot(post_trunc_lm)
# pairs(lm_trunc_data)
# 
# rstan::extract(lm_trunc_data)
# post_lm_trunc <- as.array(lm_trunc_data)
# 
# rib_lm_trunc <- post_lm_trunc[,1,] %>%
#     as.data.frame() %>%
#     sample_n(100) %>%
#     mutate(sim = row_number()) %>%
#     split(.$sim) %>%
#     map(., ~ tibble(temp = seq(40, 60, length = 100),
#                     logD = .[["logDref"]] - (temp - 50)/.[["z"]])) %>%
#     imap_dfr(., ~ mutate(.x, sim = .y)) %>%
#     group_by(temp) %>%
#     summarize(m_logD = median(logD), q10 = quantile(logD, probs = .1), q90 = quantile(logD, probs = .9)) %>%
#     # ggplot() +
#     geom_ribbon(aes(x = temp, ymin = q10, ymax = q90), alpha = .5, fill = "blue", data = .)
#     # ggplot() +
#     # geom_line(aes(x = temp, y = logD, colour = factor(sim))) +
#     # theme(legend.position = "none") +
#     # geom_point(aes(x = temperature, y = logD), data = sim_data, inherit.aes = FALSE)
# 
# rib_lm_all <- aa[,1,] %>%
#     as.data.frame() %>%
#     sample_n(100) %>%
#     mutate(sim = row_number()) %>%
#     split(.$sim) %>%
#     map(., ~ tibble(temp = seq(40, 60, length = 100),
#                     logD = .[["logDref"]] - (temp - 50)/.[["z"]])) %>%
#     imap_dfr(., ~ mutate(.x, sim = .y)) %>%
#     group_by(temp) %>%
#     summarize(m_logD = median(logD), q10 = quantile(logD, probs = .1), q90 = quantile(logD, probs = .9)) %>%
#     # ggplot() +
#     geom_ribbon(aes(x = temp, ymin = q10, ymax = q90), alpha = .5, fill = "red", data = .)
# 
# ggplot() +
#     rib_lm_all + 
#     rib_lm_trunc +
#     geom_point(aes(x = temperature, y = logD, colour = sampled), data = sim_data, inherit.aes = FALSE)
# 
# 
# ## Fit of the truncated model
# 
# trunc_data <- sim_data %>%
#     filter(sampled)
# 
# trunc_N <- nrow(trunc_data)
# 
# lmT_trunc_data <- stan(file = "truncated_model.stan", 
#                       data = list(temperature = trunc_data$temperature,
#                                   logD = trunc_data$logD,
#                                   refTemp = 50,
#                                   N = trunc_N,
#                                   L = -2, U = 2),
#                       iter = 1000, chains = 3, cores = 3
# )
# 
# print(lmT_trunc_data)
# plot(lmT_trunc_data)
# 
# post_trunc_lmT <- As.mcmc.list(lmT_trunc_data)
# plot(post_trunc_lmT)
# pairs(lmT_trunc_data)
# 
# post_lmT_trunc <- as.array(lmT_trunc_data)
# 
# rib_lmT_trunc <- post_lmT_trunc[,1,] %>%
#     as.data.frame() %>%
#     sample_n(100) %>%
#     mutate(sim = row_number()) %>%
#     split(.$sim) %>%
#     map(., ~ tibble(temp = seq(40, 60, length = 100),
#                     logD = .[["logDref"]] - (temp - 50)/.[["z"]])) %>%
#     imap_dfr(., ~ mutate(.x, sim = .y)) %>%
#     group_by(temp) %>%
#     summarize(m_logD = median(logD), q10 = quantile(logD, probs = .1), q90 = quantile(logD, probs = .9)) %>%
#     # ggplot() +
#     geom_ribbon(aes(x = temp, ymin = q10, ymax = q90), alpha = .5, fill = "green", data = .)
# 
# 
# ggplot() +
#     rib_lm_all + 
#     rib_lm_trunc +
#     rib_lmT_trunc +
#     geom_point(aes(x = temperature, y = logD, colour = sampled), data = sim_data, inherit.aes = FALSE)
# 
# 
# 
# ## Fit of the truncated model with unknown limits
# 
# unkT_trunc <- stan(file = "truncated_unkT.stan", 
#                        data = list(temperature = trunc_data$temperature,
#                                    logD = trunc_data$logD,
#                                    refTemp = 50,
#                                    N = trunc_N),
#                        iter = 1000, chains = 3, cores = 3
# )
# 
# print(unkT_trunc)
# plot(unkT_trunc)
# 
# post_trunc_unkT <- As.mcmc.list(unkT_trunc)
# plot(post_trunc_unkT)
# pairs(unkT_trunc)
# 
# post_unkT <- as.array(unkT_trunc)
# 
# rib_unkT <- post_unkT[,1,] %>%
#     as.data.frame() %>%
#     sample_n(100) %>%
#     mutate(sim = row_number()) %>%
#     split(.$sim) %>%
#     map(., ~ tibble(temp = seq(40, 60, length = 100),
#                     logD = .[["logDref"]] - (temp - 50)/.[["z"]])) %>%
#     imap_dfr(., ~ mutate(.x, sim = .y)) %>%
#     group_by(temp) %>%
#     summarize(m_logD = median(logD), q10 = quantile(logD, probs = .1), q90 = quantile(logD, probs = .9)) %>%
#     # ggplot() +
#     geom_ribbon(aes(x = temp, ymin = q10, ymax = q90), alpha = .5, fill = "purple", data = .)
# 
# 
# ggplot() +
#     rib_lm_all + 
#     rib_lm_trunc +
#     # rib_lmT_trunc +
#     rib_unkT +
#     geom_point(aes(x = temperature, y = logD, colour = sampled), data = sim_data, inherit.aes = FALSE)
# 
# ## Truncated regression for Ecoli
# 
# ## Load data
# 
# my_micro <-  c("Bacillus", "Campylobacter", "C botulinum", "C perfringens", "E sakazaki",
#                "E coli", "Listeria", "Salmonella", "Shigella", "Staphaureus", "Streptococcus",
#                "Vibrio", "Yersinia")
# aa <- my_micro %>%
#     map(., ~ read_excel("data/Inactivation pathogens2008.xls", sheet = .)) %>%
#     set_names(my_micro)
# # map(., ~ select(., Microorganism, Species))
# 
# aa %>%
#     map(names) %>%
#     unlist() %>%
#     tibble(x = .) %>%
#     group_by(x) %>%
#     summarize(n = n()) %>% 
#     filter(n < 13) %>%
#     arrange((n))
# 
# all_data <- c("Bacillus", "Campylobacter", "C botulinum", "C perfringens", "E sakazaki",
#               "E coli", "Listeria", "Salmonella", "Shigella", "Staphaureus", "Streptococcus",
#               "Vibrio", "Yersinia") %>%
#     map(., ~ read_excel("data/Inactivation pathogens2008.xls", sheet = .)) %>%
#     map(., ~ select(., "Microorganism", "Species", "Strain", "Product", "Product group", temp = "T (°C)",
#                     "pH", "Treatment", D_val = "D (min)", z_val = "z (°C)", "Ref", "Year", "Remarks",
#                     starts_with("aw"))
#     ) %>%
#     map(., ~ mutate(., 
#                     Year = as.character(Year),
#                     Strain = as.character(Strain),
#                     temp = as.numeric(temp),
#                     response = "inactivation",
#                     model_type = "primary"
#     )
#     ) 
# 
# all_data %>%
#     map_dfr(., ~ select(., Microorganism, Species, Product, temp, D_val)) %>%
#     filter(!is.na(Microorganism)) %>%
#     ggplot(aes(x = temp, y = log10(D_val), colour = Microorganism)) +
#     geom_point() +
#     geom_smooth(method = "lm") +
#     facet_wrap("Microorganism", scales = "free")
# 
# ## Let's look at E. coli
# 
# my_data <- all_data[[6]] %>%
#     mutate(logD = log10(D_val)) # %>%
# # mutate(highD = logD >= 1.3, highT  = temp >= 70) %>% 
# # mutate(weird = as.logical(highD*highT))
# 
# my_data %>%
#     ggplot(aes(x = temp, y = logD)) +
#     geom_point() +
#     geom_smooth(method = "lm") # +
# # geom_smooth(aes(colour = weird), method = "lm")
# 
# # data_salm %>%
# #     filter(!weird) %>%
# #     mutate(temp_level = cut(temp, breaks = seq(40, 80, by = 10))) %>%
# #     ggplot() +
# #         geom_density(aes(logD, colour = temp_level)) # +
# #         # geom_boxplot(aes(x = temp_level, y = logD)) +
# #         # scale_y_log10()
# 
# my_data %>%
#     # filter(!weird) %>%
#     group_by(temp) %>%
#     summarize(n = n()) %>%
#     arrange(desc(n))
# 
# my_data %>%
#     # filter(!weird) %>%
#     group_by(temp) %>%
#     mutate(n = n()) %>%
#     filter(n >= 25) %>%
#     ggplot() +
#     geom_density(aes(logD, colour = factor(temp)), kernel = "gaussian") +
#     facet_wrap("temp")
# 
# 
# my_data %>%
#     filter(temp > 53) %>%
#     ggplot(aes(x = temp, y = logD)) +
#     geom_point() +
#     geom_smooth(method = "lm") # +
# 
# 
# my_data %>%
#     filter(temp > 53) %>%
#     mutate(res = residuals(lm(logD ~ temp, data = .))) %>%
#     filter(res < 0) %>%
#     ggplot(aes(x = temp, y = res)) +
#     geom_point()  +
#     geom_smooth(method = "lm")
# 
# d <- my_data %>%
#     filter(temp > 53, logD < 2, logD > -1)
# 
# Ecoli_lm_trunc <- stan(file = "truncated_model.stan", 
#                        data = list(temperature = d$temp,
#                                    logD = d$logD,
#                                    refTemp = 50,
#                                    N = nrow(d),
#                                    L = -1, U = 2),
#                        iter = 1000, chains = 3, cores = 3
#                        )
# 
# print(Ecoli_lm_trunc)
# plot(Ecoli_lm_trunc)
# 
# post_Ecoli_trunc <- As.mcmc.list(Ecoli_lm_trunc)
# plot(post_Ecoli_trunc)
# pairs(Ecoli_lm_trunc)
# 
# post_Ecoli_trunc <- as.array(Ecoli_lm_trunc)
# 
# post_Ecoli_trunc[,1,] %>%
#     as.data.frame() %>%
#     sample_n(100) %>%
#     mutate(sim = row_number()) %>%
#     split(.$sim) %>%
#     map(., ~ tibble(temp = seq(40, 60, length = 100),
#                     logD = .[["logDref"]] - (temp - 50)/.[["z"]])) %>%
#     imap_dfr(., ~ mutate(.x, sim = .y)) %>%
#     group_by(temp) %>%
#     summarize(m_logD = median(logD), q10 = quantile(logD, probs = .1), q90 = quantile(logD, probs = .9)) %>%
#     ggplot() +
#     geom_ribbon(aes(x = temp, ymin = q10, ymax = q90), alpha = .5, fill = "purple")
# 
# my_data %>%
#     filter(temp > 53) %>%
#     nls(logD ~ logDref -  (temp -50)/z, data = ., 
#         start = list(logDref = 2, z = 5)) %>% 
#         summary()







