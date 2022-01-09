
## Load libraries

library(rstan)
library(coda)
library(tidyverse)
library(readxl)
library(broom)

## Simulated data

set.seed(121)

n_papers <- 100
n_points_per_paper <- 6

x_max <- 70
# x_max <- 65
x_min <- 50
z <- 5
my_sd <- 1
# my_sd <- .5

mu_U <- 2
# mu_U <- 1
sd_U <- .25
mu_L <- -2
# mu_L <- -1.2
sd_L <- .25

x_ref <- mean(c(x_max, x_min))

# bad_U <- mu_U - 1
# bad_L <- mu_L + 1

sim_data <- c(1:n_papers) %>%
    map(.,
        ~ tibble(x = runif(n_points_per_paper, x_min, x_max),
                 y =  - (x-x_ref)/z + rnorm(n_points_per_paper, sd = my_sd)
                 )
        ) %>%
    map(.,
        ~ mutate(.,
                 U = rnorm(1, mu_U, sd_U),
                 L = rnorm(1, mu_L, sd_L),
                 sampled = y > L & y < U
                 )
        ) %>%
    imap_dfr(., ~ mutate(.x, paper = .y))

# sim_data <- tibble(x = runif(n_points, 0, x_max),
#                    y =  - x/z + rnorm(n_points, sd = my_sd)) %>%
#     mutate(sampled = between(y, lower_lim, upper_lim))

sim_data %>%
    mutate(up = y > U,
           down = y < L) %>%
    summarize(mean(up), mean(down))

sim_data %>%
    ggplot(aes(x = x, y = y)) +
    geom_point(aes(colour = sampled)) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_smooth(aes(colour = sampled), method = "lm", se = FALSE)

# ggplot(sim_data) +
#     geom_density(aes(L))
# 
# ggplot(sim_data) +
#     geom_density(aes(U))

## Models without truncation

nls_all <- nls(y ~ logDref - (x-x_ref)/z, data = sim_data,
    start = list(logDref = 0, z = 5))

nls_all %>%
    summary()

nls_sampled <- sim_data %>%
    filter(sampled) %>%
    nls(y ~ logDref - (x-x_ref)/z, start = list(logDref = 0, z = 1), data = .) 

nls_sampled %>%
    summary()

## Lineal stan model

set.seed(14212)

d <- sim_data %>%
    filter(sampled)

lin_model <- stan(file = "lineal_model.stan",
                  data = list(temperature = d$x,
                              logD = d$y,
                              refTemp = x_ref,
                              N = nrow(d)
                              )
                  )

print(lin_model)
post_linmodel <- As.mcmc.list(lin_model)

## Basically no truncation

set.seed(7181)

this_U <- sim_data %>%
    filter(sampled) %>%
    pull(y) %>%
    max()

this_L <- sim_data %>%
    filter(sampled) %>%
    pull(y) %>%
    min()

d1 <- sim_data %>%
    filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_1 <- stan(file = "truncated_model.stan",
                data = list(temperature = d1$x,
                            logD = d1$y,
                            refTemp = x_ref,
                            N = nrow(d1),
                            L = this_L, U = this_U),
                iter = 1000, chains = 1, cores = 1
                )

post_model1 <- As.mcmc.list(model_1)
# plot(post_model1)
# pairs(model_1)

print(model_1)
plot(model_1)

## The mean value of U and L

set.seed(91791)

this_U <- mu_U

this_L <- mu_L

d2 <- sim_data %>%
    filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_2 <- stan(file = "truncated_model.stan",
                data = list(temperature = d2$x,
                            logD = d2$y,
                            refTemp = x_ref,
                            N = nrow(d2),
                            L = this_L, U = this_U),
                iter = 1000, chains = 1, cores = 1
                )

post_model2 <- As.mcmc.list(model_2)
# plot(post_model2)
# pairs(model_2)

print(model_2)
plot(model_2)

## The mean value of U, practically no L

set.seed(91791)

this_U <- mu_U

this_L <- sim_data %>%
    filter(sampled) %>%
    pull(y) %>%
    min()

d2 <- sim_data %>%
    filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_3 <- stan(file = "truncated_model.stan",
                data = list(temperature = d2$x,
                            logD = d2$y,
                            refTemp = x_ref,
                            N = nrow(d2),
                            L = this_L, U = this_U),
                iter = 1000, chains = 1, cores = 1
)

post_model3 <- As.mcmc.list(model_3)
# plot(post_model2)
# pairs(model_2)

print(model_3)

## The mean value of L, practically no U

set.seed(91791)

this_U <- sim_data %>%
    filter(sampled) %>%
    pull(y) %>%
    max()

this_L <- mu_L

d2 <- sim_data %>%
    filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_4 <- stan(file = "truncated_model.stan",
                data = list(temperature = d2$x,
                            logD = d2$y,
                            refTemp = x_ref,
                            N = nrow(d2),
                            L = this_L, U = this_U),
                iter = 1000, chains = 1, cores = 1
)

post_model4 <- As.mcmc.list(model_4)
# plot(post_model2)
# pairs(model_2)

print(model_4)


## Too much on both

set.seed(91791)

this_U <- 1

this_L <- -1

# this_U <- .5
# 
# this_L <- -.5

d2 <- sim_data %>%
    filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_5 <- stan(file = "truncated_model.stan",
                data = list(temperature = d2$x,
                            logD = d2$y,
                            refTemp = x_ref,
                            N = nrow(d2),
                            L = this_L, U = this_U),
                iter = 2000, chains = 1, cores = 1
)

post_model5 <- As.mcmc.list(model_5)
# plot(post_model2)
# pairs(model_2)

print(model_5)


################################

## Lineal stan model

set.seed(14212)

d <- sim_data 

lin_model_all <- stan(file = "lineal_model.stan",
                  data = list(temperature = d$x,
                              logD = d$y,
                              refTemp = x_ref,
                              N = nrow(d)
                  )
)

print(lin_model_all)
post_linmodel_all <- As.mcmc.list(lin_model_all)

## Basically no truncation

set.seed(7181)

this_U <- sim_data %>%
    # filter(sampled) %>%
    pull(y) %>%
    max()

this_L <- sim_data %>%
    # filter(sampled) %>%
    pull(y) %>%
    min()

d1 <- sim_data %>%
    # filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_1_all <- stan(file = "truncated_model.stan",
                data = list(temperature = d1$x,
                            logD = d1$y,
                            refTemp = x_ref,
                            N = nrow(d1),
                            L = this_L, U = this_U),
                iter = 1000, chains = 1, cores = 1
)

post_model1_all <- As.mcmc.list(model_1_all)
# plot(post_model1)
# pairs(model_1)

print(model_1_all)
plot(model_1_all)

## The mean value of U and L

set.seed(91791)

this_U <- mu_U

this_L <- mu_L

d2 <- sim_data %>%
    # filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_2_all <- stan(file = "truncated_model.stan",
                data = list(temperature = d2$x,
                            logD = d2$y,
                            refTemp = x_ref,
                            N = nrow(d2),
                            L = this_L, U = this_U),
                iter = 1000, chains = 1, cores = 1
)

post_model2_all <- As.mcmc.list(model_2_all)
# plot(post_model2)
# pairs(model_2)

print(model_2_all)
plot(model_2_all)

## The mean value of U, practically no L

set.seed(91791)

this_U <- mu_U

this_L <- sim_data %>%
    # filter(sampled) %>%
    pull(y) %>%
    min()

d2 <- sim_data %>%
    # filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_3_all <- stan(file = "truncated_model.stan",
                data = list(temperature = d2$x,
                            logD = d2$y,
                            refTemp = x_ref,
                            N = nrow(d2),
                            L = this_L, U = this_U),
                iter = 1000, chains = 1, cores = 1
)

post_model3_all <- As.mcmc.list(model_3_all)
# plot(post_model2)
# pairs(model_2)

print(model_3_all)

## The mean value of L, practically no U

set.seed(91791)

this_U <- sim_data %>%
    # filter(sampled) %>%
    pull(y) %>%
    max()

this_L <- mu_L

d2 <- sim_data %>%
    # filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_4_all <- stan(file = "truncated_model.stan",
                data = list(temperature = d2$x,
                            logD = d2$y,
                            refTemp = x_ref,
                            N = nrow(d2),
                            L = this_L, U = this_U),
                iter = 1000, chains = 1, cores = 1
)

post_model4_all <- As.mcmc.list(model_4_all)
# plot(post_model2)
# pairs(model_2)

print(model_4_all)


## Too much on both

set.seed(91791)

this_U <- 1

this_L <- -1

# this_U <- .5
# 
# this_L <- -.5

d2 <- sim_data %>%
    # filter(sampled) %>%
    filter(y > this_L,
           y < this_U)

model_5_all <- stan(file = "truncated_model.stan",
                data = list(temperature = d2$x,
                            logD = d2$y,
                            refTemp = x_ref,
                            N = nrow(d2),
                            L = this_L, U = this_U),
                iter = 2000, chains = 1, cores = 1
)

post_model5_all <- As.mcmc.list(model_5_all)
# plot(post_model2)
# pairs(model_2)

print(model_5_all)


############################



# Compare model parameters

list(
    `Linear` = post_linmodel[[1]],
    `Truncated I` = post_model1[[1]],
    `Truncated II` = post_model2[[1]],
    `Truncated III` = post_model3[[1]],
    `Truncated IV` = post_model4[[1]],
    `Truncated V` = post_model5[[1]]
    ) %>%
    map(as.data.frame) %>%
    map(.,
        ~ select(., -lp__)
        ) %>%
    map(.,
        ~ pivot_longer(., everything(), names_to = "var")
        ) %>%
    map(.,
        ~ group_by(., var)
        ) %>%
    map(.,
        ~ summarize(.,
                    m_value = median(value),
                    q10 = quantile(value, probs = .1),
                    q90 = quantile(value, probs = .9)
                    )
        ) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    ggplot(aes(x = var, y = m_value, colour = model)) +
    geom_point(position = position_dodge(.5)) +
    geom_errorbar(aes(ymin = q10, ymax = q90),
                  position = position_dodge(.5), width = 0
                  ) +
    coord_flip() +
    facet_wrap(~var, scales = "free")

list(
    `Linear` = post_linmodel_all[[1]],
    `Truncated I` = post_model1_all[[1]],
    `Truncated II` = post_model2_all[[1]],
    `Truncated III` = post_model3_all[[1]],
    `Truncated IV` = post_model4_all[[1]],
    `Truncated IV` = post_model5_all[[1]]
) %>%
    map(as.data.frame) %>%
    map(.,
        ~ select(., -lp__)
    ) %>%
    map(.,
        ~ pivot_longer(., everything(), names_to = "var")
    ) %>%
    map(.,
        ~ group_by(., var)
    ) %>%
    map(.,
        ~ summarize(.,
                    m_value = median(value),
                    q10 = quantile(value, probs = .1),
                    q90 = quantile(value, probs = .9)
        )
    ) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    ggplot(aes(x = var, y = m_value, colour = model)) +
    geom_point(position = position_dodge(.5)) +
    geom_errorbar(aes(ymin = q10, ymax = q90),
                  position = position_dodge(.5), width = 0
    ) +
    coord_flip() +
    facet_wrap(~var, scales = "free")

## Figure 5

aa <- list(
    `no truncation` = post_linmodel[[1]],
    `by max range` = post_model1[[1]],
    # `Truncated II` = post_model2[[1]],
    `only U` = post_model3[[1]],
    `only L` = post_model4[[1]],
    `narrow` = post_model5[[1]]
) %>%
    map(as.data.frame) %>%
    map(.,
        ~ select(., -lp__)
    ) %>%
    map(.,
        ~ pivot_longer(., everything(), names_to = "var")
    ) %>%
    map(.,
        ~ group_by(., var)
    ) %>%
    map(.,
        ~ summarize(.,
                    m_value = median(value),
                    q10 = quantile(value, probs = .05),
                    q90 = quantile(value, probs = .95)
        )
    ) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    mutate(dataset = "Biased data")


list(
    `no truncation` = post_linmodel_all[[1]],
    `by max range` = post_model1_all[[1]],
    # `Truncated II` = post_model2_all[[1]],
    `only U` = post_model3_all[[1]],
    `only L` = post_model4_all[[1]],
    `narrow` = post_model5_all[[1]]
) %>%
    map(as.data.frame) %>%
    map(.,
        ~ select(., -lp__)
    ) %>%
    map(.,
        ~ pivot_longer(., everything(), names_to = "var")
    ) %>%
    map(.,
        ~ group_by(., var)
    ) %>%
    map(.,
        ~ summarize(.,
                    m_value = median(value),
                    q10 = quantile(value, probs = .05),
                    q90 = quantile(value, probs = .95)
        )
    ) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    mutate(dataset = "Complete data") %>%
    # bind_rows(., aa) %>%
    mutate(model = factor(model, 
                          levels = c("no truncation", "by max range", "only U", "only L", "narrow")
    )) %>%
    mutate(
        var = ifelse(var == "logDref", 
                     "log Dref (log min)", 
                     ifelse(var == "z",
                            "z-value (ºC)",
                            "sigma of logD (log min)"))
    ) %>% 
    ggplot(aes(x = model, y = m_value)) + #, colour = dataset)) +
    geom_point(position = position_dodge(.5)) +
    geom_errorbar(aes(ymin = q10, ymax = q90),
                  position = position_dodge(.5), width = 0) +
    geom_hline(aes(yintercept = y), 
               data = tibble(y = c(0, my_sd, z),
                             var = c("log Dref (log min)", "sigma of logD (log min)", "z-value (ºC)")),
               linetype = 2) +
    # facet_wrap(~var, scales = "free", nrow = 3) +
    facet_grid(var~1, scales = "free") +
    # coord_flip() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "top",
          strip.text.x = element_blank(),
          strip.text.y = element_text(size = 14),
          legend.text = element_text(size = 16)
          ) +
    xlab("") + ylab("Parameter estimate") 

##########################

## Finding the right truncation by fixing U to the max

#-  min L

set.seed(91791)

this_L <- sim_data %>%
    filter(sampled) %>%
    pull(y) %>%
    min()

U_models <- seq(.5, 1.5, length = 10) %>%
    set_names(., .) %>%
    map(., 
        ~ filter(sim_data, 
                 sampled,
                 y > this_L,
                 y < .)
        ) %>%
    imap(.,
        ~ stan(file = "truncated_model.stan",
               data = list(temperature = .x$x,
                           logD = .x$y,
                           refTemp = x_ref,
                           N = nrow(.x),
                           L = this_L, U = as.numeric(.y)),
               iter = 2000, chains = 1, cores = 1
        )
        
        )


p1 <- U_models %>%
    map(As.mcmc.list) %>%
    map(., ~ as.data.frame(.[[1]])) %>%
    map(.,
        ~ select(., -lp__)
    ) %>%
    map(.,
        ~ pivot_longer(., everything(), names_to = "var")
    ) %>%
    map(.,
        ~ group_by(., var)
    ) %>%
    map(.,
        ~ summarize(.,
                    m_value = median(value),
                    q10 = quantile(value, probs = .1),
                    q90 = quantile(value, probs = .9)
        )
    ) %>%
    imap_dfr(., ~ mutate(.x, U = as.numeric(.y))) %>%
    ggplot(aes(x = U, y = m_value)) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymin = q10, ymax = q90)) +
        facet_wrap("var", scales = "free") +
        geom_hline(aes(yintercept = y), 
                   data = tibble(y = c(0, my_sd, z),
                                 var = c("logDref", "sigma", "z")),
                   linetype = 2, colour = "red") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "top") +
    xlab("Value of U (ºC)") + ylab("Parameter estimate") 


#- Second step

set.seed(91791)

my_U <- 0.94

L_models <- seq(-1.5, -.5, length = 10) %>%
    set_names(., .) %>%
    map(., 
        ~ filter(sim_data, 
                 sampled,
                 y > .,
                 y < my_U)
    ) %>%
    imap(.,
         ~ stan(file = "truncated_model.stan",
                data = list(temperature = .x$x,
                            logD = .x$y,
                            refTemp = x_ref,
                            N = nrow(.x),
                            L = as.numeric(.y), U = my_U),
                iter = 2000, chains = 1, cores = 1
         )
         
    )

p2 <- L_models %>%
    map(As.mcmc.list) %>%
    map(., ~ as.data.frame(.[[1]])) %>%
    map(.,
        ~ select(., -lp__)
    ) %>%
    map(.,
        ~ pivot_longer(., everything(), names_to = "var")
    ) %>%
    map(.,
        ~ group_by(., var)
    ) %>%
    map(.,
        ~ summarize(.,
                    m_value = median(value),
                    q10 = quantile(value, probs = .1),
                    q90 = quantile(value, probs = .9)
        )
    ) %>%
    imap_dfr(., ~ mutate(.x, L = as.numeric(.y))) %>%
    ggplot(aes(x = L, y = m_value)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = q10, ymax = q90)) +
    facet_wrap("var", scales = "free") +
    geom_hline(aes(yintercept = y), 
               data = tibble(y = c(0, my_sd, z),
                             var = c("logDref", "sigma", "z")),
               linetype = 2, colour = "red")  +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "top") +
    xlab("Value of L (ºC)") + ylab("Parameter estimate") 

#- Last step

set.seed(91791)

my_L <- -0.72

U_models_2 <- seq(.4, 1.2, length = 10) %>%
    set_names(., .) %>%
    map(., 
        ~ filter(sim_data, 
                 sampled,
                 y > my_L,
                 y < .)
    ) %>%
    imap(.,
         ~ stan(file = "truncated_model.stan",
                data = list(temperature = .x$x,
                            logD = .x$y,
                            refTemp = x_ref,
                            N = nrow(.x),
                            L = my_L, U = as.numeric(.y)),
                iter = 2000, chains = 1, cores = 1
         )
         
    )

p3 <- U_models_2 %>%
    map(As.mcmc.list) %>%
    map(., ~ as.data.frame(.[[1]])) %>%
    map(.,
        ~ select(., -lp__)
    ) %>%
    map(.,
        ~ pivot_longer(., everything(), names_to = "var")
    ) %>%
    map(.,
        ~ group_by(., var)
    ) %>%
    map(.,
        ~ summarize(.,
                    m_value = median(value),
                    q10 = quantile(value, probs = .1),
                    q90 = quantile(value, probs = .9)
        )
    ) %>%
    imap_dfr(., ~ mutate(.x, U = as.numeric(.y))) %>%
    ggplot(aes(x = U, y = m_value)) +
    geom_line() +
    geom_point() +
    geom_errorbar(aes(ymin = q10, ymax = q90)) +
    facet_wrap("var", scales = "free") +
    geom_hline(aes(yintercept = y), 
               data = tibble(y = c(0, my_sd, z),
                             var = c("logDref", "sigma", "z")),
               linetype = 2, colour = "red")  +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "top") +
    xlab("Value of U (ºC)") + ylab("Parameter estimate") 

## supp Figure 2

cowplot::plot_grid(p1, p2, p3, ncol = 1, labels = "AUTO")









## Compare model predictions

list(
    nls_sampled = nls_sampled,
    nls_all = nls_all
) %>% 
    map(summary) %>%
    map(., ~.$par) %>%
    map(as.data.frame) %>%
    map(., ~ rownames_to_column(., "var")) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    select(var, m_value = Estimate, sd_value = `Std. Error`, model) %>%
    # bind_rows(., pars_trunc) %>%
    filter(var != "sigma") %>%
    split(.$model) %>%
    map(.,
        ~ tibble(x = seq(0, 10, length = 100),
                y = .$m_value[1] - (x-5)/.$m_value[2])
        ) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    ggplot() +
        geom_line(aes(x = x, y = y, colour = model))

n_boots <- 1e3

pars_trunc %>%
    split(.$model) %>%
    map(.,
        ~ tibble(logD = rnorm(n_boots, .$m_value[1], .$sd_value[1]),
                 z = rnorm(n_boots, .$m_value[3], .$sd_value[3]))
        ) %>%
    imap(.,
        ~ mutate(.x, sim = row_number(), model = .y)
        ) %>%
    map(., ~ split(., .$sim)) %>%
    map(., 
         ~ map(.,
            ~ tibble(x = seq(0, 10, length = 101),
                     y = .[["logD"]] - (x-5)/.[["z"]])
            )
        ) %>%
    map(.,
        ~ imap_dfr(.,
                   ~ mutate(.x, sim = .y)
                   )
        ) %>%
    imap_dfr(., 
             ~ mutate(.x, model = .y)
             ) %>%
    group_by(model, x) %>%
    summarize(m_y = mean(y),
              q10 = quantile(y, probs = .1),
              q90 = quantile(y, probs = .9)
              ) %>%
    ggplot(aes(x = x, y = m_y)) +
        geom_line() +
        geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .5) +
        geom_abline(intercept = 0, slope = -1, colour = "blue", size = 1) +
        facet_wrap("model")



# ## Truncated model with the right truncation
# 
# d <- sim_data %>%
#     filter(sampled)
# 
# model_1 <- stan(file = "truncated_model.stan",
#                 data = list(temperature = d$x,
#                             logD = d$y,
#                             refTemp = 0,
#                             N = nrow(d),
#                             L = mu_L, U = mu_U),
#                 iter = 1000, chains = 3, cores = 3
#                 )
# 
# print(model_1)
# plot(model_1)
# 
# post_model1 <- As.mcmc.list(model_1)
# plot(post_model1)
# pairs(model_1)
# 
# 
# ## Truncated model with too low U truncation
# 
# d2 <- sim_data %>% 
#     filter(sampled) %>%
#     filter(y < bad_U)
#     
# model_2 <- stan(file = "truncated_model.stan", 
#                 data = list(temperature = d2$x,
#                             logD = d2$y,
#                             refTemp = 0,
#                             N = nrow(d2),
#                             L = lower_lim, U = bad_U),
#                 iter = 1000, chains = 3, cores = 3
# )
# 
# post_model2 <- As.mcmc.list(model_2)
# plot(post_model2)
# pairs(model_2)
# 
# print(model_2)
# plot(model_2)
# 
# ## Truncated model with too high L truncation
# 
# d3 <- sim_data %>% 
#     filter(sampled) %>%
#     filter(y > bad_L)
# 
# model_3 <- stan(file = "truncated_model.stan", 
#                 data = list(temperature = d3$x,
#                             logD = d3$y,
#                             refTemp = 0,
#                             N = nrow(d3),
#                             L = bad_L, U = upper_lim),
#                 iter = 1000, chains = 3, cores = 3
# )
# 
# post_model3 <- As.mcmc.list(model_3)
# plot(post_model3)
# pairs(model_3)
# 
# print(model_3)
# plot(model_3)
# 
# ## Truncated model with too low U and too high L truncations
# 
# d4 <- sim_data %>% 
#     filter(sampled) %>%
#     filter(y > bad_L,
#            y < bad_U)
# 
# model_4 <- stan(file = "truncated_model.stan", 
#                 data = list(temperature = d4$x,
#                             logD = d4$y,
#                             refTemp = 0,
#                             N = nrow(d4),
#                             L = bad_L, U = bad_U),
#                 iter = 1000, chains = 1, cores = 1
#                 )
# 
# post_model4 <- As.mcmc.list(model_4)
# plot(post_model4)
# pairs(model_4)
# 
# print(model_4)
# plot(model_4)
#
## Compare model parameters
# 
# list(
#     model1 = post_model1[[1]],
#     model2 = post_model2[[1]],
#     model3 = post_model3[[1]],
#     model4 = post_model4[[1]]
#     ) %>%
#     map(as.data.frame) %>%
#     map(.,
#         ~ select(., -lp__)
#         ) %>%
#     map(.,
#         ~ pivot_longer(., everything(), names_to = "var")
#         ) %>%
#     map(.,
#         ~ group_by(., var)
#         ) %>%
#     map(.,
#         ~ summarize(., 
#                     m_value = mean(value),
#                     q10 = quantile(value, probs = .1),
#                     q90 = quantile(value, probs = .9)
#                     )
#         ) %>%
#     imap_dfr(., ~ mutate(.x, model = .y)) %>%
#     ggplot(aes(x = var, y = m_value, colour = model)) +
#     geom_point(position = position_dodge(.5)) +
#     geom_errorbar(aes(ymin = q10, ymax = q90),
#                   position = position_dodge(.5), width = 0
#                   ) +
#     coord_flip()
























