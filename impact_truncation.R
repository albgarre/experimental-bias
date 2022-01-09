
library(tidyverse)
library(broom)
library(GGally)

## Parameters

n_sims <- 3000
x_min <- 50
x_max <- 70
z <- 5
x_ref <- mean(c(x_min, x_max))
# x_ref <- 0

## Generate the simulations

simulate_review <- function(n_papers, n_points_per_paper, 
                            x_min, x_max, z, my_sd,
                            mu_U, sd_U, mu_L, sd_L) {
    
    c(1:n_papers) %>%
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
    
}

set.seed(9716)

sim_parameters <-  tibble(
    n_papers = runif(n_sims, 20, 100), 
    n_points_per_paper = runif(n_sims, 5, 12), 
    x_min = x_min, 
    x_max = x_max, 
    z = z, 
    # my_sd = runif(n_sims, 0.5, 2),
    my_sd = runif(n_sims, 0.2, 1),
    # mu_U = runif(n_sims, 2, 4), 
    mu_U = runif(n_sims, 1, 4),
    sd_U = runif(n_sims, 0.1, 1.5), 
    # mu_L = runif(n_sims, -4, -2), 
    mu_L = runif(n_sims, -4, -1), 
    sd_L = runif(n_sims, 0.1, 1.5)
    ) %>%
    mutate(sim = row_number())

sim_data <- sim_parameters %>%
    split(.$sim) %>%
    map(unlist) %>%
    map(as.list) %>%
    map(.,
        ~ simulate_review(.$n_papers, .$n_points_per_paper, 
                          .$x_min, .$x_max, .$z, .$my_sd,
                          .$mu_U, .$sd_U, .$mu_L, .$sd_L)
        )

sim_data[[9]] %>%
    ggplot() +
        geom_point(aes(x, y, colour = sampled), shape = 1) +
        geom_smooth(aes(x, y), method = "lm", data = filter(sim_data[[9]], sampled),
                    inherit.aes = FALSE, se = FALSE) +
        geom_abline(slope = -1/5, intercept = 60/5) +
        scale_colour_manual(values = c("grey75", "black")) +
        cowplot::theme_cowplot() +
        theme(legend.position = "none") +
        xlab("Temperature (ºC)") + ylab("D-value (log min)")

# sim_data %>%
#     bind_rows() %>%
#     ggplot() +
#         geom_point(aes(x, y, colour = sampled), shape = 1)

sim_data %>%
    map(., ~ filter(., sampled)) %>%
    map_dbl(nrow) %>%
    tibble(x = .) %>%
    # arrange(x)
    # summary()
    ggplot() +
        geom_histogram(aes(x))

## Fit the models

# pars_sampled <- sim_data %>%
#     map(., ~ filter(., sampled)) %>%
#     map(., ~ nls(y ~ logDref - (x-x_ref)/z, start = list(logDref = 0, z = 5), 
#                  data = ., control = list(warnOnly=TRUE))
#     ) %>%
#     map(coefficients) %>%
#     imap_dfr(., ~ tibble(term = names(.x), estimate = .x, sim = .y))
    
pars_sampled <- sim_data %>%
    map(., ~ filter(., sampled)) %>%
    map(., ~ nls(y ~ logDref - (x-x_ref)/z, start = list(logDref = 0, z = 5),
                 data = ., control = list(warnOnly=TRUE))
        ) %>%
    map(tidy) %>%
    imap_dfr(., ~ mutate(.x, sim = .y)) %>%
    mutate(model = "sampled")

pars_all <- sim_data %>%
    map(., ~ nls(y ~ logDref - (x-x_ref)/z, start = list(logDref = 0, z = 5),
                 data = ., control = list(warnOnly=TRUE))
    ) %>%
    map(tidy) %>%
    imap_dfr(., ~ mutate(.x, sim = .y)) %>%
    mutate(model = "all")

pars_sampled %>%
    group_by(term) %>%
    summarize(mean(estimate), median(estimate))

## Figure 2

bind_rows(pars_sampled, pars_all) %>%
    mutate(term = ifelse(term == "z", "z-value (ºC)", "log Dref (min)")) %>%
    ggplot() +
    geom_density(aes(x = estimate, fill = model), alpha = .5,
                 kernel = "triangular") +
    facet_wrap("term", scales = "free") +
    geom_vline(aes(xintercept = x), linetype = 2, colour = "black", size = 1,
               data = tibble(x = c(0, 5), term = c("log Dref (min)", "z-value (ºC)"))) +
    theme_bw(base_size = 14) +
    ylab("Density") +
    xlab("Parameter estimate") +
    theme(legend.position = "none")

# 
# pars_sampled %>%
#     mutate(term = ifelse(term == "z", "z-value (ºC)", "log D-value (min)")) %>%
#     ggplot() +
#     geom_histogram(aes(estimate)) +
#     facet_wrap("term", scales = "free") +
#     geom_vline(aes(xintercept = x), linetype = 2, colour = "red", size = 1,
#                data = tibble(x = c(0, 5), term = c("log D-value (min)", "z-value (ºC)"))) +
#     theme_bw() +
#     ylab("Frequency in the numerical simulations") +
#     xlab("Parameter estimate")

## Correlations between simulation parameters and estimate of z

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, estimate) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    select(-sim) %>%
    ggpairs()

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, estimate) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    # select(z, my_sd) %>% 
    cor() %>%
    as_tibble(rownames = "par") %>%
    select(par, z) %>%
    arrange((abs(z))) %>%
    filter(par != "z") %>%
    mutate(par = factor(par, levels = par)) %>%
    ggplot() +
    geom_col(aes(x = par, y = z)) +
    geom_hline(yintercept = 0, linetype = 2) +
    geom_label(aes(x = par, y = z, label = round(z, 2)), hjust = 0) +
    coord_flip() + 
    xlab("") + ylab("Pearson correlation with the estimate of the z-value") +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size = 16)) +
    ylim(-.7, .7)

## Figure sensitivities

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, estimate) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    select(-sim) %>% 
    cor() %>%
    as.data.frame() %>%
    rownames_to_column("par1") %>%
    gather(par2, cor, -par1) %>%
    filter(par1 == "z") %>%
    filter(par2 != "z") %>%
    arrange(abs(cor)) %>%
    mutate(par2 = factor(par2, levels = par2)) %>%
    ggplot() +
        geom_col(aes(x = par2, y = cor)) +
        coord_flip()

## Figure 3

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, estimate) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    mutate(aa = z < 5) %>%
    summarize(mean(aa))

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, estimate) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    ggplot(., aes(x = my_sd, y = z)) +
        geom_point(shape=1) +
        geom_smooth(se =FALSE, colour = "maroon", size = 1.5) +
        # geom_vline(xintercept = .64, linetype = 2, size = 1,
        #            colour = "red") +
        geom_hline(yintercept = 5, linetype = 1, size = 1.5,
                   colour = "darkgrey") +
        ylab("Estimate of the z-value (ºC)") +
        xlab("Standard deviation of the log D-values (log min)") +
        theme_gray(base_size = 14)

# pars_sampled %>%
#     mutate(sim = as.numeric(sim)) %>%
#     select(sim, term, estimate) %>%
#     pivot_wider(., names_from = term, values_from = estimate) %>%
#     full_join(., 
#               select(sim_parameters, -z, -x_min, -x_max),
#               by = "sim") %>%
#     ggplot(., aes(x = my_sd, y = (z-5)/5*100)) +
#     geom_point() +
#     geom_smooth(se =FALSE) +
#     ylab("Relative error in the estimate of the z-value (%)") +
#     xlab("Standard deviation of the log D-values") +
#     cowplot::theme_cowplot()

pars_all %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, estimate) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    ggplot(., aes(x = my_sd, y = z)) +
    geom_point(shape = 1) +
    geom_smooth()

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, estimate) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    ggplot(., aes(x = mu_U, y = z)) +
    geom_point(shape = 1) +
    geom_smooth()

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, estimate) %>%
    pivot_wider(., names_from = term, values_from = estimate) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    ggplot(., aes(x = mu_L, y = z)) +
    geom_point(shape = 1) +
    geom_smooth()

## Correlation with number of points included

pars_sampled

sim_data %>%
    map(., ~ filter(., sampled)) %>%
    map(nrow) %>%
    imap_dfr(., ~tibble(sim = .y, n_p = .x)) %>%
    full_join(., pars_sampled) %>%
    filter(term == "z") %>%
    ggplot(aes(x = n_p, y = estimate)) +
        geom_point() +
        geom_smooth()

aa <- sim_data %>%
    map(., ~ filter(., sampled)) %>%
    map(nrow) %>%
    imap_dfr(., ~tibble(sim = .y, n_p = .x)) %>%
    full_join(., pars_sampled) %>%
    filter(term == "z") 

cor(aa$n_p, aa$estimate)

bb <- sim_data %>%
    map(., ~ summarize(., prop_in = mean(sampled))) %>%
    imap_dfr(., ~mutate(.x, sim = .y)) %>%
    full_join(., pars_sampled) %>%
    filter(term == "z") 

cor(bb$prop_in, bb$estimate)

## Correlation between simulation parameters and std of z

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, std.error) %>%
    pivot_wider(., names_from = term, values_from = std.error) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    select(-sim) %>%
    ggpairs()

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, std.error) %>%
    pivot_wider(., names_from = term, values_from = std.error) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    filter(z < 0.5) %>%
    ggplot(., aes(x = my_sd, y = z)) +
    geom_point(shape = 1) +
    geom_smooth()

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, std.error) %>%
    pivot_wider(., names_from = term, values_from = std.error) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    filter(z < 0.5) %>%
    ggplot(., aes(x = mu_U, y = z)) +
    geom_point(shape = 1) +
    geom_smooth()

pars_sampled %>%
    mutate(sim = as.numeric(sim)) %>%
    select(sim, term, std.error) %>%
    pivot_wider(., names_from = term, values_from = std.error) %>%
    full_join(., 
              select(sim_parameters, -z, -x_min, -x_max),
              by = "sim") %>%
    filter(z < 0.5) %>%
    ggplot(., aes(x = mu_L, y = z)) +
    geom_point(shape = 1) +
    geom_smooth()


## Resample one of the simulations

aa <- sim_data[[1]] %>%
    filter(sampled)

my_resample <- sample(60:nrow(aa), 1000, replace = TRUE) %>%
    set_names(., .) %>%
    map(.,
        ~ sample_n(aa, ., replace = TRUE)
        ) %>%
    map(., ~ nls(y ~ logDref - (x-x_ref)/z, start = list(logDref = 0, z = 1), data = .)) %>%
    map(tidy) %>%
    imap_dfr(., ~ mutate(.x, n_points = as.numeric(.y)))

ggplot(my_resample) +
    geom_density(aes(x = estimate)) +
    facet_wrap("term", scales = "free")

ggplot(my_resample, aes(x = n_points, y = std.error)) +
    geom_point() +
    geom_smooth() +
    facet_wrap("term", scales = "free")

## Window resampling 

aa <- sim_data %>%
    imap_dfr(., ~ mutate(.x, sim = .y)) %>%
    filter(sampled)

seq(1, 5, length = 40) %>%
    set_names(., .) %>%
    map(.,
        ~ filter(aa, x < 5+., x > 5-.)
        ) %>%
    map(., ~ nls(y ~ logDref - (x-x_ref)/z, start = list(logDref = 0, z = 1), data = .)) %>%
    map(summary) %>%
    imap_dfr(., ~ tibble(window_size = as.numeric(.y)*2, sigma = .x$sigma)) %>%
    ggplot(aes(x = window_size, y = sigma)) +
        geom_point()

bb <- sim_data %>%
    imap_dfr(., ~ mutate(.x, sim = .y))

seq(1, 5, length = 20) %>%
    set_names(., .) %>%
    map(.,
        ~ filter(bb, x < 5+., x > 5-.)
    ) %>%
    map(., ~ nls(y ~ logDref - (x-x_ref)/z, start = list(logDref = 0, z = 1), data = .)) %>%
    map(summary) %>%
    imap_dfr(., ~ tibble(window_size = as.numeric(.y)*2, sigma = .x$sigma)) %>%
    ggplot(aes(x = window_size, y = sigma)) +
    geom_point()











