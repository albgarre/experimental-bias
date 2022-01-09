
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

ggplot(my_data, aes(x = temp, y = logD)) +
    geom_point() +
    # geom_smooth(method = "lm") +
    xlab("Temperature (ºC)") + ylab("D-value (log min)")

my_data %>%
    filter(temp == 100) %>%
    ggplot() + geom_density(aes(logD), fill = "darkgrey") + 
    xlab("D-value (log min)") + ylab("Probability density")


## Lineal stan model

mean(c(max(my_data$temp), min(my_data$temp)))
mean(my_data$temp)
x_ref <- 103

# nls(logD ~ logDref - (temp - x_ref)/z, data = my_data,
#     start = list(logDref = 1, z = 5))

set.seed(14212)

lin_model <- stan(file = "lineal_cereus.stan",
                  data = list(temperature = my_data$temp,
                              logD = my_data$logD,
                              refTemp = x_ref,
                              N = nrow(my_data)
                  )
)

print(lin_model)
post_linmodel <- As.mcmc.list(lin_model)

## Finding the right truncation by fixing U to the max

#-  min L

set.seed(91791)

this_L <- my_data %>%
    pull(logD) %>%
    min()

U_models <- seq(.5, 1.5, length = 10) %>%
    set_names(., .) %>%
    map(., 
        ~ filter(my_data, 
                 logD > this_L,
                 logD < .)
    ) %>%
    imap(.,
         ~ stan(file = "truncated_cereus2.stan",
                data = list(temperature = .x$temp,
                            logD = .x$logD,
                            refTemp = x_ref,
                            N = nrow(.x),
                            L = this_L, U = as.numeric(.y)),
                iter = 2000, chains = 1, cores = 1
         )
         
    )

U_models %>%
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
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "top") +
    xlab("Value of U (log min)") + ylab("Parameter estimate") 


#- Second step

set.seed(91791)

my_U <- 0.72

L_models <- seq(-1.5, -.5, length = 10) %>%
    set_names(., .) %>%
    map(., 
        ~ filter(my_data, 
                 logD > .,
                 logD < my_U)
    ) %>%
    imap(.,
         ~ stan(file = "truncated_cereus2.stan",
                data = list(temperature = .x$temp,
                            logD = .x$logD,
                            refTemp = x_ref,
                            N = nrow(.x),
                            L = as.numeric(.y), U = my_U),
                iter = 2000, chains = 1, cores = 1
         )
         
    )

L_models %>%
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
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "top") +
    xlab("Value of L (log min)") + ylab("Parameter estimate") 

#- Last step

set.seed(91791)

my_L <- -1.4
U_models_2 <- seq(.5, 1.2, length = 10) %>%
    set_names(., .) %>%
    map(., 
        ~ filter(my_data, 
                 logD > my_L,
                 logD < .)
    ) %>%
    imap(.,
         ~ stan(file = "truncated_cereus2.stan",
                data = list(temperature = .x$temp,
                            logD = .x$logD,
                            refTemp = x_ref,
                            N = nrow(.x),
                            L = my_L, U = as.numeric(.y)),
                iter = 2000, chains = 1, cores = 1
         )
         
    )



U_models_2 %>%
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
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14),
          axis.text.y = element_text(size = 14),
          axis.title = element_text(size = 14),
          legend.title = element_blank(),
          legend.position = "top") +
    xlab("Value of U (log min)") + ylab("Parameter estimate") 

## Look at the posteriors

trunc_model <- U_models_2[[7]]
trunc_model
lin_model

set.seed(12412)

list(Truncated = trunc_model,
     Classical = lin_model) %>%
    map(., ~ As.mcmc.list(.)[[1]]) %>%
    map(as.data.frame) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    select(-lp__) %>%
    mutate(logD130 = logDref - (130-x_ref)/z) %>%
    gather(par, value, -model) %>%
    ggplot() +
        geom_density(aes(value, colour = model)) +
        facet_wrap(~par, scales = "free")


list(Truncated = trunc_model,
     Classical = lin_model) %>%
    map(., ~ As.mcmc.list(.)[[1]]) %>%
    map(as.data.frame) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    select(-lp__) %>%
    mutate(logD130 = logDref - (130-x_ref)/z) %>%
    gather(par, value, -model) %>%
    ggplot() +
    geom_boxplot(aes(x = par, y = value, colour = model)) +
    facet_wrap("par", scales = "free") +
    xlab("") + ylab("") +
    theme(legend.title = element_blank())
    

set.seed(12412)

list(Truncated = trunc_model,
     Classical = lin_model) %>%
    map(., ~ As.mcmc.list(.)[[1]]) %>%
    map(as.data.frame) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    select(-lp__) %>%
    # mutate(logD130 = logDref - (130-x_ref)/z) %>%
    gather(par, value, -model) %>%
    group_by(par, model) %>%
    summarize(mean(value), sd(value))

## Figure 7

set.seed(12412)

list(`Truncated model` = trunc_model,
     `Classical model` = lin_model) %>%
    map(., ~ As.mcmc.list(.)[[1]]) %>%
    map(as.data.frame) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    select(-lp__) %>%
    mutate(sim = row_number()) %>%
    split(.$sim) %>%
    map_dfr(.,
            ~ tibble(temp = seq(80, 130, length = 101),
                     logD = .$logDref - (temp - x_ref)/.$z,
                     sigma = .$sigma,
                     model = .$model)
            ) %>%
    group_by(model, temp) %>%
    summarize(m_logD = median(logD),
              q10 = quantile(logD, .1),
              q90 = quantile(logD, .9),
              sigma = mean(sigma)) %>% 
    ggplot(aes(x = temp, colour = model, fill = model)) +
        # geom_point(aes(x = temp, y = logD), inherit.aes = FALSE,
        #            data = my_data, shape = 1) +
        geom_line(aes(y = q10), linetype = 2, size = 1) +
        geom_line(aes(y = q90), linetype = 2, size = 1) +
        geom_line(aes(y = m_logD-1.96*sigma), linetype = 3, size = 1) +
        geom_line(aes(y = m_logD+1.96*sigma), linetype = 3, size = 1) +
        geom_line(aes(y = m_logD), linetype = 1, size = 1) +
        geom_hline(yintercept = c(my_L, 0.97), linetype = 2) +
        geom_point(aes(x = temp, y = logD), shape=1, 
               data = mutate(my_data, included = between(logD, my_L, my_U)), 
               inherit.aes = FALSE) +
        # geom_point(aes(x = temp, y = logD, colour = included), 
        #            data = mutate(my_data, included = between(logD, my_L, my_U)), 
        #            inherit.aes = FALSE) +
        theme_bw() +
        xlab("Temperature (ºC)") + ylab("D-value (log min)") +
        theme(legend.position = "top",
              legend.title = element_blank(),
              axis.text = element_text(size = 14),
              axis.title = element_text(size = 16),
              legend.text = element_text(size = 14)) 

## Sup. Table

set.seed(12412)

list(`Truncated model` = trunc_model,
     `Classical model` = lin_model) %>%
    map(., ~ As.mcmc.list(.)[[1]]) %>%
    map(as.data.frame) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    select(-lp__) %>%
    mutate(sim = row_number()) %>%
    split(.$sim) %>%
    map_dfr(.,
            ~ tibble(temp = seq(80, 140, by = 5),
                     logD = .$logDref - (temp - x_ref)/.$z,
                     sigma = .$sigma,
                     model = .$model)
    ) %>%
    group_by(model, temp) %>%
    summarize(m_logD = median(logD),
              q10 = quantile(logD, .1),
              q90 = quantile(logD, .9),
              sigma = mean(sigma)) %>%
    mutate(`Upper prediction interval (95%)` = m_logD+1.96*sigma,
           `Lower prediction interval (95%)` = m_logD-1.96*sigma,
           ) %>%
    rename(`Modeling approach` = model,
           `Temperature (ºC)` = temp,
           `Expected log D-value (log min)` = m_logD,
           `10th Quantile of the mean log D-value (log min)` = q10,
           `90th Quantile of the mean log D-value (log min)` = q90,
           ) %>%
    select(-sigma) %>% 
    write_excel_csv("sup_Table.csv")
    # View()

# set.seed(12412)
# 
# list(`Truncated model` = trunc_model,
#      `Classical model` = lin_model) %>%
#     map(., ~ As.mcmc.list(.)[[1]]) %>%
#     map(as.data.frame) %>%
#     imap_dfr(., ~ mutate(.x, model = .y)) %>%
#     select(-lp__) %>%
#     mutate(sim = row_number()) %>%
#     split(.$sim) %>%
#     map_dfr(.,
#             ~ tibble(temp = seq(80, 130, length = 101),
#                      logD = .$logDref - (temp - x_ref)/.$z,
#                      sigma = .$sigma,
#                      model = .$model)
#     ) %>%
#     group_by(model, temp) %>%
#     summarize(m_logD = median(logD),
#               q10 = quantile(logD, .1),
#               q90 = quantile(logD, .9)) %>% 
#     ggplot(aes(x = temp, colour = model, fill = model)) +
#     # geom_point(aes(x = temp, y = logD), inherit.aes = FALSE,
#     #            data = my_data, shape = 1) +
#     geom_ribbon(aes(ymin = q10, ymax = q90), alpha = .3) +
#     geom_line(aes(y = m_logD), linetype = 2, size = 1) +
#     geom_point(aes(x = temp, y = logD), data = my_data, inherit.aes = FALSE) +
#     # theme_bw() +
#     xlab("Temperature (ºC)") + ylab("D-value (log min)") +
#     theme(legend.position = "top",
#           legend.title = element_blank(),
#           axis.text = element_text(size = 14),
#           axis.title = element_text(size = 16),
#           legend.text = element_text(size = 14)) 
        

set.seed(12412)

z <- 11.6
logDref <- -0.052
x_ref
10^(logDref - (120-x_ref)/11.6)

As.mcmc.list(trunc_model)[[1]] %>%
    as.data.frame() %>%
    summarize(median(z), median(logDref))

As.mcmc.list(trunc_model)[[1]] %>%
    as.data.frame() %>%
    mutate(logD = logDref - (120-x_ref)/11.6,
           D = 10^logD) %>%
    summarize(median(D), median(logD), 10^median(logD))

list(`Truncated model` = trunc_model,
     `Classical model` = lin_model) %>%
    map(., ~ As.mcmc.list(.)[[1]]) %>%
    map(as.data.frame) %>%
    imap_dfr(., ~ mutate(.x, model = .y)) %>%
    select(-lp__) %>%
    mutate(sim = row_number()) %>%
    split(.$sim) %>%
    map_dfr(.,
            ~ tibble(temp = c(120, 130, 140),
                     logD = .$logDref - (temp - x_ref)/.$z,
                     sigma = .$sigma,
                     model = .$model)
    ) %>%
    group_by(model, temp) %>%
    summarize(q05 = quantile(logD, .05),
              q50 = quantile(logD, .5),
              q90 = quantile(logD, .9),
              q95 = quantile(logD, 0.95),
              q99 = quantile(logD, .99)
              ) %>%
    mutate(10^q50*60) %>% View()
    # mutate(10^q95*60, 10^q05*60)
    # mutate(10^q50*6*60, 10^q90*6*60, 10^q95*6*60, 10^q99*6*60) %>% View()









