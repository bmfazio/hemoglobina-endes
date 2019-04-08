# Entrega
endes2016 %>%
  transmute(rtotal = rtotal > 0) %>%
  pull(rtotal) %>% table
endes2017 %>%
  transmute(rtotal = rtotal > 0) %>%
  pull(rtotal) %>% table

# quickie fitie
endes2016 %>%
  mutate(supp = as.numeric(rtotal > 0)) %>%
brm(supp ~ s(edad, bs = "cr") + s(nse, bs = "cr") + s(altura, bs = "cr") + region,
     data = ., family = bernoulli, control = list(adapt_delta = 0.90, max_treedepth = 15)) -> logireb16

endes2017 %>%
  mutate(supp = as.numeric(rtotal > 0)) %>%
brm(supp ~ s(edad, bs = "cr") + s(nse, bs = "cr") + s(altura, bs = "cr") + region,
     data = ., family = bernoulli, control = list(adapt_delta = 0.90, max_treedepth = 15)) -> logireb17

# Post-fit analysis
load("out/m1.2016.rds")
load("out/m1_adj.2016.rds")
load("out/m2.2016.rds")
load("out/m2_r.2016.rds")

m1.2016$formula
m2.2016$formula
m2_r.2016$formula
#hb ~
# t2(total, altura) +
# s(edad, bs = "cr") +
# s(nse, bs = "cr") +
# region + (1 | cluster)
###

# raneff only
a <- as.array(m2_r.2016)
a <- a[,,38:2949]
bayesplot::mcmc_areas(a[,,1:25])
bayesplot::mcmc_intervals(a[,,103:105])
# cluster 109
# La mayoria de clusters tiene entre 4 y 11 miembros
endes2016 %>% group_by(cluster) %>% transmute(n = n()) %>% pull(n) %>% table

####

dif_plot <- function(fit_) {
  pred_df <- data.frame(
  total = rep(c(0, 30, 90, 180, 360), 5),
  altura = rep(1000*(0:4), each = 5),
  edad = 12,
  nse = 0,
  region = "15"
)

cbind(pred_df,
      y = predict(fit_, newdata = pred_df)[, 1]) %>%
  mutate(altura = as.factor(altura)) %>%
  group_by(altura) %>%
  mutate(ymin = min(y),
         ydif = y - ymin) %>%
  ggplot(aes(x = total, y = ydif, color = altura)) +
  geom_point() + geom_line()
}

dif_plot(m1.2016)
dif_plot(m2.2016)
dif_plot(m2_r.2016)
dif_plot(m1_adj.2016)

# En el 2016 se revierte la tendencia: las personas en altura cero muestran menos mejoría con mayor suplementación

# Se me ocurre que la entrega mas indiscriminada podria explicarlo. Idea para evaluar esto: comparar 2015, 2016 vs 2017, 2018 en la proporcion de personas que recibieron suplementacion por region