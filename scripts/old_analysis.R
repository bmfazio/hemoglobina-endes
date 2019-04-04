# Grafica del ajuste:
plot(endes2017$V040, endes2017$HW53 - endes2017$HW56)

endes2017$S465DB_A %>% table
endes2017$S465DB_B %>% table
endes2017$S465DB_C %>% table
endes2017$S465DB_D %>% table

endes2017$S465DD_AR %>% table
endes2017$S465DD_BR %>% table
endes2017$S465DD_CR %>% table

endes2017$S465DD_AC %>% table
endes2017$S465DD_BC %>% table
endes2017$S465DD_CC %>% table

(endes2017$S465DD_AC/endes2017$S465DD_AR) %>% hist
(endes2017$S465DD_BC/endes2017$S465DD_BR) %>% hist
(endes2017$S465DD_CC/endes2017$S465DD_CR) %>% hist


listo %>%
  filter(
    !is.na(hb) &
    rtotal > 0
  ) %>%
  lm(hb_adj ~ total + edad + nse + altura, data = .) -> a
summary(a)

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr"),
      data = .,
      control = list(adapt_delta = 0.99)) -> m1

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb_adj ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr"),
      data = .,
      control = list(adapt_delta = 0.99)) -> m2

##

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb_adj ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + region,
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m3.0

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb_adj ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region),
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m3.1

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb_adj ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region/cluster),
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m3.2

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb_adj ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region/cluster/hogar),
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m3.3
# save(list = c("m3.0", "m3.1", "m3.2", "m3.3"), file = "modelos3.rds")
# load(file = "modelos3.rds")
##

data.frame(
  total = c(0, 90, 180, 360),
  altura = c(rep(c(0, 1000, 2000, 3000, 4000), each = 4)),
  region = 15,
  nse = 0,
  edad = 12
) -> pred

m3.0p <- fitted(m3.0, newdata = pred, summary = F)
# 0 vs 90
rbind(summary(m3.0p[, 1] - m3.0p[, 2]),
      summary(m3.0p[, 5] - m3.0p[, 6]),
      summary(m3.0p[, 9] - m3.0p[, 10]),
      summary(m3.0p[, 13] - m3.0p[, 14]),
      summary(m3.0p[, 17]  - m3.0p[, 18])) -> pr1
# 0 vs 180
rbind(summary(m3.0p[, 1] - m3.0p[, 3]),
      summary(m3.0p[, 5] - m3.0p[, 7]),
      summary(m3.0p[, 9] - m3.0p[, 11]),
      summary(m3.0p[, 13] - m3.0p[, 15]),
      summary(m3.0p[, 17]  - m3.0p[, 19])) -> pr2
# 0 vs 360
rbind(summary(m3.0p[, 1] - m3.0p[, 4]),
      summary(m3.0p[, 5] - m3.0p[, 8]),
      summary(m3.0p[, 9] - m3.0p[, 12]),
      summary(m3.0p[, 13] - m3.0p[, 16]),
      summary(m3.0p[, 17]  - m3.0p[, 20])) -> pr3

##

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + region,
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m4.0

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region),
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m4.1

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region/cluster),
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m4.2

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region/cluster/hogar),
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m4.3

listo %>%
  filter(
    !is.na(hb) &
      rtotal > 0
  ) %>%
  brm(hb ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region/cluster) + (1|hogar),
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m4.4

# save(list = c("m4.0", "m4.1", "m4.2", "m4.3", "m4.4"), file = "modelos4.rds")

m4.0p <- fitted(m4.0, newdata = pred, summary = F)
# 0 vs 90
rbind(summary(m4.0p[, 1] - m4.0p[, 2]),
      summary(m4.0p[, 5] - m4.0p[, 6]),
      summary(m4.0p[, 9] - m4.0p[, 10]),
      summary(m4.0p[, 13] - m4.0p[, 14]),
      summary(m4.0p[, 17]  - m4.0p[, 18])) -> pr1
# 0 vs 180
rbind(summary(m4.0p[, 1] - m4.0p[, 3]),
      summary(m4.0p[, 5] - m4.0p[, 7]),
      summary(m4.0p[, 9] - m4.0p[, 11]),
      summary(m4.0p[, 13] - m4.0p[, 15]),
      summary(m4.0p[, 17]  - m4.0p[, 19])) -> pr2
# 0 vs 360
rbind(summary(m4.0p[, 1] - m4.0p[, 4]),
      summary(m4.0p[, 5] - m4.0p[, 8]),
      summary(m4.0p[, 9] - m4.0p[, 12]),
      summary(m4.0p[, 13] - m4.0p[, 16]),
      summary(m4.0p[, 17]  - m4.0p[, 20])) -> pr3

###
# listo %>%
#   filter(
#     !is.na(hb)
#   ) %>%
#   brm(hb ~ t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region/cluster) + (1|hogar),
#       data = .,
#       control = list(adapt_delta = 0.95,
#                      max_treedepth = 20)) -> m5
# 
# listo %>%
#   filter(
#     !is.na(hb)
#   ) %>%
#   brm(hb ~ I(rtotal==0) + I(rtotal!=0)*t2(total, altura) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region/cluster) + (1|hogar),
#       data = .,
#       control = list(adapt_delta = 0.95,
#                      max_treedepth = 20)) -> m6


# listo %>%
#   filter(
#     !is.na(hb) &
#       rtotal > 0) %>%
#   ggplot(aes(x = total, y = hb)) + geom_point() + geom_smooth()
# listo %>%
#   filter(
#     !is.na(hb) &
#       rtotal > 0) %>%
#   ggplot(aes(x = total, y = hb_adj)) + geom_point() + geom_smooth()

listo %>%
  filter(
    !is.na(hb)
  ) %>%
  mutate(rtotal = rtotal > 0) %>%
  brm(hb ~ t2(total, altura, by = rtotal) + s(edad, bs = "cr") + s(nse, bs = "cr") + (1|region/cluster),
      data = .,
      control = list(adapt_delta = 0.95,
                     max_treedepth = 20)) -> m5
