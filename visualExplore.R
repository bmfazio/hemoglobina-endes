library(ggplot2)
library(ggridges)

listo %>%
  ggplot(aes(x = hb, y = altura)) +
  geom_point()

listo %>% filter(!is.na(hb)&altura<5000) %>%
  ggplot(aes(x = hb,
             y = as.factor((altura/1000)%/%1),
             fill = factor(rtotal>0))) +
  geom_density_ridges()

listo %>% filter(!is.na(hb)&altura<5000) %>%
  ggplot(aes(x = hb_adj,
             y = as.factor((altura/1000)%/%1),
             fill = factor(rtotal>0),
             alpha = 0.5)) +
  geom_density_ridges(scale = 1.2) +
  scale_x_continuous(limits=c(50,150))

listo %>% filter(!is.na(hb)&altura<5000) %>%
  ggplot(aes(x = hb,
             y = as.factor((altura/1000)%/%1),
             fill = factor(rtotal>0),
             alpha = 0.5)) +
  geom_density_ridges(scale = 1.2) +
  scale_x_continuous(limits=c(75,180))

listo %>% filter(rtotal>0) %>%
  ggplot(aes(x = altura, y = total)) +
  geom_point()

listo %>% filter(rtotal>0) %>%
  ggplot(aes(x = altura, y = total)) +
  geom_bin2d(bins = 20) +
  scale_fill_gradient2(midpoint = 420, low = "red", mid = "green", high = "blue")