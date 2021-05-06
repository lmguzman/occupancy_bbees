library(ggplot2)
library(dplyr)
library(cowplot)
library(purrr)
library(Metrics)
library(tidyr)
library(grid)
library(gridExtra) 
library(RColorBrewer)


##### Figure 2 ######

per_change_all_sor_df <- read.csv("simulations/outputs/compiled_outputs/SS_all_all_community_var_p_no_missing_visits_ss.csv")
per_change_all_sor_df_visit <- read.csv("simulations/outputs/compiled_outputs/SS_all_all_community_var_p_missing_visits_ms_ss.csv")

soroye_change_estimated_all <- per_change_all_sor_df_visit %>% 
  bind_rows(per_change_all_sor_df %>% 
              mutate(mu.v.yr = Inf)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), actual = mean(actual)) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(low = mn-se, high = mn+se) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("Inf", "0", "-0.5"))) %>% 
  ggplot() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn, colour = mu.v.yr), size = 2.5) +
  geom_linerange(aes(x = p.yr, ymin = low, ymax = high, colour = mu.v.yr), size = 1.5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.position = c(0.45, 0.15),
        legend.text.align = 0) + 
  ylab("Estimated change in occupancy (%)") + 
  xlab(as.expression(bquote("Change in detection probability (" ~ italic(p)["era"] ~ ")" ))) +
  scale_colour_manual(name = "",
                      values= c("#FF595E", "#FFCA3A", "#1982C4"),
                      labels = c("no missing visits", as.expression(bquote("missing visits, "~ italic(v)["era"] ~ "= 0")), 
                                 as.expression(bquote("missing visits, "~ italic(v)["era"] ~ "= -0.5"))))

ggsave(soroye_change_estimated_all, filename = "simulations/figures/soroye_change_estimated.jpeg", width =7.5)



######## Figure 5 ########

North_america_status <- read.csv("simulations/outputs/compiled_outputs/Bee status - North America.csv")
Europe_status <- read.csv("simulations/outputs/compiled_outputs/Bee status - Europe.csv")

NA_plot <- North_america_status %>% 
  filter(SS_soroye < 1.5, !is.na(IUCN_status)) %>% 
  ggplot() + 
  geom_point(aes(x = MS_range_detected*100, y = SS_soroye*100, fill = IUCN_trend), size = 3, pch = 21) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  theme_cowplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-90, 50)) + scale_x_continuous(limits = c(-90, 90)) +
  scale_fill_manual(values = c('red', 'yellow', 'grey')) + ggtitle("North America") +
  theme(legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20))+ xlab("") + ylab(bquote(atop("Change in probability","of site occupancy (%)" ~ SS["all,all"])))



EU_plot <- Europe_status %>% 
  ggplot() + 
  geom_point(aes(x = MS_range_detected*100, y = SS_soroye*100, fill = IUCN_trend), size = 3, pch = 21) +
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  theme_cowplot() +
  geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(-90, 50)) + scale_x_continuous(limits = c(-90, 90)) +
  scale_fill_manual(name = "IUCN trend", values = c('red', 'green', 'yellow', 'grey')) + ggtitle("Europe") + 
  xlab("") + ylab("") +
  theme(legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20),
        legend.text = element_text(size = 20))


legend_p <- Europe_status %>% 
  ggplot() + 
  geom_point(aes(x =MS_range_detected*100, y = SS_soroye*100, fill = IUCN_trend), size = 3, pch = 21) +
  theme_cowplot() +
  scale_fill_manual(name = "IUCN trend", values = c('red', 'green', 'yellow', 'grey')) + ggtitle("") + 
  xlab("") + ylab("") +
  theme(title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.position = 'bottom')

legend <- cowplot::get_legend(legend_p)

species_specific_iucn <- plot_grid(NA_plot, EU_plot, rel_widths = c(1.15, 1), label_x = "h")


species_specific_iucn_lab <- ggdraw(add_sub(species_specific_iucn, expression("Change in probability of site occupancy (%)" ~ MS["range,detected"]), vpadding=grid::unit(0,"lines"),y=6, x=0.5, vjust=5.5, size = 20))

species_specific_iucn_lab_2 <- ggdraw() + 
  draw_plot(species_specific_iucn_lab, 0, 0.15, 1, 0.8) +
  draw_plot(legend, 0.25, -0.2, 0.5, .5) 


ggsave(species_specific_iucn_lab_2, filename = "simulations/figures/species_specific_iucn.jpeg", width =  10, height = 5)


##### Figure 6 #####

###### Species specific changes #####

North_america_status <- read.csv("simulations/outputs/compiled_outputs/Bee status - North America.csv")
Europe_status <- read.csv("simulations/outputs/compiled_outputs/Bee status - Europe.csv")

## NA
NA_long <- North_america_status %>% 
  dplyr::select(Species, SS_soroye, MS_range_detected) %>% 
  pivot_longer(-Species, names_to = 'model', values_to = 'value')

cuts <- cut(NA_long$value, breaks = c(-1, -0.75,-0.5,-0.25, 0,0.25, 0.5,0.75, 1, 8))

NA_long_cat <- NA_long %>% 
  filter(!(Species == 'distinguendus' & model == "SS_soroye")) %>% 
  mutate(values_cat = cut(value, breaks = c(-1, -0.75,-0.5,-0.25, 0,0.25, 0.5,0.75, 1, 8))) %>% 
  bind_rows(mutate(filter(NA_long, (Species == 'distinguendus' & model == "SS_soroye")), values_cat = '(1,8]')) %>% 
  mutate(model = factor(model, levels = c("SS_soroye", "MS_range_detected"))) %>% 
  mutate(values_cat = factor(values_cat, levels = levels(cuts))) %>% 
  mutate(Species = factor(Species, levels = sort(unique(NA_long$Species), decreasing = TRUE)))%>% 
  mutate(continent = "North America")

## EU

EU_long <- Europe_status %>% 
  dplyr::select(Species, SS_soroye, MS_range_detected) %>% 
  pivot_longer(-Species, names_to = 'model', values_to = 'value')

cuts <- cut(EU_long$value, breaks = c(-1, -0.75,-0.5,-0.25, 0,0.25, 0.5,0.75, 1, 8))

EU_long_cat <- EU_long %>% 
  mutate(values_cat = cut(value, breaks = c(-1, -0.75,-0.5,-0.25, 0,0.25, 0.5,0.75, 1, 8))) %>% 
  mutate(model = factor(model, levels = c("SS_soroye", "MS_range_detected"))) %>% 
  mutate(values_cat = factor(values_cat, levels = levels(cuts))) %>% 
  mutate(Species = factor(Species, levels = sort(unique(EU_long$Species), decreasing = TRUE))) %>% 
  mutate(continent = "Europe")


label_cats <- c("-1 > x > -0.75", "-0.75 > x > -0.5", "-0.5 > x > -0.25",
                "-0.25 > x > 0", "0 > x > 0.25", "0.25 > x > 0.5", "0.5 > x > 0.75",
                "0.75 > x > 1",
                "x > 1")

cols_1 <- brewer.pal(8,"BrBG")

all_table <- bind_rows(NA_long_cat, EU_long_cat) %>% 
  mutate(continent = factor(continent, levels = c('North America', "Europe"))) %>% 
  ggplot() + 
  geom_tile(aes(x = model, y = Species, fill = values_cat), alpha = 0.9) +
  scale_fill_manual(values = c(cols_1, brewer.pal(10,"BrBG")[10]), labels = label_cats, name = "") +
  ylab("") + xlab('') +
  facet_wrap(~continent, scales = "free") +
  geom_text(aes(x = model, y = Species, label = round(value,2))) +
  scale_x_discrete(labels = c(expression(SS["all,all"]), expression(MS["range,detected"]))) +
  theme_cowplot()+
  theme(legend.position = 'bottom', 
        strip.background = element_blank(),
        legend.key = element_rect(color="black"))

all_table

ggsave(all_table, filename = "simulations/figures/table.jpeg", height = 12)



########## supplement figure 2 ########

##### MS_all_all #####

multi_ag_df <- read.csv("simulations/outputs/compiled_outputs/MS_all_all_community_var_p_no_missing_visits_ms.csv")

M2_int <- multi_ag_df %>% 
  filter(var.names == "mu.psi", case == 'allall') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  ggplot() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn), position = position_dodge(width=0.3), size = 3, colour = "#FF595E") + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se), position = position_dodge(width=0.3), size = 2, colour = "#FF595E") +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5), position = position_dodge(width=0.3), size = 1, colour = "#FF595E") +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab(expression("Estimated mean occupancy (" ~ psi[0] ~ ")")) + xlab("") 

M2 <- multi_ag_df %>%
  filter(var.names == "mu.psi.yr", case == 'allall') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  ggplot() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = p.yr, y = mn), position = position_dodge(width=0.3), size = 3, colour = "#FF595E") + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se), position = position_dodge(width=0.3), size = 2, colour = "#FF595E") +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5), position = position_dodge(width=0.3), size = 1, colour = "#FF595E") +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab(expression("Estimated change in occupancy (" ~ mu[psi~", era"] ~ ")")) + 
  xlab(as.expression(bquote("Change in detection probability (" ~ italic(p)["era"] ~ ")" ))) 


multi_ag_df_vpsi <- read.csv("simulations/outputs/compiled_outputs/MS_all_all_community_var_psi_no_missing_visits_ms.csv")

M2_int_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi", case == 'allall') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  ggplot() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = mu.psi.yr, y = mn), position = position_dodge(width=0.3), size = 3, colour = "#FF595E") + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se), position = position_dodge(width=0.3), size = 2, colour = "#FF595E") +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5), position = position_dodge(width=0.3), size = 1, colour = "#FF595E") +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab("") 


M2_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi.yr", case == 'allall') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn), position = position_dodge(width=0.3), size = 3, colour = "#FF595E") + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se), position = position_dodge(width=0.3), size = 2, colour = "#FF595E") +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5), position = position_dodge(width=0.3), size = 1, colour = "#FF595E") +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab(expression("Change in occupancy (" ~ mu[psi~",era"] ~ ")")) 


MS_all_all <- plot_grid(M2_int, M2_int_vpsi, M2, M2_vpsi)

ggsave(MS_all_all, filename = "simulations/figures/MS_all_all.jpeg", height = 13, width = 12)



########## Supplementary Figure 3 ########

##### MSrange_detected ####

multi_ag_df <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_p_missing_visits_ms_ss.csv")

M3_int <- multi_ag_df %>%
  filter(var.names == "mu.psi", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>% 
  ggplot() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab(expression("Estimated mean occupancy (" ~ psi[0] ~ ")")) + xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))

M3 <- multi_ag_df %>%
  filter(var.names == "mu.psi.yr", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>% 
  ggplot() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab(expression("Estimated change in occupancy (" ~ mu[psi~", era"] ~ ")")) + 
  xlab(as.expression(bquote("Change in detection probability (" ~ italic(p)["era"] ~ ")" ))) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


multi_ag_df_vpsi <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_psi_missing_visits_ms_ss.csv")

M3_int_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>% 
  ggplot() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


M3_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi.yr", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>% 
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab(expression("Change in occupancy (" ~ mu[psi~",era"] ~ ")")) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


legend_p <-  multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi.yr", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>% 
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  scale_colour_manual(name = as.expression(bquote("Change in visit probability (" ~ italic(v)["era"] ~ ")")), labels = c("0","-0.5"), values = c("#FFCA3A", "#1982C4")) +
  theme_cowplot() +
  theme(title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.position = 'bottom') 


legend <- cowplot::get_legend(legend_p)


MS_range_detected <- plot_grid(M3_int, M3_int_vpsi, M3, M3_vpsi)

MS_range_detected_2 <- ggdraw() + 
  draw_plot(MS_range_detected, 0, 0.15, 1, 0.8) +
  draw_plot(legend, 0.25, -0.2, 0.5, .6) 

ggsave(MS_range_detected_2, filename = "simulations/figures/MS_range_detected.jpeg", height = 13, width = 12)


########## supplement figure 4 ########

####  MSrange_visits ####

multi_ag_df <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_p_missing_visits_ms_ss.csv")

M4_int <- multi_ag_df %>%
  filter(var.names == "mu.psi", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>% 
  ggplot() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab(expression("Estimated mean occupancy (" ~ psi[0] ~ ")")) + xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))

M4 <- multi_ag_df %>%
  filter(var.names == "mu.psi.yr", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab(expression("Estimated change in occupancy (" ~ mu[psi~", era"] ~ ")")) + 
  xlab(as.expression(bquote("Change in detection probability (" ~ italic(p)["era"] ~ ")" ))) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))

multi_ag_df_vpsi <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_psi_missing_visits_ms_ss.csv")

M4_int_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


M4_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi.yr", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab(expression("Change in occupancy (" ~ mu[psi~",era"] ~ ")")) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


legend_p <-  multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi.yr", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  scale_colour_manual(name = as.expression(bquote("Change in visit probability (" ~ italic(v)["era"] ~ ")")), labels = c("0","-0.5"), values = c("#FFCA3A", "#1982C4")) +
  theme_cowplot() +
  theme(title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.position = 'bottom') 

legend <- cowplot::get_legend(legend_p)


MS_range_visits <- plot_grid(M4_int, M4_int_vpsi, M4, M4_vpsi)

MS_range_visits_2 <- ggdraw() + 
  draw_plot(MS_range_visits, 0, 0.15, 1, 0.8) +
  draw_plot(legend, 0.25, -0.2, 0.5, .6) 

ggsave(MS_range_visits_2, filename = "simulations/figures/MS_range_visits.jpeg", height = 13, width = 12)


########## supplement figure 5 ########

#### MSrange_all ####

multi_ag_df <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_p_missing_visits_ms_ss.csv")

M2_int <- multi_ag_df %>%
  filter(var.names == "mu.psi", case == 'all') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab(expression("Estimated mean occupancy (" ~ psi[0] ~ ")")) + xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))

M2 <- multi_ag_df %>%
  filter(var.names == "mu.psi.yr", case == 'all') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(yintercept = 0, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab(expression("Estimated change in occupancy (" ~ mu[psi~", era"] ~ ")")) + 
  xlab(as.expression(bquote("Change in detection probability (" ~ italic(p)["era"] ~ ")" ))) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


multi_ag_df_vpsi <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_psi_missing_visits_ms_ss.csv")

M2_int_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi", case == 'all') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = 0), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


M2_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi.yr", case == 'all') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab(expression("Change in occupancy (" ~ mu[psi~",era"] ~ ")")) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


legend_p <-  multi_ag_df_vpsi %>%
  filter(var.names == "mu.psi.yr", case == 'all') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  scale_colour_manual(name = as.expression(bquote("Change in visit probability (" ~ italic(v)["era"] ~ ")")), labels = c("0","-0.5"), values = c("#FFCA3A", "#1982C4")) +
  theme_cowplot() +
  theme(title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.position = 'bottom') 

legend <- cowplot::get_legend(legend_p)


MS_range_all <- plot_grid(M2_int, M2_int_vpsi, M2, M2_vpsi)

MS_range_all_2 <- ggdraw() + 
  draw_plot(MS_range_all, 0, 0.15, 1, 0.8) +
  draw_plot(legend, 0.25, -0.2, 0.5, .6) 

ggsave(MS_range_all_2, filename = "simulations/figures/MS_range_all.jpeg", height = 13, width = 12)


########## supplement figure 6 ########

#### MSrange_visits p (detection) y axis ####

multi_ag_df <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_p_missing_visits_ms_ss.csv")

M2_int <- multi_ag_df %>%
  filter(var.names == "mu.p", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = -0.5), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + 
  ylab(bquote(atop("Estimated mean","detection probability (" ~ italic(p)[0] ~ ")"))) +
  xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))

M2 <- multi_ag_df %>%
  filter(var.names == "p.yr", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_abline(aes(intercept = 0, slope = 1), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + 
  ylab(bquote(atop("Estimated change in","detection probability (" ~ italic(p)["era"] ~ ")"))) + 
  xlab(as.expression(bquote("Change in detection probability (" ~ italic(p)["era"] ~ ")" ))) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))

multi_ag_df_vpsi <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_psi_missing_visits_ms_ss.csv")

M2_int_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.p", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = -0.5), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


M2_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "p.yr", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = -0.5), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab(expression("Change in occupancy (" ~ mu[psi~",era"] ~ ")")) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


legend_p <-  multi_ag_df_vpsi %>%
  filter(var.names == "p.yr", case == 'visits') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  scale_colour_manual(name = as.expression(bquote("Change in visit probability (" ~ italic(v)["era"] ~ ")")), labels = c("0","-0.5"), values = c("#FFCA3A", "#1982C4")) +
  theme_cowplot() +
  theme(title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.position = 'bottom') 

legend <- cowplot::get_legend(legend_p)


MS_range_visits_p <- plot_grid(M2_int, M2_int_vpsi, M2, M2_vpsi)

MS_range_visits_2_p <- ggdraw() + 
  draw_plot(MS_range_visits_p, 0, 0.15, 1, 0.8) +
  draw_plot(legend, 0.25, -0.2, 0.5, .6) 

ggsave(MS_range_visits_2_p, filename = "simulations/figures/MS_range_visits_p.jpeg", height = 13, width = 12)


########## supplement figure 7 ########

#### MSrange_detected p (detection) y axis  ####

multi_ag_df <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_p_missing_visits_ms_ss.csv")

M3_int <- multi_ag_df %>%
  filter(var.names == "mu.p", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = -0.5), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + 
  ylab(bquote(atop("Estimated mean","detection probability (" ~ italic(p)[0] ~ ")"))) +
  xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))

M3 <- multi_ag_df %>%
  filter(var.names == "p.yr", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(p.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_abline(aes(intercept = 0, slope = 1), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = p.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = p.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = p.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + 
  ylab(bquote(atop("Estimated change in","detection probability (" ~ italic(p)["era"] ~ ")"))) + 
  xlab(as.expression(bquote("Change in detection probability (" ~ italic(p)["era"] ~ ")" ))) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))

multi_ag_df_vpsi <- read.csv("simulations/outputs/compiled_outputs/MS_range_community_var_psi_missing_visits_ms_ss.csv")

M3_int_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "mu.p", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = -0.5), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.05, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab("") +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


M3_vpsi <- multi_ag_df_vpsi %>%
  filter(var.names == "p.yr", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_hline(aes(yintercept = -0.5), linetype = 'dashed', size = 1.5, colour = 'grey') +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 2) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mean_2.5, ymax =  mean_97.5, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 1) +
  #geom_text(aes(x = mu.psi.yr, y = mean_97.5+0.02, label = n_bci), position = position_dodge(width=0.3), size = 5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab(expression("Change in occupancy (" ~ mu[psi~",era"] ~ ")")) +
  scale_colour_manual(values = c("#FFCA3A", "#1982C4"))


legend_p <-  multi_ag_df_vpsi %>%
  filter(var.names == "p.yr", case == 'detected') %>%
  mutate(in_bci = ifelse(actual.val < `X97.5.` & actual.val > `X2.5.`, TRUE, FALSE)) %>% 
  group_by(mu.psi.yr, mu.v.yr) %>% 
  summarise(mn = mean(mean), sd = sd(mean), n = n(), mean_2.5 = mean(`X2.5.`), mean_97.5 = mean(`X97.5.`), n_bci = 10-sum(in_bci)) %>%
  mutate(se = sd/sqrt(n)) %>% 
  mutate(mu.v.yr = factor(mu.v.yr, levels = c("0", "-0.5"))) %>%
  ggplot() + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', colour = 'grey', size = 1.5) +
  geom_point(aes(x = mu.psi.yr, y = mn, colour = factor(mu.v.yr)), position = position_dodge(width=0.3), size = 3) + 
  scale_colour_manual(name = as.expression(bquote("Change in visit probability (" ~ italic(v)["era"] ~ ")")), labels = c("0","-0.5"), values = c("#FFCA3A", "#1982C4")) +
  theme_cowplot() +
  theme(title = element_text(size = 20), 
        legend.text = element_text(size = 20), 
        legend.position = 'bottom') 

legend <- cowplot::get_legend(legend_p)


MS_range_detected_p <- plot_grid(M3_int, M3_int_vpsi, M3, M3_vpsi)

MS_range_detected_2_p <- ggdraw() + 
  draw_plot(MS_range_detected_p, 0, 0.15, 1, 0.8) +
  draw_plot(legend, 0.25, -0.2, 0.5, .6) 

ggsave(MS_range_detected_2_p, filename = "simulations/figures/MS_range_detected_p.jpeg", height = 13, width = 12)



######## Supplementary Figure 8 ########

### var p ###

sp_ch_sor_varp <- read.csv("simulations/outputs/compiled_outputs/SS_all_all_species_var_p_missing_visits_ms_ss.csv")
multi_sp_df_varp <- read.csv("simulations/outputs/compiled_outputs/MS_range_species_var_p_missing_visits_ms_ss.csv")

source('simulations/src/initialize.R')

single.sp.sor.rmse <- sp_ch_sor_varp %>% 
  group_by(p.yr, r) %>% 
  summarise(rmse = rmse(actual.change, per_change)) %>% 
  ungroup() %>% 
  group_by(p.yr) %>% 
  summarise(mn = mean(rmse), sd = sd(rmse), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(case = 'sor')

multi.sp.rmse <- multi_sp_df_varp %>% 
  filter(var.names == 'per_change') %>% 
  group_by(p.yr, r, case, mu.v.yr) %>%
  summarise(rmse = rmse(actua.val, mean))  %>% 
  ungroup() %>% 
  group_by(p.yr, case, mu.v.yr) %>% 
  summarise(mn = mean(rmse), sd = sd(rmse), n = n()) %>% 
  mutate(se = sd/sqrt(n)) 


M3_varp <- bind_rows(single.sp.sor.rmse, multi.sp.rmse) %>% 
  filter(is.na(mu.v.yr) | mu.v.yr == -0.5) %>%
  mutate(case = factor(case, levels = c("sor", "all",'detected', 'visits'))) %>% 
  ggplot() + 
  geom_point(aes(x = p.yr, y= mn, colour = case), position = position_dodge(width=0.2), size= 2.5) +
  geom_linerange(aes(x = p.yr, ymin = mn-se, ymax = mn+se, colour = case), position = position_dodge(width=0.2), size = 1.5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + 
  scale_y_continuous(limits = c(0,40)) + ylab("Root mean squared error") + 
  xlab(as.expression(bquote("Change in detection probability (" ~ italic(p)["era"] ~ ")" ))) +
  scale_colour_manual(values = c("#4E6E58", "#0D1821", "#6E2594", "#9BE564"))

### var psi ###

sp_ch_sor <- read.csv("simulations/outputs/compiled_outputs/SS_all_all_species_var_psi_missing_visits_ms_ss.csv")
multi_sp_df <- read.csv("simulations/outputs/compiled_outputs/MS_range_species_var_psi_missing_visits_ms_ss.csv")

single.sp.sor.rmse <- sp_ch_sor %>% 
  group_by(mu.psi.yr, r) %>% 
  summarise(rmse = rmse(actual.change, per_change)) %>% 
  ungroup() %>% 
  group_by(mu.psi.yr) %>% 
  summarise(mn = mean(rmse), sd = sd(rmse), n = n()) %>% 
  mutate(se = sd/sqrt(n)) %>% 
  mutate(case = 'sor')

multi.sp.rmse <- multi_sp_df %>% 
  filter(var.names == 'per_change') %>% 
  group_by(mu.psi.yr, r, case, mu.v.yr) %>%
  summarise(rmse = rmse(actua.val, mean))  %>% 
  ungroup() %>% 
  group_by(mu.psi.yr, case, mu.v.yr) %>% 
  summarise(mn = mean(rmse), sd = sd(rmse), n = n()) %>% 
  mutate(se = sd/sqrt(n)) 

M1_rmse <- bind_rows(single.sp.sor.rmse, multi.sp.rmse) %>% 
  filter( is.na(mu.v.yr) | mu.v.yr == -0.5) %>% 
  mutate(case = factor(case, levels = c("sor","all", 'detected', 'visits'))) %>% 
  ggplot() + 
  geom_point(aes(x = mu.psi.yr, y= mn, colour = case), position = position_dodge(width=0.2), size= 2.5) +
  geom_linerange(aes(x = mu.psi.yr, ymin = mn-se, ymax = mn+se, colour = case), position = position_dodge(width=0.2), size = 1.5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        legend.position = 'none',
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20),
        title = element_text(size = 20)) + ylab("") + xlab(expression("Change in occupancy (" ~ mu[psi~",era"] ~ ")")) +
  scale_y_continuous(limits = c(0,40))+
  scale_colour_manual(values = c("#4E6E58", "#0D1821", "#6E2594", "#9BE564"))


rmse_all <- plot_grid(M3_varp, M1_rmse, nrow = 1)

legend_p <- bind_rows(single.sp.sor.rmse, multi.sp.rmse) %>% 
  mutate(case = factor(case, levels = c("sor", "all", 'detected', 'visits'))) %>% 
  ggplot() + 
  geom_point(aes(x = mu.psi.yr, y= mn, colour = case), position = position_dodge(width=0.2), size= 2.5) +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        title = element_text(size = 20), 
        legend.text = element_text(size  = 20),
        legend.position = 'bottom') + scale_color_manual(name = "", labels = 
                                                             c(expression(SS["all,all"]), expression(MS["range,all"]), expression(MS["range,detected"]),  expression(MS["range,visits"])), 
                                                           values = c("#4E6E58", "#0D1821", "#6E2594", "#9BE564"))

legend <- cowplot::get_legend(legend_p)

rmse_all_2 <- ggdraw() + 
  draw_plot(rmse_all, 0, 0.15, 1, 0.8) +
  draw_plot(legend, 0.3, -0.2, 0.5, .6) 

ggsave(rmse_all_2, filename = "simulations/figures/rmse_plot.jpeg", width = 12, height = 8)



