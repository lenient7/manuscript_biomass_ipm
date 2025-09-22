## polypropylene
data_ad_pp <- read.csv(file = "/home/huy1/local/docs/prj/Active/Powder Flow/MT-PSU/Tasks/BP3/data_analysis/airdried_ipm/pp_pp/dt_aggregated.csv", header = TRUE)
data_ad_pp$mc <- "ad"
data_ad_pp$traction = data_ad_pp$traction * 0.00980665
data_ad_pp$normal = data_ad_pp$normal * 0.00980665

## Determined Derjaguin law parameters

## polypropylene
## calibrated based on the manufacturer's listed friction coefficient
##data_ipm_pp_ad <- read.csv(file = "./BP3/data_analysis/airdried_ipm/validation_pp/dt_coeffs.csv", header = TRUE)
## raw data
data_ipm_pp_ad <- read.csv(file = "/home/huy1/local/docs/prj/Active/Powder Flow/MT-PSU/Tasks/BP3/data_analysis/airdried_ipm/pp_pp/dt_coeffs.csv", header = TRUE)
data_ipm_pp_ad$mc <- "ad"
data_ipm_pp_agg <- data_ipm_pp_ad
data_ipm_pp_agg$mc <- factor(data_ipm_pp_agg$mc, levels = c("ad", "95rh"))

## summary table
tableContinuous(vars=data_ipm_pp_ad[,c("mu", "c")], prec = 2)

## aggregated data EDA for friction coefficient, mu
pp_pp_coeff_mu_box_plt <- ggplot(subset(data_ipm_pp_ad, select = c("fraction_pair", "mu", "mc")), aes(x = fraction_pair, y = mu, fill = factor(mc, level = c('ad', '95rh')))) + # , group = mc)) +
  ## geom_quasirandom(position = position_dodge2(reverse = TRUE), dodge.width=0.3) +
  geom_boxplot(outlier.alpha = 0.1, position = position_dodge2(.9), alpha = 0.2) +
  geom_beeswarm(alpha = 0.5) +
  stat_summary(fun = mean, geom = 'point', aes(group = mc, fill = factor(mc, level = c('ad', '95rh'))), position = position_dodge2(.7), shape = 23, size = 2, fill = 3) +
                                        # geom_jitter(shape = 16, position = position_jitterdodge(0.1), alpha = 0.25) +
  scale_y_continuous(name = expression(Friction ~ Coefficient), limits = c(-0.0, 1.0), expand = c(0.0, 0.0)) +
  ## scale_x_continuous(limits = c(0, 75.0), expand = c(0.0, 0.0)) + #geom_errorbar(position = position_dodge2(width = 0.9, padding = 0.5), color = "gray7", alpha = 0.8) +
  xlab("Fraction Pair") + # + facet_grid(cols = vars(mc_levels), rows = vars(temperature_levels)) +
  scale_fill_viridis_d(name = "Moisture Content", breaks = c("ad", "95rh"), labels = c("air-dried", "hydrated at 95% RH")) +
  theme_bw() + theme(panel.background = element_rect(fill = "transparent"), legend.position="none") + labs(fill = "MC", color = "MC")
print(pp_pp_coeff_mu_box_plt)

## aggregated data EDA for adhesion force, mu, combined
pp_pp_comb_coeff_mu_box_plt <- ggplot(subset(data_ipm_pp_ad, select = c("mc", "mu", "mc")), aes(x = mc, y = mu, fill = factor(mc, level = c('ad', '95rh')))) + # , group = mc)) +
      ## geom_quasirandom(position = position_dodge2(reverse = TRUE), dodge.width=0.3) +
  geom_boxplot(outlier.alpha = 0.1, position = position_dodge2(.9), alpha = 0.2) +
  geom_beeswarm(alpha = 0.5) +
  stat_summary(fun = mean, geom = 'point', aes(group = mc, fill = factor(mc, level = c('ad', '95rh'))), position = position_dodge2(.7), shape = 23, size = 2, fill = 3) +
                                        # geom_jitter(shape = 16, position = position_jitterdodge(0.1), alpha = 0.25) +
  scale_y_continuous(name = expression(Friction ~ Coefficient), limits = c(-0.0, 1.0), expand = c(0.0, 0.0)) +
  ## scale_x_continuous(limits = c(0, 75.0), expand = c(0.0, 0.0)) + #geom_errorbar(position = position_dodge2(width = 0.9, padding = 0.5), color = "gray7", alpha = 0.8) +
  xlab("Polypropylen to Polypropylen") + # + facet_grid(cols = vars(mc_levels), rows = vars(temperature_levels)) +
  scale_fill_viridis_d(name = "Moisture Content", breaks = c("ad", "95rh"), labels = c("air-dried", "hydrated at 95% RH")) +
  theme_bw() + theme(panel.background = element_rect(fill = "transparent"), legend.position="none", axis.text.x=element_blank()) + labs(fill = "MC", color = "MC")
print(pp_pp_comb_coeff_mu_box_plt)

## aggregated data EDA for adhesion force, c
pp_pp_coeff_c_box_plt <- ggplot(subset(data_ipm_pp_ad, select = c("fraction_pair", "c", "mc")), aes(x = fraction_pair, y = c, fill = factor(mc, level = c('ad', '95rh')))) + #, group = mc)) +
  ## geom_quasirandom(position = position_dodge2(reverse = TRUE), dodge.width=0.3) +
  geom_boxplot(outlier.alpha = 0.1, position = position_dodge2(.9), alpha = 0.2) +
  geom_beeswarm(alpha = 0.5) +
  stat_summary(fun = mean, geom = 'point', aes(group = mc, fill = factor(mc, level = c('ad', '95rh'))), position = position_dodge2(.7), shape = 23, size = 2, fill = 3) +
                                        #geom_jitter(shape = 16, position = position_jitterdodge(0.1), alpha = 0.25) +
  scale_y_continuous(name = expression(Adhesion~Force~(N)), limits = c(-0.5, 1.0), expand = c(0.0, 0.0)) +
  ##scale_x_continuous(limits = c(0, 75.0), expand = c(0.0, 0.0)) + #geom_errorbar(position = position_dodge2(width = 0.9, padding = 0.5), color = "gray7", alpha = 0.8) +
  xlab("Fraction pair") + # + facet_grid(cols = vars(mc_levels), rows = vars(temperature_levels)) +
  scale_fill_viridis_d(name = "Moisture Content", breaks = c("ad", "95rh"), labels = c("air-dried", "hydrated at 95% RH")) +
  theme_bw() + theme(panel.background = element_rect(fill = "transparent"), legend.position="none") + labs(fill = "MC", color = "MC")
print(pp_pp_coeff_c_box_plt)

## aggregated data EDA for adhesion force, c
pp_pp_comb_coeff_c_box_plt <- ggplot(subset(data_ipm_pp_ad, select = c("mc", "c", "mc")), aes(x = mc, y = c, fill = factor(mc, level = c('ad', '95rh')))) + #, group = mc)) +
  ## geom_quasirandom(position = position_dodge2(reverse = TRUE), dodge.width=0.3) +
  geom_boxplot(outlier.alpha = 0.1, position = position_dodge2(.9), alpha = 0.2) +
  geom_beeswarm(alpha = 0.5) +
  stat_summary(fun = mean, geom = 'point', aes(group = mc, fill = factor(mc, level = c('ad', '95rh'))), position = position_dodge2(.7), shape = 23, size = 2, fill = 3) +
                                        #geom_jitter(shape = 16, position = position_jitterdodge(0.1), alpha = 0.25) +
  scale_y_continuous(name = expression(Adhesion~Force~(N)), limits = c(-0.5, 1.0), expand = c(0.0, 0.0)) +
  ##scale_x_continuous(limits = c(0, 75.0), expand = c(0.0, 0.0)) + #geom_errorbar(position = position_dodge2(width = 0.9, padding = 0.5), color = "gray7", alpha = 0.8) +
  xlab("Polypropylen to Polypropylen") + # + facet_grid(cols = vars(mc_levels), rows = vars(temperature_levels)) +
  scale_fill_viridis_d(name = "Moisture Content", breaks = c("ad", "95rh"), labels = c("air-dried", "hydrated at 95% RH")) +
  theme_bw() + theme(panel.background = element_rect(fill = "transparent"), legend.position="none", axis.text.x=element_blank()) + labs(fill = "MC", color = "MC")
print(pp_pp_comb_coeff_c_box_plt)

## combined plot for mu and c
pp_pp_ipm_coeffs_box_plts <- plot_grid(pp_pp_coeff_mu_box_plt, pp_pp_coeff_c_box_plt, nrow=1, rel_widths=c(1/2, 1/2))
print(pp_pp_ipm_coeffs_box_plts)

## or
pp_pp_comb_ipm_coeffs_box_plts <- plot_grid(pp_pp_comb_coeff_mu_box_plt, pp_pp_comb_coeff_c_box_plt, nrow=1, rel_widths=c(1/2, 1/2))
print(pp_pp_comb_ipm_coeffs_box_plts)
ggsave(filename="./figures/pp_pp_comb_ipm_coeffs_box_plts.png", plot = pp_pp_comb_ipm_coeffs_box_plts, width = 8, height = 6, units = "in", dpi = 450)

dt_ad_cs_typical_two_cases.plt <- plot_grid(dt_ad_cs_typical_cob_cob_20230508_02.plt, dt_ad_cs_typical_stalk_stalk_20230505_11.plt, ncol = 1, labels = c("A", "B"))
# ggsave("./figures/typical_result_cs_two_cases.png", dt_ad_cs_typical_two_cases.plt, width=9, height=12, units="in", dpi=300, bg="white")
# return("figures/typical_result_cs_two_cases.png")
print(dt_ad_cs_typical_two_cases.plt)

## corn stover data
data_ipm_ad_cs <- read.csv(file = "/home/huy1/local/docs/prj/Active/Powder Flow/MT-PSU/Tasks/BP3/data_analysis/airdried_ipm/cs_calibrated_traction_force/dt_coeffs.csv", header = TRUE)
data_ipm_ad_cs$mc <- "ad"
data_ipm_95rh_cs <- read.csv(file = "/home/huy1/local/docs/prj/Active/Powder Flow/MT-PSU/Tasks/BP3/data_analysis/hydrated_ipm/cs_calibrated_traction_force/dt_coeffs.csv", header = TRUE)
data_ipm_95rh_cs$mc <- "95rh"
data_ipm_cs <- rbind(data_ipm_ad_cs, data_ipm_95rh_cs)
data_ipm_cs$fraction_pair <- as.factor(data_ipm_cs$fraction_pair)
data_ipm_cs$mc <- as.factor(data_ipm_cs$mc)

## aggregated data EDA; Friction Coefficient of CS same fraction of IPM paper #1
pal <-  viridis(4) ## pallet for viridis color
cs_cs_coeff_mu_box_plt <-
  ggplot(subset(data_ipm_cs, (fraction_pair=='cob-cob' | fraction_pair=='husk-husk' | fraction_pair=='leaf-leaf' | fraction_pair=='stalk-stalk') & mc=='ad' & mu > 0.005,
                select = c("fraction_pair", "mu")), aes(x = fraction_pair, y = mu)) + # , group = mc)) +
  geom_boxplot(outlier.alpha = 0.1, position = position_dodge2(.9), fill = pal[4]) +
  stat_summary(fun = mean, geom = 'point', shape = 23, size = 5, fill = 3) +
  geom_jitter(shape = 16, alpha = 0.25, size = 2, width = 0.1) +
  scale_y_continuous(name = expression(Friction ~ Coefficient), limits = c(0.0, 1.75), expand = c(0.0, 0.0)) +
  ## scale_x_continuous(limits = c(0, 75.0), expand = c(0.0, 0.0)) +
  ## geom_errorbar(position = position_dodge2(width = 0.9, padding = 0.5), color = "gray7", alpha = 0.8) +
  xlab("Fraction Pair") + # + facet_grid(cols = vars(mc_levels), rows = vars(temperature_levels)) +
  #scale_fill_viridis_d(name = "Moisture Content", breaks = c("ad", "95rh"), labels = c("air-dried", "hydrated at 95% RH")) +
  theme_bw() + theme(panel.background = element_rect(fill = "transparent"), text = element_text(size=24))# + labs(fill = "MC", color = "MC")
print(cs_cs_coeff_mu_box_plt)
ggsave(filename="./figures/cs_cs_same_fraction_coeff_mu_box_plt.png", plot=cs_cs_coeff_mu_box_plt, width = 8, height = 6, units = "in", dpi = 450)

## aggregated data EDA; Adhesion force CS same fraction of IPM paper #1
pal <-  viridis(4) ## pallet for viridis color
cs_cs_coeff_c_box_plt <-
  #ggplot(subset(data_ipm_cs,  (fraction_pair=='cob-cob' | fraction_pair=='husk-husk' | fraction_pair=='leaf-leaf' | fraction_pair=='stalk-stalk') & mu > 0.005,
  ggplot(subset(data_ipm_cs, (fraction_pair=='cob-cob' | fraction_pair=='husk-husk' | fraction_pair=='leaf-leaf' | fraction_pair=='stalk-stalk') & mc=='ad' & mu > 0.005,
                select = c("fraction_pair", "c")), aes(x = fraction_pair, y = c)) + # , group = mc)) +
  geom_boxplot(outlier.alpha = 0.1, position = position_dodge2(.9), fill = pal[4]) +
  stat_summary(fun = mean, geom = 'point', shape = 23, size = 5, fill = 3) +
  geom_jitter(shape = 16, alpha = 0.25, size = 2, width = 0.1) +
  scale_y_continuous(name = expression(Adhesion~Force~(N)), limits = c(-0.5, 3.5), expand = c(0.0, 0.0)) +
  ## scale_x_continuous(limits = c(0, 75.0), expand = c(0.0, 0.0)) +
  ## geom_errorbar(position = position_dodge2(width = 0.9, padding = 0.5), color = "gray7", alpha = 0.8) +
  xlab("Fraction Pair") + # + facet_grid(cols = vars(mc_levels), rows = vars(temperature_levels)) +
                                        #scale_fill_viridis_d(name = "Moisture Content", breaks = c("ad", "95rh"), labels = c("air-dried", "hydrated at 95% RH")) +
  theme_bw() + theme(panel.background = element_rect(fill = "transparent"), text = element_text(size=24))# + labs(fill = "MC", color = "MC")

print(cs_cs_coeff_c_box_plt)
ggsave(filename="./figures/cs_cs_same_fraction_coeff_c_box_plt.png", plot=cs_cs_coeff_c_box_plt, width = 8, height = 6, units = "in", dpi = 450)

## aggregated data EDA; Friction Coefficient of CS same fraction of IPM paper #1
pal <-  viridis(4) ## pallet for viridis color
cs_cs_diff_coeff_mu_box_plt <-
  ggplot(subset(data_ipm_cs, (fraction_pair=='cob-husk' | fraction_pair=='cob-leaf' | fraction_pair=='cob-stalk' | fraction_pair=='husk-leaf' | fraction_pair=='husk-stalk' | fraction_pair=='leaf-stalk') & mc=='ad' & mu > 0.005,
                select = c("fraction_pair", "mu")), aes(x = fraction_pair, y = mu)) + # , group = mc)) +
  geom_boxplot(outlier.alpha = 0.1, position = position_dodge2(.9), fill = pal[4]) +
  stat_summary(fun = mean, geom = 'point', shape = 23, size = 5, fill = 3) +
  geom_jitter(shape = 16, alpha = 0.25, size = 2, width = 0.1) +
  scale_y_continuous(name = expression(Friction ~ Coefficient), limits = c(0.0, 1.75), expand = c(0.0, 0.0)) +
  ## scale_x_continuous(limits = c(0, 75.0), expand = c(0.0, 0.0)) +
  ## geom_errorbar(position = position_dodge2(width = 0.9, padding = 0.5), color = "gray7", alpha = 0.8) +
  xlab("Fraction Pair") + # + facet_grid(cols = vars(mc_levels), rows = vars(temperature_levels)) +
  #scale_fill_viridis_d(name = "Moisture Content", breaks = c("ad", "95rh"), labels = c("air-dried", "hydrated at 95% RH")) +
  theme_bw() + theme(panel.background = element_rect(fill = "transparent"), text = element_text(size=24))# + labs(fill = "MC", color = "MC")
print(cs_cs_diff_coeff_mu_box_plt)
ggsave(filename="./figures/cs_cs_different_fraction_coeff_mu_box_plt.png", plot=cs_cs_diff_coeff_mu_box_plt, width = 8, height = 6, units = "in", dpi = 450)

## aggregated data EDA; Adhesion force CS same fraction of IPM paper #1
pal <-  viridis(4) ## pallet for viridis color
cs_cs_coeff_c_box_plt <-
  #ggplot(subset(data_ipm_cs,  (fraction_pair=='cob-cob' | fraction_pair=='husk-husk' | fraction_pair=='leaf-leaf' | fraction_pair=='stalk-stalk') & mu > 0.005,
  ggplot(subset(data_ipm_cs, (fraction_pair=='cob-cob' | fraction_pair=='husk-husk' | fraction_pair=='leaf-leaf' | fraction_pair=='stalk-stalk') & mc=='ad' & mu > 0.005,
                select = c("fraction_pair", "c")), aes(x = fraction_pair, y = c)) + # , group = mc)) +
  geom_boxplot(outlier.alpha = 0.1, position = position_dodge2(.9), fill = pal[4]) +
  stat_summary(fun = mean, geom = 'point', shape = 23, size = 5, fill = 3) +
  geom_jitter(shape = 16, alpha = 0.25, size = 2, width = 0.1) +
  scale_y_continuous(name = expression(Adhesion~Force~(N)), limits = c(-0.5, 3.5), expand = c(0.0, 0.0)) +
  ## scale_x_continuous(limits = c(0, 75.0), expand = c(0.0, 0.0)) +
  ## geom_errorbar(position = position_dodge2(width = 0.9, padding = 0.5), color = "gray7", alpha = 0.8) +
  xlab("Fraction Pair") + # + facet_grid(cols = vars(mc_levels), rows = vars(temperature_levels)) +
                                        #scale_fill_viridis_d(name = "Moisture Content", breaks = c("ad", "95rh"), labels = c("air-dried", "hydrated at 95% RH")) +
  theme_bw() + theme(panel.background = element_rect(fill = "transparent"), text = element_text(size=24))# + labs(fill = "MC", color = "MC")

print(cs_cs_coeff_c_box_plt)
ggsave(filename="./figures/cs_cs_same_fraction_coeff_c_box_plt.png", plot=cs_cs_coeff_c_box_plt, width = 8, height = 6, units = "in", dpi = 450)
