library(plyr)
library(magrittr)
library(ggplot2)
library(ggthemes)
library(extrafont)
loadfonts()
library(lme4)
library(binom)
library(showtext)

obstacle_distance <- 5

## Load raw data.
e <- na.omit(read.table('exp2/e2.csv', header = TRUE, sep = ','))
e$action <- ordered(e$action, c('over', 'around'))
for (col in c('participant', 'phase', 'gender')) {
  e[[col]] = as.factor(e[[col]])
}
e$tan_angle <- (e$width/2) / obstacle_distance
e$angle <- atan(e$tan_angle)
e$height <- e$height + 1
e$h_max <- e$lowest_height_not_afforded + 1
e$lowest_height_not_afforded <- NULL

# Drop participants who did the same thing every time on the return.
n_return_actions <- with(subset(e, e$phase == 'return'), tapply(action, list(participant), . %>% unique %>% length))
drop_return_participants <- names(n_return_actions[n_return_actions == 1])
e <- e[!(e$phase == 'return' & e$participant %in% drop_return_participants),]

n_trials <- e$trial %>% unique %>% length
e <- e[!with(e, participant == 15 & phase == 'outbound' & trial <= n_trials/2), ]
trial_data <- e


# Trial-data modeling.
means_mdl <- glmer(action ~ (1|participant), family = binomial(link = 'logit'), trial_data)
rel_height_mdl <- update(means, . ~ . + relative_height)
anova(means, rel_height_mdl)
best_mdl <- rel_height_mdl

widths_mdl <- update(best_mdl, . ~ . + width)
anova(best_mdl, widths_mdl)
best_mdl <- widths_mdl

width_ranef <- update(best_mdl, . ~ . + (width|participant))

rel_height_ranef <- update(best_mdl, . ~ . + (relative_height|participant))

phase_mdl <- update(best_mdl, . ~ . + phase)
anova(best_mdl, phase_mdl)
best_mdl <- phase_mdl

# Determine proportions at each width-height.
condition_data <- ddply(trial_data, .(relative_height, width), summarise,
  n = length(action),
  n_around = sum(action == 'around')
)
confints <- with(condition_data, binom.confint(n_around, n, method = 'wilson'))
condition_data$p_around <- confints$mean
condition_data$ci_low <- confints$lower
condition_data$ci_high <- confints$upper

# Plotting prep.
get_unique <- function(df, column) {
  df %>% extract(column) %>% unique %>% c %>% extract2(1) %>% sort
}
widths <- get_unique(trial_data, 'width')
relative_heights <- get_unique(trial_data, 'relative_height')


# Raw data plots.
relative_height_label <- expression(h[obs.] - h[max.]~~plain('(in.)'))
width_label <- expression(w[obs.]~~plain('(ft.)'))
p_around_label = expression(P(around))

# By width.
setEPS()
postscript('proportions_by_width.eps', width = 10, height = 8)
showtext.begin()
ggplot(condition_data, aes(
  x = relative_height,
  y = p_around,
  ymin = ci_low,
  ymax = ci_high,
  color = width,
  group = width
)) +
  geom_hline(y = 0.5, size = 0.5, linetype = 'dotted') + 
  geom_line(size = 2) +
  geom_errorbar(size = 1, position = 'dodge') +
  scale_x_continuous(breaks = relative_heights, name = relative_height_label) +
  scale_y_continuous(name = p_around_label) +
  scale_color_gradient(breaks = widths, name = width_label, guide = 'legend',
                       low = '#56B4E9', high = 'black') +
  geom_rangeframe(color = 'black') + 
  theme_tufte(base_size = 26, base_family = 'Arial') +
  theme(
    axis.title.y = element_text(angle = 0),
    legend.position = 'top'
)
dev.off()

# By height.
setEPS()
cairo_ps('proportions_by_height.eps', width = 11, height = 7.5)
showtext.begin()
ggplot(condition_data, aes(
  x = width,
  y = p_around,
  ymin = ci_low,
  ymax = ci_high,
  color = relative_height,
  fill = relative_height,
  group = relative_height
)) +
  geom_hline(y = 0.5, size = 0.5, linetype = 'dotted') +
  geom_line(size = 2) +
  geom_ribbon(alpha = 0.1, size = 0) +
  scale_x_continuous(breaks = widths, name = width_label) +
  scale_y_continuous(name = p_around_label) +
  scale_color_continuous(breaks = rev(relative_heights), name = relative_height_label, guide = 'legend',
                         low = 'black', high = 'green') +
  scale_fill_continuous(low = 'black', high = 'green', guide = FALSE) +
  geom_rangeframe(color = 'black') +
  theme_tufte(base_size = 26, base_family = 'Arial') +
  theme(
    axis.title.y = element_text(angle = 0)
)
dev.off()

# Determine critical heights per participant-condition.
h_crit_glm <- function(d) {
  glm(action ~ height, family = binomial(link='logit'), data = d)
}
p.glm <- function(mdl, p = .5) {
  a <- mdl %>% coef %>% extract(1)
  b <- mdl %>% coef %>% extract(2)
  result <- (log(p /(1-p)) - a) / b
  names(result) <- mdl %>% terms %>% labels
  return(result)
}
sem <- function(x) {
  sd(x) / sqrt(length(x))
}

ddply.fun <- function(d, p = .5) {
  result <- d[1, apply(d, 2, . %>% unique %>% length %>% equals(1))]
  
  if ('action' %in% names(result)) {
    result$switched <- FALSE
    
    if (result[1, 'action'] == 'over') {
      # Always went over. Set switching point to maximum height.
      result$h_crit <- result$h_max    
    } else {
      # Always went around. Set switching point to in-between the lowest height and what the next lowest would be if the sequence continued.
      result$h_crit <- min(d$height) - d$height %>% diff %>% mean %>% abs %>% divide_by(2)
    }
    result$action <- NULL
    
  } else {
    result$switched <- TRUE
    result$h_crit <- d %>% h_crit_glm %>% p.glm
  }
  
  result$sorted_order <- NULL
  result$n <- nrow(d)
  return(result)
}

crit_data$phase <- NULL
crit_data <- ddply(trial_data, .(participant, width), ddply.fun)
crit_data$hmax_scaled_crit_height <- crit_data$h_crit / crit_data$h_max
crit_data$rel_crit_height <- crit_data$h_crit - crit_data$h_max
crit_data$hip_scaled_crit_height <- crit_data$h_crit / crit_data$hip_height
crit_data$knee_scaled_crit_height <- crit_data$h_crit / crit_data$knee_height

means_mdl <- lmer(h_crit ~ (1|participant), crit_data, REML = FALSE)
h_max_mdl <- lmer(h_crit ~ h_max + (1|participant), crit_data, REML = FALSE)
h_hip_mdl <- lmer(h_crit ~ hip_height + (1|participant), crit_data, REML = FALSE)
h_knee_mdl <- lmer(h_crit ~ knee_height + (1|participant), crit_data, REML = FALSE)
anova(means_mdl, h_max_mdl, h_hip_mdl, h_knee_mdl, test = 'ChiSq')
best_mdl <- h_max_mdl

without_intercept <- update(best_mdl, . ~ . - 1)
anova(without_intercept, best_mdl)
best_mdl <- without_intercept

tan_angle_mdl <- update(best_mdl, . ~ . + double_tan_angle)
angle_mdl <- update(best_mdl, . ~ . + angle)
anova(best_mdl, tan_angle_mdl, angle_mdl, test = 'ChiSq')
best_mdl <- tan_angle_mdl

tan_angle_ranef <- update(best_mdl, . ~ . + (tan_angle|participant))
anova(best_mdl, tan_angle_ranef)

summary(best_mdl)
anova(best_mdl, update(best_mdl, . ~ . + knee_height))
anova(best_mdl, update(best_mdl, . ~ . + hip_height))

# Critical-heights plot.
crit_data$double_tan_angle <- 2*crit_data$tan_angle
plotted_model <- lm(hmax_scaled_crit_height ~ double_tan_angle, crit_data)
intercept <- coef(plotted_model)[['(Intercept)']]
slope <- coef(plotted_model)[['double_tan_angle']]

tan_angle_label = expression(w[obs.] / d[obs.])
scaled_crit_height_label <- expression(frac(h[crit.], h[max.]))

setEPS()
cairo_ps('critical_heights.eps', width = 9, height = 7)
showtext.begin()
ggplot(crit_data, aes(x = double_tan_angle, y = hmax_scaled_crit_height)) +
  stat_summary(geom = 'bar', fun.y = mean, alpha = 0.75) +
  stat_summary(geom = 'errorbar', size = 1, width = 0.05,
               fun.ymax = . %>% {mean(.) + sem(.)},
               fun.ymin = . %>% {mean(.) - sem(.)}
  ) +
  geom_abline(intercept = intercept, slope = slope,
              linetype = 'dotted', size = 1) +
  annotate(geom = 'text', x = 1.25, y = 0.84, parse = TRUE, family = 'Arial', size = 8,
           label = paste(
             'frac(hat(h[crit.]), h[max.]) ==', round(intercept, 3), '+', round(slope, 3), '~frac(w[obs.],d[obs.])'
           )) +
  scale_x_continuous(breaks = widths / obstacle_distance, name = tan_angle_label) +
  scale_y_continuous(name = scaled_crit_height_label) +
  theme_tufte(base_size = 26, base_family = 'Arial') +
  theme(
    axis.title.y = element_text(angle = 0)
  ) +
  coord_cartesian(ylim = c(0.475, 0.875))
dev.off()


# Affordance boundary analysis.
participant_data <- ddply(trial_data, .(participant), function(d) d[1, apply(d, 2, . %>% unique %>% length %>% equals(1))])
participant_data$phase <- NULL

anova(lm(h_max ~ hip_height*knee_height, participant_data))
summary(lm(h_max ~ hip_height - 1, participant_data))

participant_data$hip_scaled_hmax <- with(participant_data, h_max / hip_height)