library(plyr)
library(magrittr)
library(ggplot2)
library(extrafont)
loadfonts()
library(lme4)

obstacle_distance <- 5

e <- na.omit(read.table('exp2/e2.csv', header=TRUE, sep=','))
e$action <- ordered(e$action, c('over', 'around'))
for (col in c('participant', 'phase', 'gender')) {
  e[[col]] = as.factor(e[[col]])
}
e$tan_angle <- (e$width/2) / obstacle_distance
e$angle <- atan(e$tan_angle)
e$height <- e$height + 1
e$h_max <- e$lowest_height_not_afforded + 1
e$lowest_height_not_afforded <- NULL

n_return_actions <- with(subset(e, e$phase == 'return'), tapply(action, list(participant), . %>% unique %>% length))
drop_return_participants <- names(n_return_actions[n_return_actions == 1])
e <- e[!(e$phase == 'return' & e$participant %in% drop_return_participants),]

n_trials <- e$trial %>% unique %>% length
e <- e[!with(e, participant == 15 & phase == 'outbound' & trial <= n_trials/2), ]

h_crit_glm <- function(d) {
  glm(action ~ height, family=binomial(link='logit'), data = d)
}
p.glm <- function(mdl, p=.5) {
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

data <- ddply(e, .(participant, width, phase), ddply.fun)
data$scaled_height.50 <- data$h_crit / data$h_max
data$rel_height.50 <- data$h_crit - data$h_max

get_unique <- function(df, column) {
  df %>% extract(column) %>% unique %>% c %>% extract2(1)
}
widths <- get_unique(e, 'width')
relative_heights <- get_unique(e, 'relative_height')

fig.dims <- c(5, 5)
font.family <- 'Ubuntu'
font.size <- 20
width_x <- scale_x_continuous(breaks=widths, name='Obstacle width (ft.)')
tan_angle_x <- scale_x_continuous(breaks=widths/10, name=expression(tan(theta)))
crit_relative_height_y <- scale_y_continuous(breaks=c(relative_heights, 0), name=expression(h[crit] - h[max] ~~ plain('(in.)')))
crit_scaled_height_y <- scale_y_continuous(name=expression(frac(h[crit.], h[max])))
upright_ylabel <- theme(
  axis.title.y=element_text(angle=0)
)
plot_theme_bw <- theme_bw() + theme(
  text=element_text(family=font.family, size=font.size)
)
no_x_grid <- theme(
  panel.grid.major.x=element_blank(),
  panel.grid.minor.x=element_blank()
)

ggplot(data, aes(x=width, y=rel_height.50)) +
  stat_summary(geom='bar', fun.y=mean, alpha=0.75) +
  stat_summary(geom='errorbar', width=0.5, size=1,
    fun.ymax=. %>% {mean(.) + sem(.)},
    fun.ymin=. %>% {mean(.) - sem(.)}
  ) +
  width_x + crit_relative_height_y + plot_theme_bw + no_x_grid
ggsave('critical_relative_heights.png', width=fig.dims[1], height=fig.dims[2])

ggplot(data, aes(x=width, y=scaled_height.50)) +
  stat_summary(geom='bar', fun.y=mean, alpha=0.75) +
  stat_summary(geom='errorbar', width=0.5, size=1,
               fun.ymax=. %>% {mean(.) + sem(.)},
               fun.ymin=. %>% {mean(.) - sem(.)}
  ) +
  width_x + plot_theme_bw + no_x_grid + crit_scaled_height_y  + upright_ylabel +
  coord_cartesian(ylim=c(0.5, 1))
ggsave('critical_scaled_heights.png', width=fig.dims[1], height=fig.dims[2])

cond_means <- lmer(h_crit ~ 1 + (1|participant), data, REML = FALSE)
h_max <- lmer(h_crit ~ h_max + (1|participant), data, REML = FALSE)
anova(cond_means, h_max, test = 'ChiSq')

h_max_noint <- lmer(h_crit ~ -1 + h_max + (1|participant), data, REML = FALSE)
anova(h_max_noint, h_max, test = 'ChiSq')

means <- lmer(h_crit ~ -1 + (1|participant), data, REML = FALSE)
anova(means, h_max_noint, test = 'ChiSq')

age <- lmer(h_crit ~ -1 + age + (1|participant), data, REML = FALSE)
anova(age, h_max_noint)

knee_height <- lmer(h_crit ~ -1 + knee_height + (1|participant), data, REML = FALSE)
anova(knee_height, h_max_noint)

hip_height <- lmer(h_crit ~ -1 + hip_height + (1|participant), data, REML = FALSE)
anova(knee_height, hip_height)

both_heights <- lmer(h_crit ~ -1 + hip_height + knee_height + (1|participant), data, REML = FALSE)
anova(hip_height, both_heights, test = 'ChiSq')

hip_and_max <- lmer(h_crit ~ -1 + hip_height + h_max + (1|participant), data, REML = FALSE)
anova(hip_height, hip_and_max, test= 'ChiSq')

hip_and_int <- lmer(h_crit ~ 1 + hip_height + (1|participant), data, REML = FALSE)
anova(hip_height, hip_and_int, test = 'ChiSq')

hip_and_age <- lmer(h_crit ~ -1 + hip_height + age + (1|participant), data, REML = FALSE)
anova(hip_height, hip_and_age, test = 'ChiSq')

widths <- lmer(h_crit ~ -1 + hip_height + width + (1|participant), data, REML = FALSE)
anova(hip_height, widths, test = 'ChiSq')

angle <- lmer(h_crit ~ -1 + hip_height + angle + (1|participant), data, REML = FALSE)
anova(widths, angle)

tan_angle <- lmer(h_crit ~ -1 + hip_height + tan(angle) + (1|participant), data, REML = FALSE)
anova(angle, tan_angle)

angle_raneff <- lmer(h_crit ~ -1 + hip_height + angle + (1 + width|participant), data, REML = FALSE)
anova(angle, angle_raneff, test = 'ChiSq')

width_int <- lmer(h_crit ~ -1 + h_max*width + (1|participant), data, REML = FALSE)
anova(widths, width_int, test = 'ChiSq')

phase <- lmer(h_crit ~ -1 + hip_height + angle + phase + (1|participant), data, REML = FALSE)
anova(angle, phase, test = 'ChiSq')

gender <- lmer(h_crit ~ -1 + hip_height + angle + gender + (1|participant), data, REML = FALSE)
anova(angle, gender, test = 'ChiSq')

phase_raneff <- lmer(h_crit ~ -1 + hip_height + angle + (1 + phase|participant), data, REML = FALSE)
anova(angle, phase_raneff, test = 'ChiSq')

final <- phase_raneff
summary(final)

data$crit_hip_prop <- with(data, h_crit / hip_height)
crit_hip_prop_y <- scale_y_continuous(name=expression(h[crit] / h[hip] ~~ plain('(in.)')))

ggplot(data, aes(x=width, y=crit_hip_prop)) +
  stat_summary(geom='bar', fun.y=mean, alpha=0.75) +
  stat_summary(geom='errorbar', width=0.5, size=1,
               fun.ymax=. %>% {mean(.) + sem(.)},
               fun.ymin=. %>% {mean(.) - sem(.)}
  ) +
  width_x + crit_hip_prop_y + plot_theme_bw + no_x_grid +
  coord_cartesian(ylim = c(0.35, 0.7))

sub <- subset(data, data$width < 10)

means <- lmer(h_crit ~ -1 + (1|participant), sub, REML = FALSE)
int <- lmer(h_crit ~ 1 + (1|participant), sub, REML = FALSE)
h_max <- lmer(h_crit ~ -1 + h_max + (1|participant), sub, REML = FALSE)
knee <- lmer(h_crit ~ -1 + knee_height + (1|participant), sub, REML = FALSE)
hip <- lmer(h_crit ~ -1 + hip_height + (1|participant), sub, REML = FALSE)
age <- lmer(h_crit ~ -1 + age + (1|participant), sub, REML = FALSE)
anova(means, int, h_max, knee, hip, age, test = 'ChiSq')
best <- hip

anova(best, update(best, . ~ . + 1), test = 'ChiSq')
anova(best, update(best, . ~ . + knee_height), test = 'ChiSq')
anova(best, update(best, . ~ . + age), test = 'ChiSq')
anova(best, update(best, . ~ . + gender), test = 'ChiSq')
anova(best, update(best, . ~ . + h_max), test = 'ChiSq')

width <- update(best, . ~ . + width)
angle <- update(best, . ~ . + angle)
tan_angle <- update(best, . ~ . + tan_angle)
anova(best, width, angle, tan_angle, test = 'ChiSq')
best <- update(best, . ~ . + tan_angle)

anova(best, update(best, . ~ . + (tan_angle|participant)), test = 'ChiSq')
anova(best, update(best, . ~ . + tan_angle:hip_height), test = 'ChiSq')
anova(best, update(best, . ~ . + phase), test = 'ChiSq')
anova(best, update(best, . ~ . + (phase|participant)), test = 'ChiSq')

summary(best)