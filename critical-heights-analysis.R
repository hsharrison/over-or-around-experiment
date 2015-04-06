library(plyr)
library(magrittr)
library(ggplot2)
library(extrafont)
loadfonts()
library(lme4)
library(lmerTest)
library(boot)
library(tikzDevice)

obstacle_distance <- 5

e <- na.omit(read.table('exp2/e2.csv', header=TRUE, sep=','))
e$action <- ordered(e$action, c('over', 'around'))
for (col in c('participant', 'phase', 'gender')) {
  e[[col]] = as.factor(e[[col]])
}
e$tan_angle <- (e$width/2) / obstacle_distance
e$angle <- atan(e$tan_angle)
e$height <- e$height + 1
e$lowest_height_not_afforded <- e$lowest_height_not_afforded + 1

n_return_actions <- with(subset(e, e$phase == 'return'), tapply(action, list(participant), . %>% unique %>% length))
drop_return_participants <- names(n_return_actions[n_return_actions == 1])
e <- e[!(e$phase == 'return' & e$participant %in% drop_return_participants),]

n_trials <- e$trial %>% unique %>% length
e <- e[!with(e, participant == 15 & phase == 'outbound' & trial <= n_trials/2), ]

simple_glm <- function(d, predictor) {
  glm(as.formula(paste('action', predictor, sep='~')), family=binomial(link='logit'), data=d)
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

rel_height.50 <- ddply(e, .(participant, width), . %>% simple_glm('relative_height') %>% p.glm)
scaled_height.50 <- ddply(e, .(participant, width), . %>% simple_glm('scaled_height') %>% p.glm)

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

ggplot(rel_height.50, aes(x=width, y=relative_height)) +
  stat_summary(geom='bar', fun.y=mean, alpha=0.75) +
  stat_summary(geom='errorbar', width=0.5, size=1,
    fun.ymax=. %>% {mean(.) + sem(.)},
    fun.ymin=. %>% {mean(.) - sem(.)}
  ) +
  width_x + crit_relative_height_y + plot_theme_bw + no_x_grid
ggsave('critical_relative_heights.png', width=fig.dims[1], height=fig.dims[2])
mer_rel_height <- lmer(relative_height ~ width + (1 + width | participant), rel_height.50)
summary(mer_rel_height)

scaled_height.50$tan_angle <- scaled_height.50$width / (10)
scaled_height.50$angle <- atan(scaled_height.50$tan_angle) * 180/pi

ggplot(scaled_height.50, aes(x=width, y=scaled_height)) +
  stat_summary(geom='bar', fun.y=mean, alpha=0.75) +
  stat_summary(geom='errorbar', width=0.5, size=1,
               fun.ymax=. %>% {mean(.) + sem(.)},
               fun.ymin=. %>% {mean(.) - sem(.)}
  ) +
  width_x + plot_theme_bw + no_x_grid + crit_scaled_height_y  + upright_ylabel +
  coord_cartesian(ylim=c(0.5, 1))
ggsave('critical_scaled_heights.png', width=fig.dims[1], height=fig.dims[2])
mer_scaled_height <- lmer(scaled_height ~ width + (1 + width | participant), scaled_height.50)
summary(mer_scaled_height)
without_width_raneff <- lmer(scaled_height ~ width + (1 | participant), scaled_height.50)
anova(without_width_raneff, mer_scaled_height)
summary(without_width_raneff)

ci <- without_width_raneff %>%
  bootMer(fixef, nsim=10000, type='parametric', .progress='txt') %>%
  boot.ci(type='basic', index=2) %>%
  extract2('basic') %>%
  extract(4:5)
print(c(ci[1], mean(ci), ci[2]))
print(c(mean(ci), (ci[2] - ci[1]) / 2))
