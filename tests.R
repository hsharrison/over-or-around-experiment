library(Hmisc)
library(magrittr)
library(lme4)
library(boot)

obstacle_distance <- 5

e <- na.omit(read.table('e2.csv', header=TRUE, sep=','))
e$action <- ordered(e$action, c('over', 'around'))
for (col in c('participant', 'phase', 'gender')) {
  e[[col]] = as.factor(e[[col]])
}
e$angle <- atan((e$width/2) / obstacle_distance)

n_return_actions <- with(subset(e, e$phase == 'return'), tapply(action, list(participant), . %>% unique %>% length))
drop_return_participants <- names(n_return_actions[n_return_actions == 1])
e <- e[!(e$phase == 'return' & e$participant %in% drop_return_participants),]

n_trials <- e$trial %>% unique %>% length
e <- e[!with(e, participant == 15 & phase == 'outbound' & trial <= n_trials/2), ]


e.glmer <- function(formula) {
  glmer(formula, data=e, family=binomial(link='logit'))
}

cond_means <- e.glmer(action ~ 1 + (1 | participant))

rel_height_only <- e.glmer(action ~ relative_height + (1 | participant))
scaled_height_only <- e.glmer(action ~ scaled_height + (1 | participant))
anova(cond_means, rel_height_only, test='ChiSquare')
anova(cond_means, scaled_height_only, test='ChiSquare')
mdl <- rel_height_only

height_and_width <- e.glmer(action ~ relative_height + width + (1 | participant))
anova(mdl, height_and_width, test='ChiSquare')
mdl <- height_and_width

height_width_interaction <- e.glmer(action ~ relative_height * width + (1 | participant))
anova(mdl, height_width_interaction, test='ChiSquare')

height_random_effect <- e.glmer(action ~ relative_height + width + (1 + relative_height | participant))
anova(mdl, height_random_effect, test='ChiSquare')
mdl <- height_random_effect

width_random_effect <- e.glmer(action ~ relative_height + width + (1 + relative_height + width | participant))
anova(mdl, width_random_effect, test='ChiSquare')
mdl <- width_random_effect

random_effect_interactions <- e.glmer(action ~ relative_height * width + (1 + relative_height + width | participant))
anova(mdl, random_effect_interactions, test='ChiSquare')

trial <- e.glmer(action ~ relative_height + width + trial + (1 + relative_height + width | participant))
anova(mdl, trial, test='ChiSquare')

gender <- e.glmer(action ~ relative_height + width + as.factor(gender) + (1 + relative_height + width | participant))
anova(mdl, gender, test='ChiSquare')

age <- e.glmer(action ~ relative_height + width + age + (1 + relative_height + width | participant))
anova(mdl, age, test='ChiSquare')

with_phase <- e.glmer(action ~ relative_height + width + phase + (1 + relative_height + width | participant))
anova(mdl, with_phase, test='ChiSquare')
mdl <- with_phase

phase_height_int <- e.glmer(action ~ relative_height + width + phase + relative_height:phase +
                            (1 + relative_height + width | participant))
anova(mdl, phase_height_int, test='ChiSquare')
mdl <- phase_height_int

phase_width_int <- e.glmer(action ~ relative_height + width + phase + relative_height:phase + width:phase +
                           (1 + relative_height + width | participant))
anova(mdl, phase_width_int)

summary(mdl)
bootMer(mdl, fixef, nsim=1000, type='parametric', .progress='txt')
