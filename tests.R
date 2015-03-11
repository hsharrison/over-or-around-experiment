library(Hmisc)
library(magrittr)
library(lme4)

e <- na.omit(read.table('e2.csv', header=TRUE, sep=','))
e$action <- ordered(e$action, c('over', 'around'))
for (col in c('participant', 'phase', 'gender')) {
  e[[col]] = as.factor(e[[col]])
}

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

height_and_width <- e.glmer(action ~ relative_height + width + (1 | participant))
anova(rel_height_only, height_and_width, test='ChiSquare')

height_width_interaction <- e.glmer(action ~ relative_height * width + (1 | participant))
anova(height_and_width, height_width_interaction, test='ChiSquare')

height_random_effect <- e.glmer(action ~ relative_height + width + (1 + relative_height | participant))
anova(height_and_width, height_random_effect, test='ChiSquare')

width_random_effect <- e.glmer(action ~ relative_height + width + (1 + relative_height + width | participant))
anova(height_random_effect, width_random_effect, test='ChiSquare')

random_effect_interactions <- e.glmer(action ~ relative_height * width + (1 + relative_height + width | participant))
anova(width_random_effect, random_effect_interactions, test='ChiSquare')

trial <- e.glmer(action ~ relative_height + width + trial + (1 + relative_height + width | participant))
anova(width_random_effect, trial, test='ChiSquare')

gender <- e.glmer(action ~ relative_height + width + as.factor(gender) + (1 + relative_height + width | participant))
anova(width_random_effect, gender, test='ChiSquare')

age <- e.glmer(action ~ relative_height + width + age + (1 + relative_height + width | participant))
anova(width_random_effect, age, test='ChiSquare')

mdl <- width_random_effect
summary(mdl)