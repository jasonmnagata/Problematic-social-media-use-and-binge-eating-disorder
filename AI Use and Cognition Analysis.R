library(emmeans)
library(dplyr)

#cleaned_6 contains the participant data with cognition, AI use, and covariates

cognition <- c("flanker", "oral_read", "pic_seq", "patt_comp", "pic_vocab", "composite_cog", "lswmt", "card_stack", "crys", "fluid")
ai_vars <- c("ai_chatbot_use_imputed", "ai_companion_binary_imputed", "ai_educ_binary_imputed", "ai_general_binary_imputed")
covariates <- c("sex", "race", "income", "education", "age", "site")

#lm models
all_models <- list()
for (ai_type in ai_vars) {
  all_models[[ai_type]] <- list()
  
  for (outcome in cognition) {
    formula_lm <- reformulate(c(ai_type, covariates), response=outcome)
    all_models[[ai_type]][[outcome]] <- lm(formula_lm, data=cleaned_6)
  }
}

#obtain emm
emm_results <- data.frame()
contrast_results <- data.frame()

for (ai_type in ai_vars) {
  for (outcome in cognition) {
    mod <- all_models[[ai_type]][[outcome]]
    
    emm <- emmeans(mod, specs=ai_type, rg.limit=20000)
    emm_df <- as.data.frame(summary(emm, infer=T))
    emm_df$outcome <- outcome
    emm_df$ai_type <- ai_type
    names(emm_df)[names(emm_df)==ai_type] <- "level"
    emm_results <- bind_rows(emm_results, emm_df)
    
    contrast_df <- as.data.frame(summary(pairs(emm, reverse=T),infer=T))
    contrast_df$outcome <- outcome
    contrast_df$ai_type <- ai_type
  
    contrast_results <- bind_rows(contrast_results, contrast_df)
  }
}

emm_results
contrast_results
