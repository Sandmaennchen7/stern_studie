##== pretesting assumptions for main hypothesis============
##how do we group exp_new and exp_old together, so that we can t.test
##do we t.test between exp_old and control like in the original study and then exploratively regression analyse the othe condition?

###assumption testing: did our exp condition elicit more awe than control 
  #-> basis for pur behavioural humility testing

dat$condition <- as.factor(dat$condition)


##regression analysis
fit_awe <- lm(awe ~ condition * screen, data= dat)
#bzw
#fit_awe <- lm(awe ~ condition * screen_control, data= dat)
  ##testing exp_old -control
    hyp_awe <- glht(fit_awe, linfct = mcp(condition ="exp_old - control<= 0"))

##hypothesis testing -> is there a difference between the conditions + is there an effect of screen_size on awe
summary(hyp_awe)
summary(fit_awe)

