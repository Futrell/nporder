library(tidyverse)
library(broom)

assert = stopifnot

frequency = read_csv("../data/frequency.csv")
dryer = read_csv("../data/dryer.csv")
cinque = read_csv("../data/cinque_merlo.csv") %>%
    mutate(first=ifelse(is.na(first), "NA", first),
           second=ifelse(is.na(second), "NA", second),
           third=ifelse(is.na(third), "NA", third))
cysouw = read_csv("../data/cysouw.csv")

d = frequency %>%
    inner_join(dryer) %>%
    inner_join(cinque) %>%
    inner_join(cysouw) %>%
    mutate(rounded_adjusted_frequency=round(adjusted_frequency, 0))
assert(nrow(d) == 24)

dryer_model = glm(rounded_adjusted_frequency ~
                      icon1 +
                      icon2 +
                      asym +
                      harmony +
                      nadj,
                  family="poisson",
                  data=d)

dryer_model2 = glm(rounded_adjusted_frequency ~
                      icon1 +
                      icon2 +
                      asym +
                      harmony,
                  family="poisson",
                  data=d)

cinque_model = glm(rounded_adjusted_frequency ~
                       violates_lca +
                       partial +
                       complete,
                   family="poisson", 
                   data=d)

cinque_model2 = glm(rounded_adjusted_frequency ~
                       first +
                       second +
                       third +
                       partial +
                       complete,
                   family="poisson", 
                   data=d)

cysouw_model = glm(rounded_adjusted_frequency ~
                       na_adjacency +
                       n_edge +
                       d_edge,
                   family="poisson",
                   data=d)

print("Dryer:")
print(summary(dryer_model))
print(logLik(dryer_model))
print("")
      
print("Cinque (single LCA feature)")
print(summary(cinque_model))
print(logLik(cinque_model))
print("")

print("Cinque (multiple merge features):")
print(summary(cinque_model2))
print(logLik(cinque_model2))
print("")

print("Cysouw:")
print(summary(cysouw_model))
print(logLik(cysouw_model))

d_with_predictions = d %>%
    inner_join(dryer_model %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), dryer_predicted=x)) %>%
    inner_join(cinque_model %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque_predicted=x)) %>%
    inner_join(cysouw_model %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cysouw_predicted=x))

gg_color_hues <- function(n) {
   hues = seq(15, 375, length=n+1)
   hcl(h=hues, l=65, c=100)[1:n]
}

hues = c("black", gg_color_hues(3))


d_with_predictions %>%
    mutate(order=factor(order, levels=reorder(order, -d$adjusted_frequency))) %>%
    select(order, adjusted_frequency, dryer_predicted, cinque_predicted, cysouw_predicted) %>%
    gather(key, frequency, -order) %>%
    mutate(key=factor(key, levels=c("adjusted_frequency", "dryer_predicted", "cysouw_predicted", "cinque_predicted"))) %>%
    mutate(real=key == "adjusted_frequency") %>%
    ggplot(aes(x=1, y=frequency, fill=key)) +
      geom_bar(stat="identity", position=position_dodge()) +
      facet_wrap(~order) +
      theme_bw() +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            legend.title=element_blank()) +
      #scale_alpha_discrete(range=c(.6, 1), guide=FALSE) +
      scale_fill_manual(labels=c("Adjusted Frequency", "Current Prediction", "Cysouw Prediction", "Cinque Prediction"),
                        values=hues)

ggsave("../output/model_plots.pdf", width=7, height=5)

hues = gg_color_hues(3)
 
d_with_predictions %>%
    mutate(order=factor(order, levels=reorder(order, -d$adjusted_frequency))) %>%
    select(order, adjusted_frequency, dryer_predicted, cinque_predicted, cysouw_predicted) %>%
    mutate(dryer=dryer_predicted - adjusted_frequency, cysouw=cysouw_predicted - adjusted_frequency, cinque=cinque_predicted - adjusted_frequency) %>%
    select(-adjusted_frequency) %>%
    gather(key, discrepancy, -order) %>%
    mutate(key=factor(key, levels=c("dryer", "cysouw", "cinque")))  %>%
    filter(!is.na(key)) %>%
    ggplot(aes(x=1, y=discrepancy, fill=key)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_hline(yintercept=0, color="black") +
      facet_wrap(~order) +
      ylab("Discrepancy from adjusted frequency") +
      theme_bw() +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            legend.title=element_blank()) +
      scale_fill_manual(labels=c("Current work", "Cysouw", "Cinque"), values=hues)

ggsave("../output/model_discrepancies.pdf", width=7, height=5)
    
 
