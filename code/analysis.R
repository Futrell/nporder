library(tidyverse)
library(broom)
library(stringr)

correct_cinque_features = TRUE # apply corrections to Merlo featurization
markedness_sign_convention = TRUE  # enforce consistency in markedness sign convention

assert = stopifnot

frequency = read_csv("../data/frequency.csv")
dryer = read_csv("../data/dryer.csv")
cinque = read_csv("../data/cinque_merlo.csv") %>%
    mutate(first=ifelse(is.na(first), "NA", first),
           second=ifelse(is.na(second), "NA", second),
           third=ifelse(is.na(third), "NA", third))
cinque2 = read_csv("../data/cinque_levy.csv") %>% select(-X1, -Notes)
cinque3 = read_csv("../data/cinque_cysouw.csv") %>% select(-X1)
cysouw = read_csv("../data/cysouw.csv")

if (markedness_sign_convention) {
    cysouw = cysouw %>%
    mutate(na_adjacency=!na_adjacency,
           n_edge=!n_edge,
           d_edge=!d_edge,
           na_order=!na_order)
}

if(correct_cinque_features) {
  cinque[10,"partial"] = "np"
  cinque[19,"partial"] = "not"
  cinque[19,"complete"] = "np"
  cinque[24,"partial"] = "not" # this one is confusing but a careful read of Cinque suggests that he does not consider this case to involve partial movement
}

d = frequency %>%
    inner_join(dryer) %>%
    inner_join(cinque) %>%
    inner_join(cinque2) %>%
    inner_join(cinque3) %>%
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
                       Merlo_AlternativeMergeOrder +
                       partial +
                       complete,
                   family="poisson", 
                   data=d)

cinque2_model_all = glm(rounded_adjusted_frequency ~
                        AlternativeMergeOrder +
                        whose_pic_move +
                        np_move_no_pp +
                        pic_of_who_move +
                        partial_move +
                        np_extraction +
                        total_move,
                    family="poisson",
                    data=d)

cinque2_model_unmarked = glm(rounded_adjusted_frequency ~
                        AlternativeMergeOrder +
                        np_move_no_pp +
                        pic_of_who_move +
                        np_extraction +
                        partial_move,
                    family="poisson",
                    data=d)

cinque2_model_markedness = d %>%
    mutate(markedness=np_move_no_pp + partial_move) %>%
    glm(rounded_adjusted_frequency ~
            AlternativeMergeOrder +
            pic_of_who_move +
            np_extraction +
            markedness,
        family="poisson",
        data=.)


cinque3_model_all = glm(rounded_adjusted_frequency ~
                        AlternativeMergeOrder +
                        cy_whose_pic_move +
                        cy_np_move_no_pp +
                        cy_pic_of_who_move +
                        cy_partial_move +
                        cy_np_extraction,
                    family="poisson",
                    data=d)

cinque3_model_unmarked = glm(rounded_adjusted_frequency ~
                        AlternativeMergeOrder +
                        cy_np_move_no_pp +
                        cy_pic_of_who_move +
                        cy_partial_move,
                    family="poisson",
                    data=d)

cinque3_model_markedness = d %>%
    mutate(markedness=cy_np_move_no_pp + cy_partial_move) %>%
    glm(rounded_adjusted_frequency ~
            AlternativeMergeOrder +
            cy_pic_of_who_move +
            markedness,
        family="poisson",
        data=.)

cysouw_model = glm(rounded_adjusted_frequency ~
                       na_adjacency +
                       n_edge +
                       d_edge +
                       na_order,
                   family="poisson",
                   data=d)

dryer_model_genera = glm(genera ~
                      icon1 +
                      icon2 +
                      asym +
                      harmony +
                      nadj,
                  family="poisson",
                  data=d)

dryer_model2_genera = glm(genera ~
                      icon1 +
                      icon2 +
                      asym +
                      harmony,
                  family="poisson",
                  data=d)

cinque_model_genera = glm(genera ~
                       Merlo_AlternativeMergeOrder +
                       partial +
                       complete,
                   family="poisson", 
                   data=d)

cinque2_model_all_genera = glm(genera ~
                        AlternativeMergeOrder +
                        whose_pic_move +
                        np_move_no_pp +
                        pic_of_who_move +
                        np_extraction +
                        partial_move +
                        total_move,
                    family="poisson",
                    data=d)

cinque2_model_unmarked_genera = glm(genera ~
                        AlternativeMergeOrder +
                        np_move_no_pp +
                        pic_of_who_move +
                        np_extraction +
                        partial_move,
                    family="poisson",
                    data=d)

cinque2_model_markedness_genera = d %>%
    mutate(markedness=np_move_no_pp + partial_move) %>%
    glm(genera ~
            AlternativeMergeOrder +
            pic_of_who_move +
            np_extraction +
            markedness,
        family="poisson",
        data=.)

cinque3_model_all_genera = glm(genera ~
                        AlternativeMergeOrder +
                        cy_whose_pic_move +
                        cy_np_move_no_pp +
                        cy_pic_of_who_move +
                        cy_partial_move +
                        cy_np_extraction,
                    family="poisson",
                    data=d)

cinque3_model_unmarked_genera = glm(genera ~
                        AlternativeMergeOrder +
                        cy_np_move_no_pp +
                        cy_pic_of_who_move +
                        cy_partial_move,
                    family="poisson",
                    data=d)

cinque3_model_markedness_genera = d %>%
    mutate(markedness=cy_np_move_no_pp + cy_partial_move) %>%
    glm(genera ~
            AlternativeMergeOrder +
            cy_pic_of_who_move +
            markedness,
        family="poisson",
        data=.)


cysouw_model_genera = glm(genera ~
                       na_adjacency +
                       n_edge +
                       d_edge +
                       na_order,
                   family="poisson",
                   data=d)



print("Dryer:")
print(summary(dryer_model))
print(logLik(dryer_model))
print("")
      
print("Cinque/Merlo (single LCA feature)")
print(summary(cinque_model))
print(logLik(cinque_model))
print("")

print("Cysouw:")
print(summary(cysouw_model))
print(logLik(cysouw_model))

d_with_predictions = d %>%
    inner_join(dryer_model %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), dryer_predicted=x)) %>%
    inner_join(cinque_model %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque_predicted=x)) %>%
    inner_join(cinque2_model_all %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque2_all_predicted=x)) %>%
    inner_join(cinque2_model_unmarked %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque2_unmarked_predicted=x)) %>%
    inner_join(cinque2_model_markedness %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque2_markedness_predicted=x)) %>%
    inner_join(cysouw_model %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cysouw_predicted=x)) %>%
    inner_join(dryer_model_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), dryer_predicted_genera=x)) %>%
    inner_join(cinque_model_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque_predicted_genera=x)) %>%
    inner_join(cinque2_model_all_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque2_all_predicted_genera=x)) %>%
    inner_join(cinque2_model_unmarked_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque2_unmarked_predicted_genera=x)) %>%
    inner_join(cinque2_model_markedness_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque2_markedness_predicted_genera=x)) %>%
    inner_join(cysouw_model_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cysouw_predicted_genera=x)) %>%
    inner_join(cinque3_model_all %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque3_all_predicted=x)) %>%
    inner_join(cinque3_model_unmarked %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque3_unmarked_predicted=x)) %>%
    inner_join(cinque3_model_markedness %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque3_markedness_predicted=x)) %>%
    inner_join(cinque3_model_all_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque3_all_predicted_genera=x)) %>%
    inner_join(cinque3_model_unmarked_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque3_unmarked_predicted_genera=x)) %>%
    inner_join(cinque3_model_markedness_genera %>% predict(type="response") %>% tidy() %>% transmute(X1=as.numeric(names), cinque3_markedness_predicted_genera=x))

gg_color_hues <- function(n) {
   hues = seq(15, 375, length=n+1)
   hcl(h=hues, l=65, c=100)[1:n]
}

hues = gg_color_hues(4)

d_with_predictions %>%
    mutate(order=factor(order, levels=reorder(order, -d$adjusted_frequency))) %>%
    select(order, adjusted_frequency, dryer_predicted, cysouw_predicted, cinque2_all_predicted) %>%
    gather(key, frequency, -order) %>%
    mutate(key=factor(key, levels=c("adjusted_frequency", "dryer_predicted", "cysouw_predicted", "cinque2_all_predicted"))) %>%
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
      scale_fill_manual(labels=c("Adjusted Frequency", "Dryer Prediction", "Cysouw Prediction", "Cinque Prediction"), values=c("black", hues))

ggsave("../output/model_plots.pdf", width=7, height=5)

signed_square = function (x) {
    x * abs(x)
}

chisq_discrepancy = function(observed, expected) {
    signed_square(observed - expected) / expected
}


d_with_predictions %>%
    mutate(order=factor(order, levels=reorder(order, -d$adjusted_frequency))) %>%
    select(order, adjusted_frequency, dryer_predicted, cysouw_predicted, cinque2_all_predicted) %>%
    mutate(dryer=dryer_predicted - adjusted_frequency, cysouw=cysouw_predicted - adjusted_frequency, cinque2=cinque2_all_predicted - adjusted_frequency) %>%
    select(-adjusted_frequency) %>%
    gather(key, discrepancy, -order) %>%
    mutate(key=factor(key, levels=c("dryer", "cysouw", "cinque2")))  %>%
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
      scale_fill_manual(labels=c("Dryer", "Cysouw", "Cinque"), values=hues)

ggsave("../output/model_discrepancies.pdf", width=7, height=5)

d_with_predictions %>%
    mutate(order=factor(order, levels=reorder(order, -d$adjusted_frequency))) %>%
    select(order, adjusted_frequency, dryer_predicted, cysouw_predicted, cinque2_all_predicted) %>%
    mutate(dryer=chisq_discrepancy(adjusted_frequency, dryer_predicted), cysouw=chisq_discrepancy(adjusted_frequency, cysouw_predicted), cinque2=chisq_discrepancy(adjusted_frequency, cinque2_all_predicted)) %>%
    select(-adjusted_frequency) %>%
    gather(key, discrepancy, -order) %>%
    mutate(key=factor(key, levels=c("dryer", "cysouw", "cinque2")))  %>%
    filter(!is.na(key)) %>%
    ggplot(aes(x=1, y=discrepancy, fill=key)) +
      geom_bar(stat="identity", position=position_dodge()) +
      facet_wrap(~order) +
      ylab("Chi-squared discrepancy from adjusted frequency") +
      theme_bw() +
      geom_hline(yintercept=0, color="black") +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            legend.title=element_blank()) +
      scale_fill_manual(labels=c("Dryer", "Cysouw", "Cinque"), values=hues)

ggsave("../output/model_discrepancies_chisq.pdf", width=7, height=5)

d_with_predictions %>%
    mutate(order=factor(order, levels=reorder(order, -d$genera))) %>%
    select(order, genera, dryer_predicted_genera, cysouw_predicted_genera, cinque2_all_predicted_genera) %>%
    gather(key, frequency, -order) %>%
    mutate(key=factor(key, levels=c("genera", "dryer_predicted_genera", "cysouw_predicted_genera", "cinque2_all_predicted_genera"))) %>%
    mutate(real=key == "genera") %>%
    ggplot(aes(x=1, y=frequency, fill=key)) +
      geom_bar(stat="identity", position=position_dodge()) +
      facet_wrap(~order) +
      theme_bw() +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            legend.title=element_blank()) +
      #scale_alpha_discrete(range=c(.6, 1), guide=FALSE) +
      scale_fill_manual(labels=c("Genera", "Dryer Prediction", "Cysouw Prediction", "Cinque Prediction"),
                        values=c("black", hues))

ggsave("../output/model_plots_genera.pdf", width=7, height=5)


d_with_predictions %>%
    mutate(order=factor(order, levels=order[reorder(order, -d$genera)])) %>%
    select(order, genera, dryer_predicted_genera, cysouw_predicted_genera, cinque2_all_predicted_genera) %>%
    mutate(dryer=dryer_predicted_genera - genera, cysouw=cysouw_predicted_genera - genera, cinque2=cinque2_all_predicted_genera - genera) %>%
    select(-genera) %>%
    gather(key, discrepancy, -order) %>%
    mutate(key=factor(key, levels=c("dryer", "cysouw", "cinque2")))  %>%
    filter(!is.na(key)) %>%
    ggplot(aes(x=1, y=discrepancy, fill=key)) +
      geom_bar(stat="identity", position=position_dodge()) +
      geom_hline(yintercept=0, color="black") +
      facet_wrap(~order) +
      ylab("Discrepancy from genera") +
      theme_bw() +
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            legend.title=element_blank()) +
      scale_fill_manual(labels=c("Dryer", "Cysouw", "Cinque"), values=hues)

ggsave("../output/model_discrepancies_genera.pdf", width=7, height=5)

d_with_predictions %>%
    mutate(order=factor(order, levels=order[reorder(order, -d$genera)])) %>%
    select(order, genera, dryer_predicted_genera, cysouw_predicted_genera, cinque2_all_predicted_genera) %>%
    mutate(dryer=chisq_discrepancy(genera, dryer_predicted_genera), cysouw=chisq_discrepancy(genera, cysouw_predicted_genera), cinque2=chisq_discrepancy(genera, cinque2_all_predicted_genera)) %>%
    select(-genera) %>%
    gather(key, discrepancy, -order) %>%
    mutate(key=factor(key, levels=c("dryer", "cysouw", "cinque2")))  %>%
    filter(!is.na(key)) %>%
    ggplot(aes(x=1, y=discrepancy, fill=key)) +
      geom_bar(stat="identity", position=position_dodge()) +
      facet_wrap(~order) +
      ylab("Chi-squared discrepancy from genera") +
      theme_bw() +
      geom_hline(yintercept=0, color="black") +    
      theme(axis.ticks.x=element_blank(),
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            legend.title=element_blank()) +
      scale_fill_manual(labels=c("Dryer", "Cysouw", "Cinque"), values=hues)

ggsave("../output/model_discrepancies_chisq_genera.pdf", width=7, height=5)


regression_table_figure = function(model, depvar, filename) {
    coefficients = model %>%
        tidy() %>%
        select(term, estimate) %>%
        mutate(term=gsub("TRUE$", "", term))

    data = model$data %>% rename_(dependent=depvar)

    table = data %>%
        gather(term, value, -order, -dependent) %>%
        filter(term %in% coefficients$term) %>%
        mutate(value=parse_logical(value)) %>%
        inner_join(coefficients) %>%
        mutate(wvalue = as.numeric(value) * estimate) %>%
        select(order, dependent, term, wvalue)

    assert(nrow(table) == 24 * length(unique(table$term)))

    table_cell = function(x) {
        if (is.numeric(x)) {
            ifelse(x == 0, "-", "+")
        } else if (is.logical(x)) {
            ifelse(x, "+", "-")
        } else {
            x
        }
    }

    table %>%
        mutate(order=factor(order, levels=levels(reorder(data$order, data$dependent)))) %>%
        mutate(term=factor(term, levels=levels(reorder(coefficients$term, coefficients$estimate)))) %>%
        ggplot(aes(x=term, y=order, fill=wvalue, label=table_cell(wvalue))) +
        geom_tile() +
        geom_text(color="white") +
        theme_bw() +
        scale_x_discrete(position="top") +
        xlab("Factor") +
        ylab("NP Order") +
        labs(fill="Weight") +
        geom_text(aes(label=formatC(dependent, format="f", digits=1), x=0), hjust=-0.3) +
        theme(panel.grid.major = element_blank(),
              axis.text.x=element_text(angle=45,
                                       hjust=0))

    ggsave(str_c("../output/", filename), height=8.25, width=7.5)
    print("Plot saved in file:")
    print(str_c("../output/", filename))

}

regression_table_figure(dryer_model, "adjusted_frequency", "dryer_regression_af.pdf")
regression_table_figure(cinque2_model_all, "adjusted_frequency", "cinque_regression_af.pdf")
regression_table_figure(dryer_model_genera, "genera", "dryer_regression_genera.pdf")
regression_table_figure(cinque2_model_all_genera, "genera", "cinque_regression_genera.pdf")

