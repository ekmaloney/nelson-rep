new_data_format <- function(d, t){
  d <- replace_with_na_all(data = d, condition = ~.x %in% c("."))
  
  if(t == "MTurk"){
    vars_select <- c("MTurkCode", "EndTime", "Attention", "question", "replaced", "Condition",
                     "idnum", "question", "replaced", "Age", "female", "RBlack", "RWhite", 
                     "RHispanic", "RNative", "RAsian", "RPacific", "ROther", "Educ",
                     "Inc", "Home1", "Home2", "Spiritual", "UrbRur", "Region")
  } else {
    vars_select <- c("idnum", "question", "replaced", "Age", "female", "RBlack", "RWhite", 
                     "RHispanic", "RNative", "RAsian", "RPacific", "ROther", "Educ",
                     "Inc", "Home1", "Home2", "Spiritual", "UrbRur", "Region", "Student", "Condition")
  }
  
  d_new <- d %>% tidyr::pivot_longer(names_to = "question",
                              values_to = "replaced",
                              cols = P2C1Q1A:P2C6Q6O) %>% 
    select(all_of(vars_select)) %>% 
    mutate(condq = dplyr::case_when(str_detect(question, "C1") ~ 1,
                             str_detect(question, "C2") ~ 2,
                             str_detect(question, "C3") ~ 3,
                             str_detect(question, "C4") ~ 4,
                             str_detect(question, "C5") ~ 5,
                             str_detect(question, "C6") ~ 6),
           element_replaced = dplyr::case_when(str_detect(question, "A") ~ "actor",
                                        str_detect(question, "B") ~ "behavior",
                                        str_detect(question, "O") ~ "object"),
           q_num = dplyr::case_when(str_detect(question, "Q1") ~ 1,
                             str_detect(question, "Q2") ~ 2,
                             str_detect(question, "Q3") ~ 3,
                             str_detect(question, "Q4") ~ 4,
                             str_detect(question, "Q5") ~ 5,
                             str_detect(question, "Q6") ~ 6),
           choice_id = paste(idnum, condq, q_num, sep = "_"),
           idnum = as.factor(idnum)) %>% 
    filter(Condition == condq & !is.na(replaced)) %>% 
    mutate(Condition = as.factor(Condition))
  
  return(d_new)
}

new_data_os <- function(d, t){
  d <- replace_with_na_all(data = d, condition = ~.x %in% c("."))
  
  if(t == "MTurk"){
    vars_select <- c("MTurkCode", "EndTime", "Attention", "question", "replaced", "Condition",
                     "idnum", "question", "replaced", "Age", "female", "RBlack", "RWhite", 
                     "RHispanic", "RNative", "RAsian", "RPacific", "ROther", "Educ",
                     "Inc", "Home1", "Home2", "Spiritual", "UrbRur", "Region")
  } else {
    vars_select <- c("idnum", "question", "replaced", "Age", "female", "RBlack", "RWhite", 
                     "RHispanic", "RNative", "RAsian", "RPacific", "ROther", "Educ",
                     "Inc", "Home1", "Home2", "Spiritual", "UrbRur", "Region", "Student", "Condition")
  }
  
  question_variables <- select(d, starts_with("P2"))
  question_variables <- question_variables %>% select(P2C1Q1A:P2C6Q2O)
  q_names <- variable.names(question_variables)
  
  d_new <- d %>% tidyr::pivot_longer(names_to = "question",
                                     values_to = "replaced",
                                     cols = q_names) %>% 
    select(all_of(vars_select)) %>% 
    mutate(condq = dplyr::case_when(str_detect(question, "C1") ~ 1,
                                    str_detect(question, "C2") ~ 2,
                                    str_detect(question, "C3") ~ 3,
                                    str_detect(question, "C4") ~ 4,
                                    str_detect(question, "C5") ~ 5,
                                    str_detect(question, "C6") ~ 6),
           element_replaced = dplyr::case_when(str_detect(question, "A") ~ "actor",
                                               str_detect(question, "B") ~ "behavior",
                                               str_detect(question, "O") ~ "object"),
           q_num = dplyr::case_when(str_detect(question, "Q1") ~ 1,
                                    str_detect(question, "Q2") ~ 2,
                                    str_detect(question, "Q3") ~ 3,
                                    str_detect(question, "Q4") ~ 4,
                                    str_detect(question, "Q5") ~ 5,
                                    str_detect(question, "Q6") ~ 6),
           choice_id = paste(idnum, condq, q_num, sep = "_"),
           Condition = condq,
           Condition = as.factor(Condition),
           idnum = as.factor(idnum)) %>% 
    filter(!is.na(replaced))
  
  return(d_new)
}

clean_deflection <- function(d){
  d_new <- clean_names(d) %>% 
    mutate(Condition = condition,
           q_num = dplyr::case_when(str_detect(question, "1") ~ 1,
                             str_detect(question, "2") ~ 2,
                             str_detect(question, "3") ~ 3,
                             str_detect(question, "4") ~ 4,
                             str_detect(question, "5") ~ 5,
                             str_detect(question, "6") ~ 6)) %>% 
    mutate(def_actor = actor_deflection,
           def_behavior = behavior_deflection,
           def_object = object_deflection,
           Condition = as.factor(Condition)) %>% 
    select(Condition, question, q_num, event, overall_deflection, def_actor,
           def_behavior, def_object, abo, a_bo, ab_o) 
  
  return(d_new)
}

analysis_format <- function(d){
  d <- as.data.frame(d)
  
  varying_vars <- c(which( colnames(d)=="def_actor"), which( colnames(d)=="def_behavior"),
                    which( colnames(d)=="def_object"))
  
  d_analysis <- dfidx(d, varying = varying_vars, sep = "_",
                      choice = "element_replaced", 
                      idx = "choice_id")
  
}

run_id_model <- function(d){
  
  m <- mlogit(formula = element_replaced ~ def | overall_deflection + idnum, 
                        data = d)
  return(m)
  
  
}

run_condition_model <- function(d){
  
  m <-  mlogit(formula = element_replaced ~ def | overall_deflection + Condition, 
                  data = d)
  
  return(m)
  
}

make_plot <- function(m, d, which_model){
  #making visualization
  library(MASS)
  m_coefs <- coef(m)
  m_vcov <- vcov(m)
  
  nsim <- 10000
  
  def_series <- rep(rep(c(rep(quantile(d$overall_deflection, 0.25), 25),
                          rep(quantile(d$overall_deflection, 0.5), 25),
                          rep(quantile(d$overall_deflection, 0.75), 25),
                          rep(quantile(d$overall_deflection, 0.99), 25)), 10), 75)
  
  samples <- mvrnorm(75000, 
                     mu = m_coefs,
                     Sigma = m_vcov) %>% 
    dplyr::as_tibble() %>% 
    clean_names() %>% 
    mutate(a_def = c(rep(seq(from = 0, to = 6, by = 0.25), 1000), 
                     rep(mean(d$def), 50000)),
           b_def = c(rep(mean(d$def), 25000), 
                     rep(seq(from = 0, to = 6, by = 0.25), 1000), 
                     rep(mean(d$def), 25000)),
           o_def = c(rep(mean(d$def), 50000), 
                     rep(seq(from = 0, to = 6, by = 0.25), 1000)),
           def = rep(rep(seq(from = 0, to = 6, by = 0.25), 1000), 3),
           which_elem = c(rep("actor", 25000), rep("behavior", 25000), rep("object", 25000)),
           overall_deflection = def_series,
           idnum = 3) %>% 
    mutate(theta_actor = a_def*def,
           theta_behavior = b_def*def + overall_deflection_behavior*overall_deflection,
           theta_object = o_def*def + overall_deflection_object*overall_deflection) %>% 
    mutate(denom = exp(theta_actor) + exp(theta_behavior) + exp(theta_object),
           ev_actor = exp(theta_actor)/denom,
           ev_behavior = exp(theta_behavior)/denom,
           ev_object = exp(theta_object)/denom) %>% 
    tidyr::pivot_longer(cols = ev_actor:ev_object, names_to = "element", 
                 names_prefix = "ev_", values_to = "ev") %>% 
    group_by(which_elem, element, def, overall_deflection) %>% 
    summarise(pt = mean(ev, na.rm = TRUE),
              ub = quantile(ev, 0.975, na.rm = TRUE),
              lb = quantile(ev, 0.025, na.rm = TRUE))
  
  
  
  p <- ggplot(data = samples, mapping = aes(x = def, y = pt, color = element)) + 
    geom_point(show.legend = FALSE) + 
    geom_line(show.legend = FALSE) + 
    geom_linerange(mapping = aes(ymin = lb, ymax = ub), show.legend = FALSE) + 
    theme_minimal() + 
    facet_grid(cols = vars(which_elem), rows = vars(overall_deflection)) + 
    labs(x = "Individual Element Deflection", 
         y = "Probability of Relabeling",
         title = which_model,
         subtitle = "test")
  
  deflection_names <- c("1.76" = "25th % Event Deflection", 
                        "3.23" = "50th %", 
                        "4.31" = "75th %", 
                        "6.53" = "99th %")
  
  return(p)
}

