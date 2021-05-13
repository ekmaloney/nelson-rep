library(targets)
library(dplyr)
source("R/functions.R")
options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("haven", "naniar", "janitor", "mlogit", "here", "stringr", "broom", "ggplot2", "tarchetypes"))

list(
  #FIRST: Read in Deflection Info
  tar_target(
    deflection_raw_file,
    "Data/Nelson Deflections Table.xlsx"
  ),
  tar_target(
    deflection_raw,
    readxl::read_xlsx(deflection_raw_file)
  ),
  tar_target(
    deflection,
    clean_deflection(deflection_raw)
  ),
  #SECOND - DO EVERYTHING WITH STRAIGHT REPLICATION DATA SET
  tar_target(
    raw_mturk_rep,
    "Data/Nelson_Replication - Complete - Cleaned.dta"
  ),
  tar_target(
    rep_turk,
    read_dta(raw_mturk_rep)
  ),
  tar_target(
    rep_turk_long,
    new_data_format(rep_turk, "MTurk")
  ),
  tar_target(
    rep_turk_def,
    dplyr::left_join(rep_turk_long, deflection, by = c("Condition", "q_num"))
  ),
  tar_target(
    rep_turk_analysis,
    analysis_format(rep_turk_def),
  ),
  #NOW THE OS + MS FILE? 
  tar_target(
    raw_mturk_os,
    "Data/Nelson_Replication - Oversample B&O - Cleaned.dta"
  ),
  tar_target(
    os_turk,
    read_dta(raw_mturk_os)
  ),
  tar_target(
    os_turk_long,
    new_data_os(os_turk, "MTurk")
  ),
  tar_target(
    os_turk_def,
    dplyr::left_join(os_turk_long, deflection, by = c("Condition", "q_num"))
  ),
  tar_target(
    os_turk_analysis,
    analysis_format(os_turk_def),
  ),
  #### MS FILE
  tar_target(
    raw_mturk_minus,
    "Data/nelsonms_lon.RDS"
  ),
  tar_target(
    ms_turk_long,
    readRDS(raw_mturk_minus)
  ),
  tar_target(
    ms_turk_def,
    dplyr::left_join(ms_turk_long, deflection, by = c("Condition", "q_num"))
  ),
  tar_target(
    ms_turk_analysis,
    analysis_format(ms_turk_def),
  ),
  #IN CLASS DATA
  tar_target(
    raw_inclass,
    "Data/Nelson_Replication - InClass - Complete - Cleaned.dta"
  ),
  tar_target(
    raw_ic_lab,
    read_dta(raw_inclass)
  ),
  tar_target(
    ic_lab,
    raw_ic_lab %>% 
      mutate(Educ = SchoolYear)
  ),
  tar_target(
    ic_lab_long,
    new_data_format(ic_lab, "Lab")
  ),
  tar_target(
    ic_lab_def,
    dplyr::left_join(ic_lab_long, deflection, by = c("Condition", "q_num"))
  ),
  tar_target(
    ic_lab_analysis,
    analysis_format(ic_lab_def),
  ),
  #ANALYSIS TIME
  tar_target(
    rep_turk_id_model,
    run_id_model(rep_turk_analysis)
  ),
  tar_target(
    rep_turk_cond_model,
    run_condition_model(rep_turk_analysis)
  ),
  tar_target(
    os_turk_id_model,
    run_id_model(os_turk_analysis)
  ),
  tar_target(
    os_turk_cond_model,
    run_condition_model(os_turk_analysis)
  ),
  tar_target(
    ic_id_model,
    run_id_model(ic_lab_analysis)
  ),
  tar_target(
    ic_cond_model,
    run_condition_model(ic_lab_analysis)
  ),
  #VISUALIZE TIME
  tar_target(
    rep_turk_id_plot,
    make_plot(rep_turk_id_model, rep_turk_analysis, which_model = "MTurk Direct Replication, ID")
  ),
  tar_target(
    rep_turk_cond_plot,
    make_plot(rep_turk_cond_model, rep_turk_analysis, which_model = "MTurk Direct Replication, Condition")
  ),
  tar_target(
    os_turk_id_plot,
    make_plot(os_turk_id_model, os_turk_analysis, which_model = "MTurk Over-Sample, ID")
  ),
  tar_target(
    os_turk_cond_plot,
    make_plot(rep_turk_cond_model, os_turk_analysis, which_model = "MTurk Over-Sample, Condition")
  ),
  tar_target(
    ic_id_plot,
    make_plot(ic_id_model, ic_lab_analysis, which_model = "In-Class, ID")
  ),
  tar_target(
    ic_cond_plot,
    make_plot(ic_cond_model, ic_lab_analysis, which_model = "In-Class, Condition")
  )
)







