### Description
### This script reads and recodes data from the 2018-2020 MSPA National 
### Needs-assessment of LGBTQ+ medical students in the United States. 

# Author: Timothy Keyes
# Version: 2020-03-23

# Libraries
library(tidyverse)

# Parameters
in_path <- here::here("data-raw", "mspa_na_raw.csv")
names_path <- here::here("data-raw", "school_names.csv")
out_path <- here::here("data", "mspa_na_data.rds")

var_names <- 
  c(
    "participant_id", 
    "survey_id", 
    "timestamp", 
    "consent", 
    "school_attend", 
    "med_school_year", 
    "med_school_year_other",
    "is_lgbtq", 
    "sab_is_male", 
    "sab_is_female", 
    "gender_man", 
    "gender_woman", 
    "gender_agender", 
    "gender_genderqueer",
    "gender_transman", 
    "gender_transwoman", 
    "gender_another", 
    "gender_another_description", 
    "so_asexual", 
    "so_bisexual", 
    "so_gay", 
    "so_lesbian", 
    "so_pansexual", 
    "so_queer", 
    "so_questioning", 
    "so_heterosexual", 
    "so_another", 
    "so_another_description",
    "race_native", 
    "race_asian", 
    "race_black", 
    "race_pi", 
    "race_white", 
    "race_hispanic", 
    "race_another", 
    "race_another_explanation",
    "interaction_agree", 
    "interaction_satisfaction", 
    "personal_benefit_mspa", 
    "community_benefit_mspa", 
    "enhanced_activity_mspa_lgbtq_meded", 
    "enhanced_activity_mspa_social",
    "enhanced_activity_mspa_di_training", 
    "enhanced_activity_mspa_discrim_bias_reduction",
    "enhanced_activity_mspa_mentorship",
    "enhanced_activity_mspa_advocacy", 
    "enhanced_activity_mspa_global_health",
    "enhanced_activity_mspa_other",
    "enhanced_activity_mspa_other_explanation",
    "school_affinity_group_exist", 
    "school_affinity_group_benefit", 
    "school_affinity_group_involved", 
    "why_not_involved_time", 
    "why_not_involved_value", 
    "why_not_involved_opportunities", 
    "why_not_involved_uninterested", 
    "why_not_involved_not_queer", 
    "why_not_involved_another", 
    "why_not_involved_another_explanation",
    "school_activities_advocacy",
    "school_activities_social",
    "school_activities_mentorship",
    "school_activities_educational",
    "school_activities_research",
    "school_activities_intercollegiate",
    "school_activities_other",
    "school_activities_other_explanation",
    "school_affinity_group_mission", 
    "school_affinity_group_supported", 
    "school_affinity_group_identify",
    "interest_lgbtq_meded", 
    "interest_lgbtq_social", 
    "interest_lgbtq_bias_training", 
    "interest_lgbtq_advocacy", 
    "interest_lgbtq_global_health",
    "interest_lgbtq_other",
    "importance_lgbtq_meded", 
    "importance_lgbtq_social", 
    "importance_lgbtq_bias_training", 
    "importance_lgbtq_mentorship", 
    "importance_lgbtq_advocacy", 
    "importance_lgbtq_global_health",
    "satisfaction_lgbtq_meded", 
    "satisfaction_lgbtq_social", 
    "satisfaction_bias_training", 
    "satisfaction_lgbtq_mentorship", 
    "satisfaction_lgbtq_advocacy", 
    "satisfaction_lgbtq_global_health",
    "out_classmates_peers", 
    "out_labmates_coworkers_team", 
    "out_mentors", 
    "out_medical_school_app", 
    "out_residency_app", 
    "out_other", 
    "out_other_explanation",
    "ability_out_classmates_peers", 
    "ability_out_labmates_coworkers_team", 
    "ability_out_mentors", 
    "ability_out_medical_school_app", 
    "ability_out_residency_app", 
    "ability_out_other", 
    "ability_out_other_explanation",
    "protections_out_classmates_peers", 
    "protections_out_labmates_coworkers_team", 
    "protections_out_mentors", 
    "protections_out_medical_school_app", 
    "protections_out_residency_app", 
    "protections_out_other", 
    "protections_out_other_explanation",
    "intersectionality", 
    "complete"
  )

#function and variables for recoding "yes/no" variables
recode_checked <- function(my_var) {
  recode(my_var, `0` = "no", `1` = "yes", .default = NA_character_)
}

checked_vars <- 
  vars(
    consent, 
    sab_is_male, 
    sab_is_female,
    gender_agender,
    gender_man, 
    gender_woman, 
    gender_genderqueer, 
    gender_transman, 
    gender_transwoman, 
    gender_another, 
    so_asexual,
    so_bisexual,
    so_gay,
    so_lesbian,
    so_pansexual,
    so_queer,
    so_questioning,
    so_heterosexual,
    so_another,
    race_native,
    race_asian,
    race_black,
    race_pi,
    race_white,
    race_hispanic,
    race_another,
    enhanced_activity_mspa_lgbtq_meded,          
    enhanced_activity_mspa_social,
    enhanced_activity_mspa_di_training,
    enhanced_activity_mspa_discrim_bias_reduction,
    enhanced_activity_mspa_mentorship,
    enhanced_activity_mspa_advocacy,
    enhanced_activity_mspa_global_health,
    enhanced_activity_mspa_other,
    enhanced_activity_mspa_other_explanation,
    why_not_involved_time,
    why_not_involved_value,
    why_not_involved_opportunities,
    why_not_involved_uninterested,
    why_not_involved_not_queer,
    why_not_involved_another,
    school_activities_advocacy,
    school_activities_social,
    school_activities_mentorship,
    school_activities_educational,
    school_activities_research,
    school_activities_intercollegiate,
    school_activities_other,
    importance_lgbtq_meded,
    importance_lgbtq_social,
    importance_lgbtq_bias_training,
    importance_lgbtq_mentorship,
    importance_lgbtq_advocacy,
    importance_lgbtq_global_health,
    out_classmates_peers,
    out_labmates_coworkers_team,
    out_mentors,
    out_medical_school_app,
    out_residency_app,
    out_other,
    ability_out_classmates_peers,
    ability_out_labmates_coworkers_team,
    ability_out_mentors,
    ability_out_medical_school_app,
    ability_out_residency_app,
    ability_out_other,
    protections_out_classmates_peers,
    protections_out_labmates_coworkers_team,
    protections_out_mentors,
    protections_out_medical_school_app,
    protections_out_residency_app,
    protections_out_other
  )

school_names <- 
  names_path %>% 
  read_csv() %>% 
  drop_na() %>% 
  deframe()
  

#===============================================================================

na_data <- 
  in_path %>% 
  read_csv(col_names = var_names, skip = 1) %>% 
  mutate_at(checked_vars, recode_checked) %>% 
  mutate(
    med_school_year = 
      recode(
        med_school_year, 
        `1` = "Pre-Clinical Student (prior to clerkships)",
        `2` = "Clinical Student (on clerkships)",
        `3` = "Research (PhD, Masters, or other)",
        `4` = "Other"
      ), 
    is_lgbtq = 
      recode(
        is_lgbtq, 
        `1` = "LGBTQ+",
        `2` = "Non-LGBTQ+"
      ), 
    school_affinity_group_exist = 
      recode(
        school_affinity_group_exist, 
        `1` = "yes", 
        `2` = "no"
      ),
    school_affinity_group_involved = 
      recode(
        school_affinity_group_involved, 
        `1` = "yes", 
        `2` = "no"
      ), 
    school_attend = 
      recode(
        school_attend, 
        !!! school_names, 
        .default = NA_character_
      )
  )

write_rds(x = na_data, path = out_path)
