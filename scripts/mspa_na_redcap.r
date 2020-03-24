#Clear existing data and graphics
rm(list=ls())
graphics.off()
#Load Hmisc library
library(Hmisc)
#Read Data
data=read.csv('SexualAndGenderMinor_DATA_2020-02-29_1353.csv')
#Setting Labels

label(data$record_id)="Study ID"
label(data$redcap_survey_identifier)="Survey Identifier"
label(data$demographics_timestamp)="Survey Timestamp"
label(data$consent___1)="By checking I consent, I certify that I have read the above information, that all of my questions about the study have been answered to my satisfaction and that I agree to participate in this study. (choice=I consent)"
label(data$school_attend)="1. What school do you attend?"
label(data$yr_medschool)="2. What year of medical school are you in?"
label(data$other_yr_medschool)="2a. Please explain what year in medical school you are in:"
label(data$lgbtq_minority)="3. Do you identify as lesbian, gay, bisexual, transgender, queer (LGBTQ), or another sexual and/or gender minority? "
label(data$sex_assigned_birth___1)="4. What was your sex assigned at birth on your original birth certificate? (choice=Male)"
label(data$sex_assigned_birth___2)="4. What was your sex assigned at birth on your original birth certificate? (choice=Female)"
label(data$gender_id___1)="5. What is your current gender identity? (These options are not mutually exclusive; check all that apply.) (choice=Man)"
label(data$gender_id___2)="5. What is your current gender identity? (These options are not mutually exclusive; check all that apply.) (choice=Woman)"
label(data$gender_id___3)="5. What is your current gender identity? (These options are not mutually exclusive; check all that apply.) (choice=Agender)"
label(data$gender_id___4)="5. What is your current gender identity? (These options are not mutually exclusive; check all that apply.) (choice=Genderqueer or Gender Non-conforming)"
label(data$gender_id___5)="5. What is your current gender identity? (These options are not mutually exclusive; check all that apply.) (choice=Transgender Man)"
label(data$gender_id___6)="5. What is your current gender identity? (These options are not mutually exclusive; check all that apply.) (choice=Transgender Woman)"
label(data$gender_id___7)="5. What is your current gender identity? (These options are not mutually exclusive; check all that apply.) (choice=Another Gender Identity)"
label(data$other_gender_id)="5a. Please describe your gender identity"
label(data$sexual_ornt___1)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Asexual)"
label(data$sexual_ornt___2)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Bisexual)"
label(data$sexual_ornt___3)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Gay)"
label(data$sexual_ornt___4)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Lesbian)"
label(data$sexual_ornt___5)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Pansexual)"
label(data$sexual_ornt___6)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Queer)"
label(data$sexual_ornt___7)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Questioning)"
label(data$sexual_ornt___8)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Straight/heterosexual)"
label(data$sexual_ornt___9)="6. What is your current sexual orientation? (These options are not mutually exclusive; check all that apply.) (choice=Another Sexual Orientation)"
label(data$other_sexual_ornt)="6a. How would you describe your current sexual orientation:"
label(data$race_id___1)="7. What is your Race/Ethnicity? (Check all that apply.) (choice=American Indian or Alaska Native)"
label(data$race_id___2)="7. What is your Race/Ethnicity? (Check all that apply.) (choice=Asian)"
label(data$race_id___3)="7. What is your Race/Ethnicity? (Check all that apply.) (choice=Black or African American)"
label(data$race_id___4)="7. What is your Race/Ethnicity? (Check all that apply.) (choice=Native Hawaiian or Other Pacific Islander)"
label(data$race_id___5)="7. What is your Race/Ethnicity? (Check all that apply.) (choice=White)"
label(data$race_id___6)="7. What is your Race/Ethnicity? (Check all that apply.) (choice=Latino or Hispanic)"
label(data$race_id___7)="7. What is your Race/Ethnicity? (Check all that apply.) (choice=Other)"
label(data$other_race_id)="7a. Please describe your race/ethnicity: "
label(data$in_other_schools)="8. The LGBTQ+ community at my medical school regularly interacts (formally or informally) with the LGBTQ+ community at other medical institutions."
label(data$in_sat_home)="9. I am satisfied with the degree to which the LGBTQ+ community at my medical school interacts and/or collaborates with the LGBTQ+ community at other medical institutions."
label(data$in_benefit_me)="10. I would personally benefit from an intercollegiate SGM medical student organization."
label(data$in_benefit_school)="11. The LGBTQ+ community at my medical school would benefit from an intercollegiate SGM medical student organization."
label(data$mspa_activities___1)="12. Which of your institutions activities (if any) do you think would be enhanced by a national LGBTQ+ medical student organization? Check all that apply. (choice=LGBTQ+ health education or curriculum development)"
label(data$mspa_activities___2)="12. Which of your institutions activities (if any) do you think would be enhanced by a national LGBTQ+ medical student organization? Check all that apply. (choice=LGBTQ+ social events)"
label(data$mspa_activities___3)="12. Which of your institutions activities (if any) do you think would be enhanced by a national LGBTQ+ medical student organization? Check all that apply. (choice=Diversity and inclusion training among healthcare providers)"
label(data$mspa_activities___4)="12. Which of your institutions activities (if any) do you think would be enhanced by a national LGBTQ+ medical student organization? Check all that apply. (choice=Discrimination and bias reduction training)"
label(data$mspa_activities___5)="12. Which of your institutions activities (if any) do you think would be enhanced by a national LGBTQ+ medical student organization? Check all that apply. (choice=Physician mentorship for LGBTQ+ students)"
label(data$mspa_activities___6)="12. Which of your institutions activities (if any) do you think would be enhanced by a national LGBTQ+ medical student organization? Check all that apply. (choice=Political and/or social advocacy for sexual and gender minorities)"
label(data$mspa_activities___7)="12. Which of your institutions activities (if any) do you think would be enhanced by a national LGBTQ+ medical student organization? Check all that apply. (choice=LGBTQ+ Global Health)"
label(data$mspa_activities___8)="12. Which of your institutions activities (if any) do you think would be enhanced by a national LGBTQ+ medical student organization? Check all that apply. (choice=Other)"
label(data$other_mspa_activities)="12a. What other activities would be enhanced by a national LGBTQ+ medical student organization? "
label(data$presence_lgbt_org)="13. Does your school have an LGBTQ+ or SGM medical student association of some kind?"
label(data$benefit_mspa)="14. Do you feel that an LGBTQ+ or SGM medical student association is something you would benefit from?"
label(data$involve_lgbt_home)="15. Are you involved in some way?"
label(data$not_lgbt_org___1)="16. Why not? Mark all that apply. (choice=I dont have enough time in my schedule.)"
label(data$not_lgbt_org___2)="16. Why not? Mark all that apply. (choice=I dont see the value of joining.)"
label(data$not_lgbt_org___3)="16. Why not? Mark all that apply. (choice=There arent many opportunities to get involved.)"
label(data$not_lgbt_org___4)="16. Why not? Mark all that apply. (choice=Im not interested in current group events and/or services.)"
label(data$not_lgbt_org___5)="16. Why not? Mark all that apply. (choice=I do not identify as LGBTQ+ or as a sexual and/or gender minority.)"
label(data$not_lgbt_org___6)="16. Why not? Mark all that apply. (choice=Another reason)"
label(data$other_no_lgbt_org)="16a. Please describe any other reasons:"
label(data$lgbt_org_activities___1)="17. In which of the following activities does your schools LGBTQ+ student group engage in? (choice=Social, political, or institutional advocacy work)"
label(data$lgbt_org_activities___2)="17. In which of the following activities does your schools LGBTQ+ student group engage in? (choice=Social outings (eg. dinner, parties))"
label(data$lgbt_org_activities___3)="17. In which of the following activities does your schools LGBTQ+ student group engage in? (choice=Professional mentorship)"
label(data$lgbt_org_activities___4)="17. In which of the following activities does your schools LGBTQ+ student group engage in? (choice=Educational activities (eg. workshops, panels))"
label(data$lgbt_org_activities___5)="17. In which of the following activities does your schools LGBTQ+ student group engage in? (choice=Research)"
label(data$lgbt_org_activities___6)="17. In which of the following activities does your schools LGBTQ+ student group engage in? (choice=Intercollegiate engagement)"
label(data$lgbt_org_activities___7)="17. In which of the following activities does your schools LGBTQ+ student group engage in? (choice=Other)"
label(data$other_lgbt_org_activities)="17a. What other student group activities do you engage in?"
label(data$lgbt_org_mission)="18. I feel the LGBTQ+ medical student group at my school has a clear mission."
label(data$lgbt_org_support)="19. I feel supported by the LGBTQ+ medical student group at my school."
label(data$lgbt_org_visibility)="20. I easily identify with members of my schools LGBTQ+ medical student group"
label(data$mspa_edu)="21. LGBTQ+ health education and curriculum development"
label(data$mspa_social)="22. LGBTQ+ social events"
label(data$mspa_bias_train)="23. Implicit bias and LGBTQ+ anti-discrimination training"
label(data$mspa_advocacy)="24. Social, political, and/or institutional LGBTQ+ advocacy"
label(data$mspa_global_health)="25. Global LGBTQ+ Health"
label(data$other_mspa_interests)="26. Are there areas related to LGBTQ+ medicine that interest you and are not listed above? If no, please leave blank."
label(data$pro_dev_import___1)="27. Which of the following would you consider important (however you define importance) in your professional development as a future physician? (choice=LGBTQ+ Health education and curriculum development)"
label(data$pro_dev_import___2)="27. Which of the following would you consider important (however you define importance) in your professional development as a future physician? (choice=LGBTQ+ Social events)"
label(data$pro_dev_import___3)="27. Which of the following would you consider important (however you define importance) in your professional development as a future physician? (choice=Discrimination training)"
label(data$pro_dev_import___4)="27. Which of the following would you consider important (however you define importance) in your professional development as a future physician? (choice=Physician mentorship)"
label(data$pro_dev_import___5)="27. Which of the following would you consider important (however you define importance) in your professional development as a future physician? (choice=Advocacy)"
label(data$pro_dev_import___6)="27. Which of the following would you consider important (however you define importance) in your professional development as a future physician? (choice=LGBTQ+ Global Health)"
label(data$sat_edu)="LGBTQ+ Health Education"
label(data$sat_social)="LGBTQ+ Social events"
label(data$sat_discrim)="Discrimination training"
label(data$sat_mentor)="Physician mentorship"
label(data$sat_advocacy)="Advocacy"
label(data$sat_global)="LGBTQ+ Global Health"
label(data$out_app___1)="29. In medical school, in which contexts are you currently out, have ever been out, or plan to be out with regard to your sexual orientation/gender identity? Check all that apply.  (choice=To my classmates/peers)"
label(data$out_app___2)="29. In medical school, in which contexts are you currently out, have ever been out, or plan to be out with regard to your sexual orientation/gender identity? Check all that apply.  (choice=To my lab-mates/co-workers/team members)"
label(data$out_app___3)="29. In medical school, in which contexts are you currently out, have ever been out, or plan to be out with regard to your sexual orientation/gender identity? Check all that apply.  (choice=To my mentors (research or clinical))"
label(data$out_app___4)="29. In medical school, in which contexts are you currently out, have ever been out, or plan to be out with regard to your sexual orientation/gender identity? Check all that apply.  (choice=On my application to medical school)"
label(data$out_app___5)="29. In medical school, in which contexts are you currently out, have ever been out, or plan to be out with regard to your sexual orientation/gender identity? Check all that apply.  (choice=On my application to residency/post-doctoral studies)"
label(data$out_app___6)="29. In medical school, in which contexts are you currently out, have ever been out, or plan to be out with regard to your sexual orientation/gender identity? Check all that apply.  (choice=Other)"
label(data$other_out_app)="29a. Which other contexts have you been out or planned to be out with regard to your sexual orientation/gender identity? "
label(data$out_trainees___1)="30. In medical school, in which contexts do you feel trainees should be able to be out with regard to their sexual orientation/gender identity? Check all that apply.  (choice=To their classmates/peers)"
label(data$out_trainees___2)="30. In medical school, in which contexts do you feel trainees should be able to be out with regard to their sexual orientation/gender identity? Check all that apply.  (choice=To their lab-mates/co-workers/team members)"
label(data$out_trainees___3)="30. In medical school, in which contexts do you feel trainees should be able to be out with regard to their sexual orientation/gender identity? Check all that apply.  (choice=To their mentors (research or clinical))"
label(data$out_trainees___4)="30. In medical school, in which contexts do you feel trainees should be able to be out with regard to their sexual orientation/gender identity? Check all that apply.  (choice=On their applications to medical school)"
label(data$out_trainees___5)="30. In medical school, in which contexts do you feel trainees should be able to be out with regard to their sexual orientation/gender identity? Check all that apply.  (choice=On their applications to residency/post-doctoral studies)"
label(data$out_trainees___6)="30. In medical school, in which contexts do you feel trainees should be able to be out with regard to their sexual orientation/gender identity? Check all that apply.  (choice=Other)"
label(data$other_out_trainees)="30a. What other contexts do you think trainees should be able to be out with regard to their sexual orientation/gender identity?"
label(data$out_protections___1)="31. In which contexts do you feel medical schools should have protections in place so that trainees can be out with regard to their sexual orientation/gender identity, if they so choose? Check all that apply.  (choice=The classroom/teaching environment (with peers/classmates and educators))"
label(data$out_protections___2)="31. In which contexts do you feel medical schools should have protections in place so that trainees can be out with regard to their sexual orientation/gender identity, if they so choose? Check all that apply.  (choice=The research environment (with lab-mates/coworkers))"
label(data$out_protections___3)="31. In which contexts do you feel medical schools should have protections in place so that trainees can be out with regard to their sexual orientation/gender identity, if they so choose? Check all that apply.  (choice=Clinical rotations/the wards (with team members/clinical mentors))"
label(data$out_protections___4)="31. In which contexts do you feel medical schools should have protections in place so that trainees can be out with regard to their sexual orientation/gender identity, if they so choose? Check all that apply.  (choice=On applications to medical school)"
label(data$out_protections___5)="31. In which contexts do you feel medical schools should have protections in place so that trainees can be out with regard to their sexual orientation/gender identity, if they so choose? Check all that apply.  (choice=On residency applications)"
label(data$out_protections___6)="31. In which contexts do you feel medical schools should have protections in place so that trainees can be out with regard to their sexual orientation/gender identity, if they so choose? Check all that apply.  (choice=Other)"
label(data$other_out_protections)="31a. In which other contexts do you feel medical schools should have protections in place so that trainees can be out with regard to their sexual orientation/gender identity, if they so choose?"
label(data$final_ques_viewpoints)="32. The LGBTQ+ community represents a wide array of identities and viewpoints, particularly with regard to issues of intersectionality. In what ways would an intercollegiate medical student organization best represent all of these viewpoints in an equitable way, especially regarding queer and trans* people of color?"
label(data$demographics_complete)="Complete?"
#Setting Units


#Setting Factors(will create new variable for factors)
data$consent___1.factor = factor(data$consent___1,levels=c("0","1"))
data$yr_medschool.factor = factor(data$yr_medschool,levels=c("1","2","3","4"))
data$lgbtq_minority.factor = factor(data$lgbtq_minority,levels=c("1","2"))
data$sex_assigned_birth___1.factor = factor(data$sex_assigned_birth___1,levels=c("0","1"))
data$sex_assigned_birth___2.factor = factor(data$sex_assigned_birth___2,levels=c("0","1"))
data$gender_id___1.factor = factor(data$gender_id___1,levels=c("0","1"))
data$gender_id___2.factor = factor(data$gender_id___2,levels=c("0","1"))
data$gender_id___3.factor = factor(data$gender_id___3,levels=c("0","1"))
data$gender_id___4.factor = factor(data$gender_id___4,levels=c("0","1"))
data$gender_id___5.factor = factor(data$gender_id___5,levels=c("0","1"))
data$gender_id___6.factor = factor(data$gender_id___6,levels=c("0","1"))
data$gender_id___7.factor = factor(data$gender_id___7,levels=c("0","1"))
data$sexual_ornt___1.factor = factor(data$sexual_ornt___1,levels=c("0","1"))
data$sexual_ornt___2.factor = factor(data$sexual_ornt___2,levels=c("0","1"))
data$sexual_ornt___3.factor = factor(data$sexual_ornt___3,levels=c("0","1"))
data$sexual_ornt___4.factor = factor(data$sexual_ornt___4,levels=c("0","1"))
data$sexual_ornt___5.factor = factor(data$sexual_ornt___5,levels=c("0","1"))
data$sexual_ornt___6.factor = factor(data$sexual_ornt___6,levels=c("0","1"))
data$sexual_ornt___7.factor = factor(data$sexual_ornt___7,levels=c("0","1"))
data$sexual_ornt___8.factor = factor(data$sexual_ornt___8,levels=c("0","1"))
data$sexual_ornt___9.factor = factor(data$sexual_ornt___9,levels=c("0","1"))
data$race_id___1.factor = factor(data$race_id___1,levels=c("0","1"))
data$race_id___2.factor = factor(data$race_id___2,levels=c("0","1"))
data$race_id___3.factor = factor(data$race_id___3,levels=c("0","1"))
data$race_id___4.factor = factor(data$race_id___4,levels=c("0","1"))
data$race_id___5.factor = factor(data$race_id___5,levels=c("0","1"))
data$race_id___6.factor = factor(data$race_id___6,levels=c("0","1"))
data$race_id___7.factor = factor(data$race_id___7,levels=c("0","1"))
data$in_other_schools.factor = factor(data$in_other_schools,levels=c("1","2","3","4","5"))
data$in_sat_home.factor = factor(data$in_sat_home,levels=c("1","2","3","4","5"))
data$in_benefit_me.factor = factor(data$in_benefit_me,levels=c("1","2","3","4","5"))
data$in_benefit_school.factor = factor(data$in_benefit_school,levels=c("1","2","3","4","5"))
data$mspa_activities___1.factor = factor(data$mspa_activities___1,levels=c("0","1"))
data$mspa_activities___2.factor = factor(data$mspa_activities___2,levels=c("0","1"))
data$mspa_activities___3.factor = factor(data$mspa_activities___3,levels=c("0","1"))
data$mspa_activities___4.factor = factor(data$mspa_activities___4,levels=c("0","1"))
data$mspa_activities___5.factor = factor(data$mspa_activities___5,levels=c("0","1"))
data$mspa_activities___6.factor = factor(data$mspa_activities___6,levels=c("0","1"))
data$mspa_activities___7.factor = factor(data$mspa_activities___7,levels=c("0","1"))
data$mspa_activities___8.factor = factor(data$mspa_activities___8,levels=c("0","1"))
data$presence_lgbt_org.factor = factor(data$presence_lgbt_org,levels=c("1","2"))
data$benefit_mspa.factor = factor(data$benefit_mspa,levels=c("1","2","3"))
data$involve_lgbt_home.factor = factor(data$involve_lgbt_home,levels=c("1","2"))
data$not_lgbt_org___1.factor = factor(data$not_lgbt_org___1,levels=c("0","1"))
data$not_lgbt_org___2.factor = factor(data$not_lgbt_org___2,levels=c("0","1"))
data$not_lgbt_org___3.factor = factor(data$not_lgbt_org___3,levels=c("0","1"))
data$not_lgbt_org___4.factor = factor(data$not_lgbt_org___4,levels=c("0","1"))
data$not_lgbt_org___5.factor = factor(data$not_lgbt_org___5,levels=c("0","1"))
data$not_lgbt_org___6.factor = factor(data$not_lgbt_org___6,levels=c("0","1"))
data$lgbt_org_activities___1.factor = factor(data$lgbt_org_activities___1,levels=c("0","1"))
data$lgbt_org_activities___2.factor = factor(data$lgbt_org_activities___2,levels=c("0","1"))
data$lgbt_org_activities___3.factor = factor(data$lgbt_org_activities___3,levels=c("0","1"))
data$lgbt_org_activities___4.factor = factor(data$lgbt_org_activities___4,levels=c("0","1"))
data$lgbt_org_activities___5.factor = factor(data$lgbt_org_activities___5,levels=c("0","1"))
data$lgbt_org_activities___6.factor = factor(data$lgbt_org_activities___6,levels=c("0","1"))
data$lgbt_org_activities___7.factor = factor(data$lgbt_org_activities___7,levels=c("0","1"))
data$lgbt_org_mission.factor = factor(data$lgbt_org_mission,levels=c("1","2","3","4","5"))
data$lgbt_org_support.factor = factor(data$lgbt_org_support,levels=c("1","2","3","4","5"))
data$lgbt_org_visibility.factor = factor(data$lgbt_org_visibility,levels=c("1","2","3","4","5"))
data$mspa_edu.factor = factor(data$mspa_edu,levels=c("1","2","3","4","5"))
data$mspa_social.factor = factor(data$mspa_social,levels=c("1","2","3","4","5"))
data$mspa_bias_train.factor = factor(data$mspa_bias_train,levels=c("1","2","3","4","5"))
data$mspa_advocacy.factor = factor(data$mspa_advocacy,levels=c("1","2","3","4","5"))
data$mspa_global_health.factor = factor(data$mspa_global_health,levels=c("1","2","3","4","5"))
data$pro_dev_import___1.factor = factor(data$pro_dev_import___1,levels=c("0","1"))
data$pro_dev_import___2.factor = factor(data$pro_dev_import___2,levels=c("0","1"))
data$pro_dev_import___3.factor = factor(data$pro_dev_import___3,levels=c("0","1"))
data$pro_dev_import___4.factor = factor(data$pro_dev_import___4,levels=c("0","1"))
data$pro_dev_import___5.factor = factor(data$pro_dev_import___5,levels=c("0","1"))
data$pro_dev_import___6.factor = factor(data$pro_dev_import___6,levels=c("0","1"))
data$sat_edu.factor = factor(data$sat_edu,levels=c("1","2","3","4","5"))
data$sat_social.factor = factor(data$sat_social,levels=c("1","2","3","4","5"))
data$sat_discrim.factor = factor(data$sat_discrim,levels=c("1","2","3","4","5"))
data$sat_mentor.factor = factor(data$sat_mentor,levels=c("1","2","3","4","5"))
data$sat_advocacy.factor = factor(data$sat_advocacy,levels=c("1","2","3","4","5"))
data$sat_global.factor = factor(data$sat_global,levels=c("1","2","3","4","5"))
data$out_app___1.factor = factor(data$out_app___1,levels=c("0","1"))
data$out_app___2.factor = factor(data$out_app___2,levels=c("0","1"))
data$out_app___3.factor = factor(data$out_app___3,levels=c("0","1"))
data$out_app___4.factor = factor(data$out_app___4,levels=c("0","1"))
data$out_app___5.factor = factor(data$out_app___5,levels=c("0","1"))
data$out_app___6.factor = factor(data$out_app___6,levels=c("0","1"))
data$out_trainees___1.factor = factor(data$out_trainees___1,levels=c("0","1"))
data$out_trainees___2.factor = factor(data$out_trainees___2,levels=c("0","1"))
data$out_trainees___3.factor = factor(data$out_trainees___3,levels=c("0","1"))
data$out_trainees___4.factor = factor(data$out_trainees___4,levels=c("0","1"))
data$out_trainees___5.factor = factor(data$out_trainees___5,levels=c("0","1"))
data$out_trainees___6.factor = factor(data$out_trainees___6,levels=c("0","1"))
data$out_protections___1.factor = factor(data$out_protections___1,levels=c("0","1"))
data$out_protections___2.factor = factor(data$out_protections___2,levels=c("0","1"))
data$out_protections___3.factor = factor(data$out_protections___3,levels=c("0","1"))
data$out_protections___4.factor = factor(data$out_protections___4,levels=c("0","1"))
data$out_protections___5.factor = factor(data$out_protections___5,levels=c("0","1"))
data$out_protections___6.factor = factor(data$out_protections___6,levels=c("0","1"))
data$demographics_complete.factor = factor(data$demographics_complete,levels=c("0","1","2"))

levels(data$consent___1.factor)=c("Unchecked","Checked")
levels(data$yr_medschool.factor)=c("Pre-Clinical Student (prior to clerkships)","Clinical Student (on clerkships)","Research (PhD, Masters, or other)","Other")
levels(data$lgbtq_minority.factor)=c("Yes","No")
levels(data$sex_assigned_birth___1.factor)=c("Unchecked","Checked")
levels(data$sex_assigned_birth___2.factor)=c("Unchecked","Checked")
levels(data$gender_id___1.factor)=c("Unchecked","Checked")
levels(data$gender_id___2.factor)=c("Unchecked","Checked")
levels(data$gender_id___3.factor)=c("Unchecked","Checked")
levels(data$gender_id___4.factor)=c("Unchecked","Checked")
levels(data$gender_id___5.factor)=c("Unchecked","Checked")
levels(data$gender_id___6.factor)=c("Unchecked","Checked")
levels(data$gender_id___7.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___1.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___2.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___3.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___4.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___5.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___6.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___7.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___8.factor)=c("Unchecked","Checked")
levels(data$sexual_ornt___9.factor)=c("Unchecked","Checked")
levels(data$race_id___1.factor)=c("Unchecked","Checked")
levels(data$race_id___2.factor)=c("Unchecked","Checked")
levels(data$race_id___3.factor)=c("Unchecked","Checked")
levels(data$race_id___4.factor)=c("Unchecked","Checked")
levels(data$race_id___5.factor)=c("Unchecked","Checked")
levels(data$race_id___6.factor)=c("Unchecked","Checked")
levels(data$race_id___7.factor)=c("Unchecked","Checked")
levels(data$in_other_schools.factor)=c("Strongly disagree (1)","Somewhat disagree (2)","Neither agree nor disagree (3)","Somewhat agree (4)","Strongly agree (5)")
levels(data$in_sat_home.factor)=c("Strongly disagree (1)","Somewhat disagree (2)","Neither agree nor disagree (3)","Somewhat agree (4)","Strongly agree (5)")
levels(data$in_benefit_me.factor)=c("Strongly disagree (1)","Somewhat disagree (2)","Neither agree nor disagree (3)","Somewhat agree (4)","Strongly agree (5)")
levels(data$in_benefit_school.factor)=c("Strongly disagree (1)","Somewhat disagree (2)","Neither agree nor disagree (3)","Somewhat agree (4)","Strongly agree (5)")
levels(data$mspa_activities___1.factor)=c("Unchecked","Checked")
levels(data$mspa_activities___2.factor)=c("Unchecked","Checked")
levels(data$mspa_activities___3.factor)=c("Unchecked","Checked")
levels(data$mspa_activities___4.factor)=c("Unchecked","Checked")
levels(data$mspa_activities___5.factor)=c("Unchecked","Checked")
levels(data$mspa_activities___6.factor)=c("Unchecked","Checked")
levels(data$mspa_activities___7.factor)=c("Unchecked","Checked")
levels(data$mspa_activities___8.factor)=c("Unchecked","Checked")
levels(data$presence_lgbt_org.factor)=c("Yes","No")
levels(data$benefit_mspa.factor)=c("Yes","No","Im not sure")
levels(data$involve_lgbt_home.factor)=c("Yes","No")
levels(data$not_lgbt_org___1.factor)=c("Unchecked","Checked")
levels(data$not_lgbt_org___2.factor)=c("Unchecked","Checked")
levels(data$not_lgbt_org___3.factor)=c("Unchecked","Checked")
levels(data$not_lgbt_org___4.factor)=c("Unchecked","Checked")
levels(data$not_lgbt_org___5.factor)=c("Unchecked","Checked")
levels(data$not_lgbt_org___6.factor)=c("Unchecked","Checked")
levels(data$lgbt_org_activities___1.factor)=c("Unchecked","Checked")
levels(data$lgbt_org_activities___2.factor)=c("Unchecked","Checked")
levels(data$lgbt_org_activities___3.factor)=c("Unchecked","Checked")
levels(data$lgbt_org_activities___4.factor)=c("Unchecked","Checked")
levels(data$lgbt_org_activities___5.factor)=c("Unchecked","Checked")
levels(data$lgbt_org_activities___6.factor)=c("Unchecked","Checked")
levels(data$lgbt_org_activities___7.factor)=c("Unchecked","Checked")
levels(data$lgbt_org_mission.factor)=c("Strongly disagree (1)","Somewhat disagree (2)","Neither agree nor disagree (3)","Somewhat agree (4)","Strongly agree (5)")
levels(data$lgbt_org_support.factor)=c("Strongly disagree (1)","Somewhat disagree (2)","Neither agree nor disagree (3)","Somewhat agree (4)","Strongly agree (5)")
levels(data$lgbt_org_visibility.factor)=c("Strongly disagree (1)","Somewhat disagree (2)","Neither agree nor disagree (3)","Somewhat agree (4)","Strongly agree (5)")
levels(data$mspa_edu.factor)=c("Not at all interested (1)","Less interested (2)","Undecided (3)","Somewhat interested (4)","Very interested (5)")
levels(data$mspa_social.factor)=c("Not at all interested (1)","Less interested (2)","Undecided (3)","Somewhat interested (4)","Very interested (5)")
levels(data$mspa_bias_train.factor)=c("Not at all interested (1)","Less interested (2)","Undecided (3)","Somewhat interested (4)","Very interested (5)")
levels(data$mspa_advocacy.factor)=c("Not at all interested (1)","Less interested (2)","Undecided (3)","Somewhat interested (4)","Very interested (5)")
levels(data$mspa_global_health.factor)=c("Not at all interested (1)","Less interested (2)","Undecided (3)","Somewhat interested (4)","Very interested (5)")
levels(data$pro_dev_import___1.factor)=c("Unchecked","Checked")
levels(data$pro_dev_import___2.factor)=c("Unchecked","Checked")
levels(data$pro_dev_import___3.factor)=c("Unchecked","Checked")
levels(data$pro_dev_import___4.factor)=c("Unchecked","Checked")
levels(data$pro_dev_import___5.factor)=c("Unchecked","Checked")
levels(data$pro_dev_import___6.factor)=c("Unchecked","Checked")
levels(data$sat_edu.factor)=c("1 (very dissatisfied)","2","3","4","5 (exceptionally satisfied)")
levels(data$sat_social.factor)=c("1 (very dissatisfied)","2","3","4","5 (exceptionally satisfied)")
levels(data$sat_discrim.factor)=c("1 (very dissatisfied)","2","3","4","5 (exceptionally satisfied)")
levels(data$sat_mentor.factor)=c("1 (very dissatisfied)","2","3","4","5 (exceptionally satisfied)")
levels(data$sat_advocacy.factor)=c("1 (very dissatisfied)","2","3","4","5 (exceptionally satisfied)")
levels(data$sat_global.factor)=c("1 (very dissatisfied)","2","3","4","5 (exceptionally satisfied)")
levels(data$out_app___1.factor)=c("Unchecked","Checked")
levels(data$out_app___2.factor)=c("Unchecked","Checked")
levels(data$out_app___3.factor)=c("Unchecked","Checked")
levels(data$out_app___4.factor)=c("Unchecked","Checked")
levels(data$out_app___5.factor)=c("Unchecked","Checked")
levels(data$out_app___6.factor)=c("Unchecked","Checked")
levels(data$out_trainees___1.factor)=c("Unchecked","Checked")
levels(data$out_trainees___2.factor)=c("Unchecked","Checked")
levels(data$out_trainees___3.factor)=c("Unchecked","Checked")
levels(data$out_trainees___4.factor)=c("Unchecked","Checked")
levels(data$out_trainees___5.factor)=c("Unchecked","Checked")
levels(data$out_trainees___6.factor)=c("Unchecked","Checked")
levels(data$out_protections___1.factor)=c("Unchecked","Checked")
levels(data$out_protections___2.factor)=c("Unchecked","Checked")
levels(data$out_protections___3.factor)=c("Unchecked","Checked")
levels(data$out_protections___4.factor)=c("Unchecked","Checked")
levels(data$out_protections___5.factor)=c("Unchecked","Checked")
levels(data$out_protections___6.factor)=c("Unchecked","Checked")
levels(data$demographics_complete.factor)=c("Incomplete","Unverified","Complete")
