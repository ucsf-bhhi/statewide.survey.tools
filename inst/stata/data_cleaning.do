
des *_timestamp
** RECODES (serious / to keep)
* this record isn't marked as complete for prescreen. Elana did another the same MINUTE with all the same responses, so I'm assuming it's the same person.
drop if record_id  ==  8 | record_id  ==  30

* 87 - eligible but never consented, Madison says dup with other ppt

** questions for interviewers:
* why was screener override used? Is there something we should change? Is there a reason you didn't leave notes?

// Reformat Timestamps
gen double prescreen_stamp = clock(prescreen_timestamp, "YMDhms")
gen double eligibility_stamp = clock(eligibility_timestamp, "YMDhms")
gen double consent_stamp = clock(consent_timestamp, "YMDhms")
gen double trajectory_stamp = clock(housing_trajectory_timestamp, "YMDhms")
gen double precipitants_stamp = clock(precipitants_to_home_v_0, "YMDhms")
gen double subsidy_stamp = clock(lumpsumsubsidy_preve_v_2, "YMDhms")
gen double stable_stamp = clock(stable_housing_suppl_v_4, "YMDhms")
gen double demographics_stamp = clock(demographics_timestamp, "YMDhms")
gen double history_stamp = clock(history_of_homelessn_v_6, "YMDhms")
gen double rehousing_stamp = clock(rehousing_timestamp, "YMDhms")
gen double housingservices_stamp = clock(housing_services_timestamp, "YMDhms")
gen double livsit_stamp = clock(living_situation_timestamp, "YMDhms")
gen double income_stamp = clock(income_employment_an_v_8, "YMDhms")
gen double healthcare_stamp = clock(healthcare_utilizati_v_10, "YMDhms")
gen double physicalhealth_stamp = clock(physical_health_timestamp, "YMDhms")
gen double pregnancy_stamp = clock(pregnancy_timestamp, "YMDhms")
gen double children_stamp = clock(children_timestamp, "YMDhms")
gen double carceral_stamp = clock(carceral_system_timestamp, "YMDhms")
gen double mentalhealth_stamp = clock(mental_health_3_timestamp, "YMDhms")
gen double substanceuse_stamp = clock(substance_use_2_timestamp, "YMDhms")
gen double ipv_stamp = clock(ipv_2_timestamp, "YMDhms")
gen double discrimination_stamp = clock(discrimination_timestamp, "YMDhms")
gen double endsurvey_stamp = clock(end_survey_and_rds_timestamp, "YMDhms")

// Generate time spent in each section
gen prescreen_time = (eligibility_stamp - prescreen_stamp) / 60000
gen eligibility_time = (consent_stamp - eligibility_stamp) / 60000
gen consent_time = (trajectory_stamp - consent_stamp) / 60000
gen trajectory_time = (precipitants_stamp - trajectory_stamp) / 60000
gen precipitants_time = (subsidy_stamp - precipitants_stamp) / 60000
gen subsidy_time = (stable_stamp - subsidy_stamp) / 60000
gen stable_time = (demographics_stamp - stable_stamp) / 60000
gen demographics_time = (history_stamp - demographics_stamp) / 60000
gen history_time = (rehousing_stamp - history_stamp) / 60000
gen rehousing_time = (housingservices_stamp - rehousing_stamp) / 60000
gen housingservices_time = (livsit_stamp - housingservices_stamp) / 60000
gen livsit_time = (income_stamp - livsit_stamp) / 60000
gen income_time = (healthcare_stamp - income_stamp) / 60000
gen healthcare_time = (physicalhealth_stamp - healthcare_stamp) / 60000
gen physicalhealth_time = (pregnancy_stamp - physicalhealth_stamp) / 60000
gen pregnancy_time = (children_stamp - pregnancy_stamp) / 60000
gen children_time = (carceral_stamp - children_stamp) / 60000
gen carceral_time = (mentalhealth_stamp - carceral_stamp) / 60000
gen mentalhealth_time = (substanceuse_stamp - mentalhealth_stamp) / 60000
gen substanceuse_time = (ipv_stamp - substanceuse_stamp) / 60000
gen ipv_time = (discrimination_stamp - ipv_stamp) / 60000
gen discrimination_time = (endsurvey_stamp - discrimination_stamp) / 60000
gen total_time = (endsurvey_stamp - eligibility_stamp) / 60000

// Create variable that shows date in format that can be read by a human in tables
gen survey_day = dofc(prescreen_stamp)
format survey_day %td
gen survey_day_string = string(survey_day, "%td")
replace survey_day_string = "" if survey_day_string == "."
gen survey_day2 = dofc(demographics_stamp)
format survey_day2 %td
gen survey_day2_string = string(survey_day2, "%td")
replace survey_day2_string = "" if survey_day2_string == "."

// Eligibility status
gen eligible_yn = -2
* last_night == 1, 2, 3, 4
replace eligible_yn = 1 if last_night == 1 | last_night == 2 | last_night == 3 | last_night == 4
* last_night == 5:
replace eligible_yn = 1 if hotel_alt == 0
replace eligible_yn = 0 if hotel_alt == 1 | hotel_rights == 1
* last_night == 9, 11:
replace eligible_yn = 0 if next_14 == 1
* last_night == 6, 7, 8, plus extended 9, 11:
replace eligible_yn = 1 if next_place_14 == 0 & last_night == 6
replace eligible_yn = 1 if next_place_14 == 0 & last_night == 7
replace eligible_yn = 1 if next_place_14 == 0 & last_night == 8
replace eligible_yn = 1 if hmls_past_month == 1 | hmls_past_month == 2 | hmls_past_month == 3 | hmls_past_month == 4 | hmls_past_month == 5
replace eligible_yn = 0 if next_place_14 == 1 | hmls_past_month == 0
* last_night == 10 is below
replace eligible_yn = 1 if dom_vi == 1 | next_place_15 == 0
replace eligible_yn = 0 if dom_vi == 0 | next_place_15 == 1

replace eligible_yn = 0 if (covid_1 == 1 | covid_2 == 1 | participated == 2 | age_self_report < 18)

replace eligible_yn = 2 if override_type == 1
replace eligible_yn = -1 if override_type == 2
replace eligible_yn = -3 if screening_consent == 0 & consent != 1
replace eligible_yn = -4 if language_non_resp == 1 | language_non_resp == 2
replace eligible_yn = 3 if (eligible_yn == 1 | eligible_yn == 2) & consent != 1


label values eligible_yn eligible_yn_
label define eligible_yn_ -4 "Language non-eligibility (from pre-screen)" -3 "Did not consent to eligibility screening" -2 "Did not answer enough screener questions to determine" -1 "Excluded via override" 0 "Not eligible" 1 "Eligible, consented" 2 "Eligible via override, consented" 3 "Eligible but did not consent"

// Perceived race
gen perceived_race_sum = perceived_race___1 + perceived_race___2 + perceived_race___3 + perceived_race___4 + perceived_race___5 + perceived_race___6 + perceived_race___8 + perceived_race___9
gen perceived_race_onecat = -1
replace perceived_race_onecat = 1 if perceived_race___1 == 1
replace perceived_race_onecat = 2 if perceived_race___2 == 1
replace perceived_race_onecat = 3 if perceived_race___3 == 1
replace perceived_race_onecat = 4 if perceived_race___4 == 1
replace perceived_race_onecat = 5 if perceived_race___5 == 1
replace perceived_race_onecat = 6 if perceived_race___6 == 1
replace perceived_race_onecat = 8 if perceived_race___8 == 1
replace perceived_race_onecat = 9 if perceived_race___9 == 1
replace perceived_race_onecat = 8 if perceived_race_sum > 1
replace perceived_race_onecat = . if perceived_race_sum == 0
drop perceived_race_sum
label variable perceived_race_onecat "What would you guess is the participant's race and/or ethnicity?"
label values perceived_race_onecat perceived_race_onecat_
label define perceived_race_onecat_ -1 "FIX CODING ERROR" 1 "Asian" 2 "Black, African-American, African" 3 "White, Caucasian, European American" 4 "Latinx, Hispanic, Latin American" 5 "Native American, Alaska Native"  6 "Pacific Islander, Samoan, Hawaiian" 8 "Mixed/Multiracial" 9 "Different race or ethnic ID, or unclear"


// Warning to leave last housing, single unit

gen warning_text_onecat = .
replace warning_text_onecat = warning_text / 7 if warning_time == 1
replace warning_text_onecat = warning_text if warning_time == 2
replace warning_text_onecat = warning_text * 4.34524 if warning_time == 3
replace warning_text_onecat = warning_text * 52.1786 if warning_time == 4
label variable warning_text_onecat "Length of warning to leave last housing, in weeks"

// Cost of last housing, single unit

gen housing_cost_text_onecat = .
replace housing_cost_text_onecat = housing_cost_text * 30.4167 if housing_cost_time == 1
replace housing_cost_text_onecat = housing_cost_text * 4.34524 if housing_cost_time == 2
replace housing_cost_text_onecat = housing_cost_text if housing_cost_time == 3
label variable housing_cost_text_onecat "Cost of last housing per month"


// When left stable housing [buckets]

gen date_stable_bucket = .
* asdoc list dom_moveindate, title(Mo/year moved to current home)
replace date_stable_bucket = 2 if (date_stable < td(01/01/2010) & date_stable >= td(01/01/2000))
replace date_stable_bucket = 1 if (date_stable < td(01/01/2000))
replace date_stable_bucket = 3 if (date_stable < td(01/01/2015) & date_stable >= td(01/01/2010))
replace date_stable_bucket = 4 if (date_stable < td(01/01/2016) & date_stable >= td(01/01/2015))
replace date_stable_bucket = 5 if (date_stable < td(01/01/2017) & date_stable >= td(01/01/2016))
replace date_stable_bucket = 6 if (date_stable < td(01/01/2018) & date_stable >= td(01/01/2017))
replace date_stable_bucket = 7 if (date_stable < td(01/01/2019) & date_stable >= td(01/01/2018))
replace date_stable_bucket = 8 if (date_stable < td(01/01/2020) & date_stable >= td(01/01/2019))
replace date_stable_bucket = 9 if (date_stable < td(01/01/2021) & date_stable >= td(01/01/2020))
replace date_stable_bucket = 10 if (date_stable < td(01/01/2022) & date_stable >= td(01/01/2021))

label define date_stable_bucket_ 1 "Before 2000" 2 "2000-2009" 3 "2010-2014" 4 "2015" 5 "2016" 6 "2017" 7 "2018" 8 "2019" 9 "2020" 10 "2021"
label values date_stable_bucket date_stable_bucket_
label variable date_stable_bucket "When did you leave this place?"

// Time spent in last stable housing, single unit

gen stable_time_onecat = .
replace stable_time_onecat = stable_time_months + (stable_time_years*12)
replace stable_time_onecat = stable_time_months if (stable_time_years == . & stable_time_months != .)
replace stable_time_onecat = stable_time_years * 12 if (stable_time_months == . & stable_time_years != .)
label variable stable_time_onecat "Time spent in last stable housing, in months"

label variable stable_time_years "Years in last stable housing, not including months"
label variable stable_time_months "Months in last stable housing, not including years"


// Gender

* individual cleaning
* this participant wrote in a different gender identity, but did not actually select the other box. Further, he wrote in 'gay male' and selected male in gender and gay in sexual orientation, so this is redundant. making blank
replace gender_id_other = "" if record_id == 3
* this participant filled in the other gender box, but didn't actually check 'other'
replace gender_id___6 = 1 if record_id == 41


gen gender_onecat = .
replace gender_onecat = 0 if gender_id___0 == 1
replace gender_onecat = 1 if gender_id___1 == 1
replace gender_onecat = 3 if gender_id___3 == 1
replace gender_onecat = 4 if gender_id___4 == 1
replace gender_onecat = 5 if gender_id___5 == 1
replace gender_onecat = 6 if gender_id___6 == 1
replace gender_onecat = 7 if (gender_id___0 + gender_id___1 + gender_id___3 + gender_id___4 + gender_id___5 + gender_id___6 > 1)
replace gender_onecat = 8 if gender_id____1 == 1 | gender_id____2 == 1

label values gender_onecat gender_onecat_
label define gender_onecat_ 0 "Male" 1 "Female" 3 "Trans male/man" 4 "Trans female/woman" 5 "Genderqueer/non-conforming" 6 "Other" 7 "Selected more than one of the choices" 8 "Dont' Know/Refused"


// Sexual orientation
* individual cleaning
* this participant filled in the other sexual orientation box, but didn't actually check 'other'
replace sex_orientation___6 = 1 if record_id == 41
* this participant filled in the other sexual orientation box, but wrote in "bisexual" which is an option above (which they did not check, nor did they check other)
replace sex_orient_specify = "" if record_id == 45
replace sex_orientation___3 = 1 if record_id == 45

gen sex_orientation_sum = sex_orientation___1 + sex_orientation___2 + sex_orientation___3 + sex_orientation___4 + sex_orientation___5 + sex_orientation___6 + sex_orientation___7 + sex_orientation___8

gen sex_orientation_onecat = .
replace sex_orientation_onecat = -1 if sex_orientation____1 == 1
replace sex_orientation_onecat = -2 if sex_orientation____2 == 1
replace sex_orientation_onecat = 1 if sex_orientation___1 == 1
replace sex_orientation_onecat = 2 if sex_orientation___2 == 1
replace sex_orientation_onecat = 3 if sex_orientation___3 == 1
replace sex_orientation_onecat = 4 if sex_orientation___4 == 1
replace sex_orientation_onecat = 5 if sex_orientation___5 == 1
replace sex_orientation_onecat = 6 if sex_orientation___6 == 1
replace sex_orientation_onecat = 7 if sex_orientation___7 == 1
replace sex_orientation_onecat = 8 if sex_orientation___8 == 1
replace sex_orientation_onecat = 9 if sex_orientation_sum > 1
drop sex_orientation_sum

label variable sex_orientation_onecat "What best describes your current sexual orientation? [Recode]"
label values sex_orientation_onecat sex_orientation_onecat_
label define sex_orientation_onecat_ -1 "Don't Know" -2 "Refused" 1 "Heterosexual / Straight" 2 "Gay / Lesbian" 3 "Bisexual" 4 "Asexual" 5 "Same-gender loving" 6 "Pansexual" 7 "Queer" 8 "Other" 9 "More than one of these"


// Race/Ethnicity
* individual cleaning
* this ppt wrote in italian, but didn't check other (did check white, which is where we would intend italian to fall)
replace other_race_eth = "" if record_id == 3
* this ppt selected other and wrote in apache athabaskan, though we would intend this to fall within native american
replace other_race_eth = "" if record_id == 16
replace race_identity___9 = 0 if record_id == 16
replace race_identity___5 = 1 if record_id == 16
* this ppt selected other and wrote native/african american, but didn't check either of those boxes
replace race_identity___2 = 1 if record_id == 40
replace race_identity___5 = 1 if record_id == 40
replace race_identity___9 = 0 if record_id == 40
replace other_race_eth = "" if record_id == 40
* this ppt didn't select other, but did write in "pre-siberian native". replacing with NA/Alaska Native
replace race_identity___5 = 1 if record_id == 41
replace other_race_eth = "" if record_id == 41
* this ppt didn't select other but wrote in native/african american (and didn't check either of those boxes)
replace race_identity___2 = 1 if record_id == 44
replace race_identity___5 = 1 if record_id == 44
replace other_race_eth = "" if record_id == 44
* this ppt selected white and not other, but wrote in persian & anglo
replace race_identity___9 = 1 if record_id == 86
replace other_race_eth = "Persian" if record_id == 86
* this ppt selected other and wrote in swedish, also checked white
replace race_identity___9 = 0 if record_id == 117
replace other_race_eth = "" if record_id == 117
* this ppt selected other and wrote in brazilian, this is considered latin american!
replace race_identity___4 = 1 if record_id == 129
replace race_identity___9 = 0 if record_id == 129
replace other_race_eth = "" if record_id == 129


gen race_identity_sum = race_identity___1 + race_identity___2 + race_identity___3 + race_identity___5 + race_identity___6 + race_identity___8 + race_identity___9
gen race_identity_onecat = .
replace race_identity_onecat = 1 if race_identity___1 == 1
replace race_identity_onecat = 2 if race_identity___2 == 1
replace race_identity_onecat = 3 if race_identity___3 == 1
replace race_identity_onecat = 5 if race_identity___5 == 1
replace race_identity_onecat = 6 if race_identity___6 == 1
replace race_identity_onecat = 8 if race_identity___8 == 1
replace race_identity_onecat = 9 if race_identity___9 == 1
replace race_identity_onecat = 8 if race_identity_sum > 1
replace race_identity_onecat = 4 if race_identity___4 == 1
replace race_identity_onecat = 7 if race_identity___7 == 1

label variable race_identity_onecat "Which of these race/ethnicity groups do you identify with? [Recode]"
label values race_identity_onecat race_identity_onecat_
label define race_identity_onecat_ 1 "NH Asian" 2 "NH Black, African-American, African" 3 "NH White, Caucasian, European American" 4 "Latinx, Hispanic, Latin American" 5 "NH Native American, Alaska Native"  6 "NH Pacific Islander, Samoan, Hawaiian" 7 "Indigenous from Mexico/Central/South America" 8 "NH Mixed/Multiracial" 9 "NH Other"


// Household size comparison
gen hh_size_compare = -1
replace hh_size_compare = 1 if hh_size == hh_size_6mob4
replace hh_size_compare = 2 if hh_size > hh_size_6mob4
replace hh_size_compare = 3 if hh_size < hh_size_6mob4
replace hh_size_compare = . if hh_size == . | hh_size_6mob4 == .

label variable hh_size_compare "How has household size changed since 6 months before becoming homeless? [Recode]"
label values hh_size_compare hh_size_compare_
label define hh_size_compare_ -1 "CHECK CODING" 1 "Household is same size at both timepoints" 2 "Household is larger than it was since 6 months before becoming homeless" 3 "Household is smaller than it was 6 months before becoming homeless"


// Chronic homelessness (without disability included)
gen chronic_homeless_nodisability = .
* current episode 12 months or greater: IN REGARDLESS
replace chronic_homeless_nodisability = 1 if episode_length >= 12
* current episode < 11 and first time homeless: out
replace chronic_homeless_nodisability = 0 if episode_length < 11 & history_first_episode == 1
* current episode = 11 and first time homeless: double check
* same logic for if current episode = 11 and not first time homeless, but hasn't been homeless in last 3 years:
replace chronic_homeless_nodisability = 1 if month_rounding_check == 2
replace chronic_homeless_nodisability = 0 if month_rounding_check == 1

* current episode < 11 and not first time homeless:
   * if not homeless in past 3 years: out
replace chronic_homeless_nodisability = 0 if episode_length < 11 & history_last_3years == 0
   * if homeless in past 3 years, did time add up to 1 year or more: if no, out
replace chronic_homeless_nodisability = 0 if episode_length < 11 & history_last_3years == 1 & history_12mo_total == 0
   * if homeless in past 3 years, time adds up to 1 year or more, how many occasions?: if less than 3, out
replace chronic_homeless_nodisability = 0 if episode_length < 11 & history_last_3years == 1 & history_12mo_total == 1 & history_3years == 1
   * if homeless in past 3 years, time adds up to 1 year or more, 4+ occasions: in
replace chronic_homeless_nodisability = 1 if episode_length < 11 & history_last_3years == 1 & history_12mo_total == 1 & history_3years == 2

label variable chronic_homeless_nodisability "Based on housing factors alone, is the participant considered chronically homeless?"
label values chronic_homeless_nodisability chronic_homeless_nodisability_
label define chronic_homeless_nodisability_ 0 "Not Chronically Homeless" 1 "Chronically Homeless"


// Chronic homelessness: which factor allowed them to be a yes?
gen chronic_homeless_why = .
replace chronic_homeless_why = 1 if episode_length >= 12 | month_rounding_check == 2
replace chronic_homeless_why = 2 if history_3years == 2

label variable chronic_homeless_why "Why is the participant considered chronically homeless?"
label values chronic_homeless_why chronic_homeless_why_
label define chronic_homeless_why_ 1 "Current episode of homelessness is more than 12 months in length" 2 "Total of 12 months or more in last 3 years, and 4 or more episodes"


gen chronic_homeless_1 = chronic_homeless_nodisability
replace chronic_homeless_1 = 0 if disability == 0
label variable chronic_homeless_1 "chronically homeless and condition, impairment, or disability that affects daily activities"

gen chronic_homeless_2 = chronic_homeless_1
replace chronic_homeless_2 = 1 if disability_2 == 1 & chronic_homeless_nodisability == 1
label variable chronic_homeless_2 "chronically homeless + a condition that affects daily activities or requires special device"

gen chronic_homeless_3 = chronic_homeless_2
replace chronic_homeless_3 = 1 if devices == 1 & chronic_homeless_nodisability == 1
label variable chronic_homeless_3 "chronically homeless + a condition that affects daily activities or requires special device"

gen chronic_homeless_4=chronic_homeless_3
replace chronic_homeless_4 = 1 if mh_halluc_xp_v2_v3 == 1 & chronic_homeless_nodisability == 1
label variable chronic_homeless_3 "housing + condition that affects daily activities or requires special device + hallucinations ever"

gen chronic_homeless_5 = chronic_homeless_4
replace chronic_homeless_5 = 1 if ptsd_yn == 1 & chronic_homeless_nodisability == 1
label variable chronic_homeless_3 "housing + condition that affects daily activities or requires special device + hallucinations ever + PTSD"


// Lifetime jail
gen lifetime_jail_recode = .
replace lifetime_jail_recode = lifetime_jail_dd156b
replace lifetime_jail_recode = 1 if type_housing_institution == 1 | type_stable_institution == 1

label values lifetime_jail_recode lifetime_jail_dd156b_

// Lifetime prison
gen lifetime_prison_recode = .
replace lifetime_prison_recode = prison_lifetime_703ee0
replace lifetime_prison_recode = 1 if type_housing_institution == 8 | type_stable_institution == 9

label values lifetime_prison_recode prison_lifetime_703ee0_

* this ppt was overrode into the study, but this was later found to be incorrect and they were marked for deletion.
replace screen_override = . if record_id == 79
replace override_type = . if record_id == 79
replace eligible_yn = 0 if record_id == 79

label variable prescreen_time "Pre-Screen"
label variable eligibility_time "Eligibility"
label variable consent_time "Consent"
label variable trajectory_time "Housing Trajectory"
label variable precipitants_time "Precipitants to Homelessness"
label variable subsidy_time "Lump-Sum/Subsidy Prevention"
label variable stable_time "Stable Housing Supplement"
label variable demographics_time "Demographics"
label variable history_time "History of Homelessness"
label variable rehousing_time "Rehousing"
label variable housingservices_time "Housing Services"
label variable livsit_time "Living Situation"
label variable income_time "Income, Employment, and Benefits"
label variable healthcare_time "Healthcare Utilization"
label variable physicalhealth_time "Physical Health"
label variable pregnancy_time "Pregnancy"
label variable children_time "Children"
label variable carceral_time "Carceral System"
label variable mentalhealth_time "Mental Health"
label variable substanceuse_time "Substance Use"
label variable ipv_time "Interpersonal Violence"
label variable discrimination_time "Discrimination"
label variable total_time "Total"

label define end_survey_ 1 "Survey completed, participant received gift card" 2 "Participant withdrew post-consent, received gift card" 3 "Participant withdrew post-consent, did NOT receive gift card"
label values end_survey end_survey_

label define last_night_ 1 "Outdoors, street, parks, or a tent, abandoned building, bus or train station, or any place else not meant for people to live" 2 "Vehicle (car, van, RV, etc.) (do NOT include an RV/trailer at a trailer park)" 3 "Shelter (for homeless adults, families, or people fleeing domestic violence)" 4 "Motel or hotel paid for by the government or an organization to protect you from COVID (also called a shelter in place/SIP hotel)" 5 "Motel or hotel paid for by you or a family member" 6 "City or county jail" 7 "Prison" 8 "Inpatient hospital  or mental health facility(stayed as a patient overnight)" 9 "Drug/alcohol or behavioral health treatment program" 10 "A room, apartment, or house that you rented or owned (including Permanent Supportive Housing or Transitional Housing)" 11 "A family member or friends room, apartment, or house--whether or not you paid any money to stay there"
label values last_night last_night_

label define history_first_episode_ 1 "Yes, this is the FIRST time I have EVER been homeless" 0 "No, I have been homeless another time"
label values history_first_episode history_first_episode_

replace interviewer = upper(interviewer)
replace interviewer = "TE" if interviewer == "T" & record_id == 109

label define county_ 1 "Sonoma" 2 "Placer" 3 "Fresno/Madera" 4 "Los Angeles" 5 "San Diego" 6 "Santa Clara" 7 "Sacramento" 
label values county county_
