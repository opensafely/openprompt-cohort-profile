from ehrql import codelist_from_csv

# A variety of plausible long covid codelists:
long_covid_nice_dx = codelist_from_csv(
    "codelists/opensafely-nice-managing-the-long-term-effects-of-covid-19.csv",
    column="code"
)
long_covid_referral = codelist_from_csv(
    "codelists/opensafely-referral-and-signposting-for-long-covid.csv",
    column="code"
)
long_covid_assessment = codelist_from_csv(
    "codelists/opensafely-assessment-instruments-and-outcome-measures-for-long-covid.csv",
    column="code"
)
long_covid_combine = (
    long_covid_nice_dx
    + long_covid_referral
    + long_covid_assessment
)

# some demographic codelists:
ethnicity = codelist_from_csv(
    "codelists/opensafely-ethnicity.csv",
    column="Code",
    category_column="Grouping_6",
)

# adminstered vaccine codes
vac_adm_1 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm1.csv",
  column="code"
)
vac_adm_2 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm2.csv",
  column="code"
)
vac_adm_3 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm3_cod.csv",
  column="code"
)
vac_adm_4 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm4_cod.csv",
  column="code"
)
vac_adm_5 = codelist_from_csv(
  "codelists/primis-covid19-vacc-uptake-covadm5_cod.csv",
  column="code"
)
vac_adm_combine = (
    vac_adm_1 +
    vac_adm_2 +
    vac_adm_3 +
    vac_adm_4 +
    vac_adm_5
)

# covid identification
hosp_covid = codelist_from_csv(
    "codelists/opensafely-covid-identification.csv",
    column="icd10_code",
)
covid_primary_care_positive_test = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-positive-test.csv",
    column="CTV3ID",
)
covid_primary_care_code = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-clinical-code.csv",
    column="CTV3ID",
)
covid_primary_care_sequalae = codelist_from_csv(
    "codelists/opensafely-covid-identification-in-primary-care-probable-covid-sequelae.csv",
    column="CTV3ID",
)
any_primary_care_code = (
    covid_primary_care_code +
    covid_primary_care_positive_test +
    covid_primary_care_sequalae
)
dementia = codelist_from_csv(
    "codelists/opensafely-dementia.csv",
    column="CTV3ID"
)
other_neuro = codelist_from_csv(
    "codelists/opensafely-other-neurological-conditions.csv",
    column="CTV3ID",
)
chronic_respiratory_disease = codelist_from_csv(
    "codelists/opensafely-chronic-respiratory-disease.csv",
    column="CTV3ID",
)
asthma = codelist_from_csv(
    "codelists/opensafely-asthma-diagnosis.csv", 
    column="CTV3ID"
)
salbutamol = codelist_from_csv(
    "codelists/opensafely-asthma-inhaler-salbutamol-medication.csv",
    column="id",
)
ics = codelist_from_csv(
    "codelists/opensafely-asthma-inhaler-steroid-medication.csv",
    column="id",
)
prednisolone = codelist_from_csv(
    "codelists/opensafely-asthma-oral-prednisolone-medication.csv",
    column="snomed_id",
)
clear_smoking = codelist_from_csv(
    "codelists/opensafely-smoking-clear.csv",
    column="CTV3Code",
    category_column="Category",
)
stroke_gp = codelist_from_csv(
    "codelists/opensafely-stroke-updated.csv",
    column="CTV3ID"
)
lung_cancer = codelist_from_csv(
    "codelists/opensafely-lung-cancer.csv",
    column="CTV3ID"
)
haem_cancer = codelist_from_csv(
    "codelists/opensafely-haematological-cancer.csv",
    column="CTV3ID"
)
other_cancer = codelist_from_csv(
    "codelists/opensafely-cancer-excluding-lung-and-haematological.csv",
    column="CTV3ID",
)
chronic_cardiac_disease = codelist_from_csv(
    "codelists/opensafely-chronic-cardiac-disease.csv",
    column="CTV3ID"
)
hiv = codelist_from_csv(
    "codelists/opensafely-hiv.csv",
    column="CTV3ID",
    category_column="CTV3ID",
)
permanent_immune = codelist_from_csv(
    "codelists/opensafely-permanent-immunosuppression.csv",
    column="CTV3ID",
)
temp_immune = codelist_from_csv(
    "codelists/opensafely-temporary-immunosuppression.csv",
    column="CTV3ID",
)
aplastic = codelist_from_csv(
    "codelists/opensafely-aplastic-anaemia.csv",
    column="CTV3ID"
)
spleen = codelist_from_csv(
    "codelists/opensafely-asplenia.csv",
    column="CTV3ID"
)
organ_transplant = codelist_from_csv(
    "codelists/opensafely-solid-organ-transplantation.csv",
    column="CTV3ID",
)
sickle_cell = codelist_from_csv(
    "codelists/opensafely-sickle-cell-disease.csv",
    column="CTV3ID"
)
ra_sle_psoriasis = codelist_from_csv(
    "codelists/opensafely-ra-sle-psoriasis.csv",
    column="CTV3ID"
)
chronic_liver_disease = codelist_from_csv(
    "codelists/opensafely-chronic-liver-disease.csv",
    column="CTV3ID"
)
diabetes = codelist_from_csv(
    "codelists/opensafely-diabetes.csv",
    column="CTV3ID"
)
psychosis_schizophrenia_bipolar = codelist_from_csv(
    "codelists/opensafely-psychosis-schizophrenia-bipolar-affective-disease.csv",
    column="CTV3Code",
)
depression = codelist_from_csv(
    "codelists/opensafely-depression.csv",
    column="CTV3Code"
)
comorbidities_codelist = (
    diabetes +
    haem_cancer +
    lung_cancer +
    other_cancer +
    asthma +
    chronic_cardiac_disease +
    chronic_liver_disease +
    chronic_respiratory_disease +
    other_neuro +
    stroke_gp +
    dementia +
    ra_sle_psoriasis +
    psychosis_schizophrenia_bipolar +
    permanent_immune +
    temp_immune
)

high_risk_shield = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-shield.csv",
    column="code"
)

low_risk_shield = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-nonshield.csv",
    column="code"
)
