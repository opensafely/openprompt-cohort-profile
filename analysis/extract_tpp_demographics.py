# where EHRQl is defined. Only really need Dastaset, the others are specific
from ehrql import days, case, when, Dataset
# import the schema to run the study with
from ehrql.tables.beta.tpp import (
    patients,
    practice_registrations,
    clinical_events
)
import datetime

from variable_lib import (
    age_as_of,
    address_as_of
)
import codelists

dataset = Dataset()

minimum_registration = 90  # 3months of previous registration
study_start_date = datetime.date(2022, 11, 1)
end_date = datetime.date(2023, 11, 1)

# practice registration selection
registrations = practice_registrations \
    .except_where(practice_registrations.start_date + days(minimum_registration) >= end_date) \
    .except_where(practice_registrations.end_date <= study_start_date)

# count number of registrations for restriction later
registrations_number = registrations.count_for_patient()

# only keep latest reg
registration = registrations \
    .sort_by(practice_registrations.start_date).last_for_patient()

# define "start" date to use in definition of address and age
dataset.pt_start_date = case(
    when(registration.start_date + days(minimum_registration) > study_start_date).then(registration.start_date + days(minimum_registration)),
    default=study_start_date,
)

#
dataset.sex = patients.sex
dataset.age = age_as_of(study_start_date)
dataset.practice_nuts = registration.practice_nuts1_region_name
dataset.imd = address_as_of(study_start_date).imd_rounded

# Ethnicity in 6 categories ------------------------------------------------------------
dataset.ethnicity = clinical_events.where(clinical_events.ctv3_code.is_in(codelists.ethnicity)) \
    .sort_by(clinical_events.date) \
    .last_for_patient() \
    .ctv3_code.to_category(codelists.ethnicity)

population = (registrations_number == 1) & (dataset.age >= 18)
dataset.define_population(population)
dataset.configure_dummy_data(population_size = 10000)