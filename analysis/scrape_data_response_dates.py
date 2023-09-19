from ehrql import Dataset, case, when
from ehrql.tables.beta.tpp import open_prompt


dataset = Dataset()

# The date of the earliest response from teh `creation_date` column
index_date = (
    open_prompt
    .creation_date.minimum_for_patient()
)

# The number of days from the date of the earliest response to the date of each
# response. We expect this to be >= 0.
offset_from_index_date = (open_prompt.creation_date - index_date).days

dataset = Dataset()

dataset.define_population(open_prompt.exists_for_patient())

for day in range(0, 120):
    today_data = open_prompt.where(offset_from_index_date == day)

    record_yesno = case(
        when(today_data.exists_for_patient()).then(1),
        when(~today_data.exists_for_patient()).then(0)
    )
    
    setattr(dataset, f"day_{day}", record_yesno)