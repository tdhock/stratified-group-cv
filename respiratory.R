library(geepack)
data(respiratory)
str(respiratory)
## The data are from a clinical trial of patients with respiratory
## illness, where 111 patients from two different clinics were
## randomized to receive either placebo or an active treatment.
## Patients were examined at baseline and at four visits during
## treatment. The respiratory status (categorized as 1 = good, 0 =
## poor) was determined at each visit.
group=id
target=outcome
visit
library(data.table)
data.table(respiratory)[, table(baseline, outcome)]
data.table(respiratory)[, table(id, paste(outcome, baseline))]

