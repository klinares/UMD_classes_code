library(readxl)
library(tidyverse)

dat <- read_xls(
  "~/repos/UMD_classes_code/applied_sampling_SURV625/project/MI_school_frame_head_counts.xls")

# we will use Region, County_ID, and tot_all

# region counts
dat |> 
  group_by(Region) |> 
  tally(tot_all) |> 
  mutate(prop = n/sum(n))


# what is the proportionate allocation plan of that sample 
## of students across these nine strata?

