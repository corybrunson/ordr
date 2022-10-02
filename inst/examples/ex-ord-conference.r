# illustrative ordination: correspendence analysis of hair & eye data
haireye_ca <- ordinate(
  as.data.frame(rowSums(HairEyeColor, dims = 2L)),
  cols = everything(), model = MASS::corresp
)
print(haireye_ca)

# check distribution of inertia
get_conference(haireye_ca)
# confer inertia to rows, then to columns
confer_inertia(haireye_ca, "rows")
confer_inertia(haireye_ca, "columns")
# confer inertia symmetrically
(haireye_ca <- confer_inertia(haireye_ca, "symmetric"))
# check redistributed inertia
get_conference(haireye_ca)
# restore default distribution of inertia
revert_conference(haireye_ca)
