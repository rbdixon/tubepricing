BOM = ldply( seq(2,16,2), function(ci) {
  R = bill.of.materials[, c(1, ci, ci+1)]
  colnames(R) = c("tube_assembly_id", "component", "quantity")
  return(R)
}) %>%
  tbl_df %>% 
  na.omit() %>% 
  mutate_each( funs(factor), tube_assembly_id, component)

PARTS = BOM %>% 
  group_by( tube_assembly_id ) %>% 
  summarize( num_parts = sum(quantity) )