COMPONENTS = ldply( c("comp.adaptor", "comp.boss", "comp.elbow", "comp.float", "comp.hfl", "comp.nut",
                      "comp.other", "comp.sleeve", "comp.straight", "comp.tee", "comp.threaded"), get) %>% 
  tbl_df

TUBE = tube %>% 
  tbl_df %>% 
  left_join(specs, by="tube_assembly_id") %>% 
  left_join(tube.end.form, by=c(end_a="end_form_id")) %>% 
  rename( forming_a = forming ) %>% 
  left_join(tube.end.form, by=c(end_x="end_form_id")) %>% 
  rename( forming_x = forming ) %>% 
  mutate_each( funs(factor), end_a, end_x)