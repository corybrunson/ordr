state_center <- as.data.frame(state.center)
# US hull from the perspective of florida
fl.center <- state_center[state.abb == "FL", ]
as.data.frame(state.center) |> 
  transform(x = x - fl.center$x, y = y - fl.center$y, abbr = state.abb) |> 
  subset(abbr != "HI" & abbr != "AK") |> 
  ggplot(aes(x, y, label = abbr)) +
  stat_cone(data = \(d) subset(d, abbr != "FL")) +
  geom_text()
# US hull from the perspective of Hawai'i
hi.center <- state_center[state.abb == "HI", ]
as.data.frame(state.center) |> 
  transform(x = x - hi.center$x, y = y - hi.center$y, abbr = state.abb) |> 
  ggplot(aes(x, y, label = abbr)) +
  geom_path(stat = "cone", data = \(d) subset(d, abbr != "HI")) +
  geom_text()
