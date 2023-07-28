LV1.PLS = data.frame(
  region = c("amygdala", "thalamus proper", "hippocampus", "thalamus proper"),
  XL = as.numeric(c(-0.340139721, -0.427967958, -0.977098159,-0.629750885)),
  hemi = c("right", "right", "left", "left"),
  stringsAsFactors = F)


LV1.PLS %>%
  ggseg(atlas = aseg, color = "grey37", mapping=aes(fill=as.numeric(XL)), 
        position = "stacked") + theme(legend.position = "bottom",
                                      legend.text = element_text(size=16),
                                      legend.box = "horizontal") +
  labs(fill = "XL") + theme_void() +scale_fill_gradient(low = "white",high ="red", na.value = "light grey", trans = "reverse", limits=c(0,-4.71))


LV2.PLS = data.frame(
  region = c("thalamus proper"),
  XL = as.numeric(c(0.609643566)),
  hemi = c("left"),
  stringsAsFactors = F)


LV2.PLS %>%
  ggseg(atlas = aseg, color = "grey37", mapping=aes(fill=as.numeric(XL)), 
        position = "stacked") + theme(legend.position = "bottom",
                                      legend.text = element_text(size=16),
                                      legend.box = "horizontal") +
  labs(fill = "XL") + theme_void() +scale_fill_gradient(low = "white",high ="blue", na.value = "light grey", limits=c(0,4.48))


LV3.PLS = data.frame(
  region = c("thalamus proper","thalamus proper"),
  XL = as.numeric(c(-0.925070569,-0.33342869)),
  hemi = c("right","left"),
  stringsAsFactors = F)


LV3.PLS %>%
  ggseg(atlas = aseg, color = "grey37", mapping=aes(fill=as.numeric(XL)), 
        position = "stacked") + theme(legend.position = "bottom",
                                      legend.text = element_text(size=16),
                                      legend.box = "horizontal") +
  labs(fill = "XL") + theme_void() +scale_fill_gradient(low = "white",high ="red", na.value = "light grey", trans = "reverse", limits=c(0,-3.57))

