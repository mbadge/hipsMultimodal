# Perform tsne on raw image pixels
library(hips)

DIR_IMGS <- "/media/marcus/Vulcan/radiology/msh_hip/00_parsed_dcms/"


FPS_IMG <- list.files(DIR_IMGS, full.names = TRUE)

# Pilot
FP_IMG <- FPS_IMG[1]

npy <- readNpy(FP_IMG)
vizR::Viz(npy)

#! Issue #70
