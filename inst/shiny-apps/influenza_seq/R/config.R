# R/config.R

# Production:
DATA_ROOT <- Sys.getenv("NF_FLU_DATA_ROOT", unset = "/data/NF/NF_FLU")

BASE_URL <- Sys.getenv("NF_FLU_BASE_URL", unset = "")
if (BASE_URL == "") BASE_URL <- NULL

REFRESH_MS <- as.integer(Sys.getenv("NF_FLU_REFRESH_MS", unset = "120000")) # 2 min

