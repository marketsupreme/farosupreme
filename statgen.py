import rpy2
import rpy2.robjects as robj

fetch_stats = "./fetchstats.R"
robj.r.source(fetch_stats)
