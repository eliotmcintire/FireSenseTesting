
adminAbbrev <- function(adminNames){

  adminAbbrevs <- c(
    "Newfoundland"              = "NL",
    "Labrador"                  = "NL",
    "Newfoundland and Labrador" = "NL",
    "Prince Edward Island"      = "PE",
    "Nova Scotia"               = "NS",
    "New Brunswick"             = "NB",
    "Quebec"                    = "QC",
    "Ontario"                   = "ON",
    "Manitoba"                  = "MB",
    "Alberta"                   = "AB",
    "Saskatchewan"              = "SK",
    "British Columbia"          = "BC",
    "Yukon Territory"           = "YT",
    "Yukon"                     = "YT",
    "Northwest Territories"     = "NT",
    "Nunavut"                   = "NU"
  )

  data.table::data.table(
    admin_name   = names(adminAbbrevs),
    admin_abbrev = factor(adminAbbrevs)
  )[data.table::data.table(admin_name = as.character(adminNames)),
    on = "admin_name"]$admin_abbrev
}

