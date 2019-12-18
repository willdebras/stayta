#stayta scratch

test_command <- "svy: prop bull, over(shit)"

test_sev <- "use some file

svyset [something]

svy: tab var

svy: prop var2, over(var)
"


df_stata <- as.data.frame(stringr::str_split(test_sev, "\n"))

df_pref <- stringr::str_split_fixed(df_stata[[1]], ":", n = 2)

df_pref <- cbind(df_pref[,1], stringr::str_split_fixed(df_pref[,2], ",", n = 2))
