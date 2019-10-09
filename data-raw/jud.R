# .loadCommonRegexes()

jud <- list(
  regexes = c(
    common.regexes$q_1,
    common.regexes$q_2,
    common.regexes$q_3,
    common.regexes$q_6,
    common.regexes$q_7,
    "^qq_10___([1-5]|97)$",
    "^q_44___[1-7]$",
    "^q_50___[1-4]$"
  ),

  options = list(
    q_1 = gbv.occur,
    q_2 = gbv.addressed.extra,
    q_3 = presenting.court,
    q_6 = presenting.issues,
    q_7 = age.range,
    qq_10 = sops.court,
    q_44 = daysoftheweek,
    q50 = ref.services.jud
  )
)
