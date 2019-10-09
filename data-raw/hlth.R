# .loadCommonRegexes()

hlth <- list(
  regexes = c(
    common.regexes$target,
    common.regexes$q1,
    "^q3___([1-6]|95)$",
    "^q4___([1-7]|95)$",
    "^q7___([1-3]|95)$",
    "^q8___[1-5]$",
    "^q11___[1-7]$",
    "^q12a___[1-4]$",
    "^q14___([1-3]|95$)",
    "^q22___([1-3]|95)$",
    "^q23___([1-9]|10|11)$",
    "^q23c___[1-6]$",
    "^q25___[1-7]$",
    "^q34___([1-5]|95)$",
    "^q42___([1-3]|95)$",
    "^q43___[0-1]$",
    "^q_44___(1|3|95)$",
    "^q_46___([1-2]|95)$",
    "^q52___([1-3]|95)$",
    "^q56___([1-6]|95)$"
  ),

  options = list(
    target_pop = target.populations,
    q1 = gbv.occur,
    q3 = services.provided,
    q4 = gbv.addressed,
    q7 = presenting.issues,
    q8 = age.range,
    q11 = official.docs,
    q12a = available.forms,
    q14 = basic.sops,
    q22 = edu.material,
    q23 = postgbv.care,
    q23c = ref.services.hlth,
    q25 = daysoftheweek,
    q34 = components.counsel,
    q42 = referral.method,
    q43 = yesno,
    q_44 = how.track,
    q_46 = how.followup,
    q52 = economic.type,
    q56 = childabuse.service
  )
)
