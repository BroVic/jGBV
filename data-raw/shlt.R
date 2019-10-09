# .loadCommonRegexes()

shlt <- list(
  regexes = c(
    common.regexes$target,
    common.regexes$q_1,
    common.regexes$q_2,
    "^q_3___([1-7]|95)$",
    "^q_6___([1-3]|95)$",
    common.regexes$q_7,
    "^q_10___[2-5]$",
    "^q_14___([1-5]|95)$",
    "^q_20___([1-6]|95)$",
    "^q_34___([1-9]|95)$",
    "^q_40___([2-4]|95)$",
    "^q_42___[1-7]$",
    "^q_45___([1-3]|95)$",
    "^q54a___([1-3]|95)$",
    "^q_55___(1|3|95)$",
    "^q_57___([1-2]|95)$",
    "^q_64___([1-6]|95)$",
    "^q_67___[1-4]$"
  ),

  options = list(
    target_pop = target.populations,
    q_1 = gbv.occur,
    q_2 = gbv.addressed.extra,
    q_3 = services.socl,
    q_6 = presenting.issues,
    q_7 = age.range,
    q_10 = sops.shlt,
    q_14 = standard.docs.socl,
    q_20 = temp.shelter,
    q_34 = gbv.addressed.extra,
    q_40 = edu.material,
    q_42 = daysoftheweek,
    q_45 = feedback.lawenf,
    q54a = referral.method,
    q_55 = how.track,
    q_57 = how.followup,
    q_64 = childabuse.service,
    q_67 = economic.type
  )
)
