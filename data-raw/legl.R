# .loadCommonRegexes()

legl <- list(
  regexes = c(
    common.regexes$target,
    common.regexes$q_1,
    common.regexes$q_2,
    common.regexes$q_3,
    common.regexes$q_6,
    common.regexes$q_7,
    "^q_10___[1-4]$",
    "^q_13___([1-9]|95)$",
    "^q_27___([1-3]|95)$",
    "^q_30___([1-3]|95)$",
    "^q_38___[1-7]$",
    "^q50___([1-3]|95)$",
    "^q51___[0-1]$",
    "^q_52___(1|3|95)$",
    "^q_55___([1-2]|95)$"
  ),

  options = list(
    target_pop = target.populations,
    q_1 = gbv.occur,
    q_2 = gbv.addressed.extra,
    q_3 = services.legl,
    q_6 = presenting.issues,
    q_7 = age.range,
    q_10 = sops.legl,
    q_13 = standard.docs.legal,
    q_27 = edu.material,
    q_30 = feedback.lawenf,
    q_38 = daysoftheweek,
    q50 = referral.method,
    q51 = yesno,
    q_52 = how.track,
    q_55 = how.followup
  )
)
