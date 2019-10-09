# .loadCommonRegexes()

socl <- list(
  regexes = c(
    common.regexes$target,
    common.regexes$q1,
    "^q2___([1-9]|95)$",
    "^q3___([1-7]|95)$",
    "^q7___([1-3]|95)$",
    "^q8___[1-5]$",
    "^q10___[1-3]$",
    "^q12___([1-5]|95)$",
    "^q18___([1-6]|95)$",
    "^q19___[0-1]$",
    "^q27___([2-4]|95)$",
    "^q30___[1-7]$",
    "^q38___([1-6]|95)$",
    "^q40___([1-3]|95)$",
    "^q41___[0-1]$",
    "^q42___(1|3|95)$",
    "^q46a___[1-3]$"
  ),

  options = list(
    target_pop = target.populations,
    q1 = gbv.occur,
    q2 = gbv.addressed.extra,
    q3 = services.socl,
    q7 = presenting.issues,
    q8 = age.range,
    q10 = sops.socl,
    q12 = standard.docs.socl,
    q18 = social.support,
    q19 = yesno,
    q27 = edu.material,
    q30 = daysoftheweek,
    q38 = mtg.freq,
    q40 = referral.method,
    q41 = yesno,
    q42 = how.track,
    q46 = how.followup.socl
  )
)

