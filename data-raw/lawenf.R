# .loadCommonRegexes()

lawenf <- list(
  regexes =
    c(
      common.regexes$q_1,
      "^q_2___([2-9]|10|95)$",
      "^q_5___([1-3]|95)$",
      "^q_6___([1-6])$",
      "^q_11___([1-7]|95)$",
      "^q_14___([1-5]|95)$",
      "^q25___([1-7])$",
      "^q_34___([1-3]|95)$",
      "^q_37___([1-3]|95)$",
      "^q_46___([1-4])$",
      "^q_49___(1|95)$",
      "^q50___(1|3|95)$"
    ),

  options = list(
    q_1 = gbv.occur,
    q_2 = gbv.addressed.extra,
    q_5 = presenting.issues,
    q_6 = age.range,
    q_11 = standard.docs.lawenf,
    q_14 = sops.lawenf,
    q_25 = daysoftheweek,
    q_34 = edu.material,
    q_37 = feedback.lawenf,
    q_46 = ref.services.lawenf,
    q_49 = trackinginfo.doc,
    q50 = how.track
  )
)
