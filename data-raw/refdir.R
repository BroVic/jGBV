refdir <- list(
  regexes = c(
    "^referral_centers_direct___([1-9]|10)$",
    "^category_refer_service_1___[1-5]$",
    "^category_referral_service_2___[1-5]$",
    "^category_referral_service_3___[1-5]$",
    "^category_referral_service_4___[1-5]$",
    "^category_referral_service_5___[1-5]$",
    "^category_referral_service_6___[1-5]$",
    "^category_referral_service_7___[1-5]$"
  ),

  options = list(
    centres.list = cat.ref.center,
    cat.ref.service1 = services.refdir,
    cat.ref.service2 = services.refdir,
    cat.ref.service3 = services.refdir,
    cat.ref.service4 = services.refdir,
    cat.ref.service5 = services.refdir,
    cat.ref.service6 = services.refdir,
    cat.ref.service7 = services.refdir
  )
)
