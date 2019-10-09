# Various objects used project-wide

.makeDirList <- function() {
  suppressPackageStartupMessages(require(here))
  require(purrr, quietly = TRUE)

  first.level.dirs <- c("data", "src", "downloads", "doc", "tools")
  dList <- map(first.level.dirs, here) %>%
    stats::setNames(c("dat", "src", "dwn", "doc", "tls"))

  ql <- file.path(dList$dat, "qual")
  qt <- file.path(dList$dat, "quant")
  cod <- "coding"

  list(
    qual = ql,
    quant = qt,
    data = dList$dat,
    tools = dList$tls,
    downloads = dList$dwn,
    coding = file.path(dList$src, cod),
    clean = file.path(dList$src, "clean"),
    output = file.path(dList$doc, "output"),
    utility = file.path(dList$src, "utility"),
    transcripts = file.path(ql, "transcripts"),
    shinyApp = file.path(dList$src, "shiny", "app"),
    codebooks = file.path(dList$src, cod, "codebooks")
  )
}

raampDir <- .makeDirList()

## Study states
raampStates <- c("Akwa Ibom", "Ogun", "Bauchi") %>%
  structure(names = tolower(.))


## Study quantitative tools
tool.sectors <-
  c("Health",
    "Judicial",
    "Legal",
    "Referral",
    "Security",
    "Social",
    "Temporary") %>%
  structure(names = tolower(.), class = "Tools")

# TODO: Full Description for the Sectors


#####################################################################
## ---------- Objects for Dummy Tables and Data Wrangling ----------
#####################################################################
# Common generic responses
yesno <- c("Yes", "No")
oth <- "Other"

# services
hlth <- "Health facilities"
legl <- "Legal aid"
shlt <- "Shelter homes"
polc <- "Police"
lawenf <- "Law enforcement services"
socl <- "Social services"
econ.emp <- "Economic empowerment/ Livelihood services"


target.populations <-
  c(
    "Women",
    "Orphans and Vulnerable Children",
    "Adolescents and Youth",
    "People living with HIV",
    "Internally-displaced populations",
    "People with disabilities",
    "Key Populations"
  )

all.facilities <-
  c("Health facilities",
    "Legal aid",
    "Police",
    "Shelter homes",
    "Social services")

form.type <-
  c("Consent (GBV)",
    "Consent (HIV)",
    "Safety Assessment",
    "Client Intake")

## Types of GBV
ipv <- "Intimate Partner Violence"
rap <- "Rape"
sco <- "Sexual coercion"
csa <- "Child sexual abuse"
fgm <- "Female Genital Mutilation"
fm <- "Forced marriage"
sexhar <- "Sexual harrassment"
gbv.occur <-                          # length == 7L
  c(ipv,
    rap,
    sco,
    csa,
    fgm,
    fm,
    oth)

gbv.inner <- c("Sexual assault",
               # length == 7L
               sexhar,
               "Physical assault",
               ipv,
               csa,
               fgm,
               fm)
gbv.addressed <- c(gbv.inner, oth)   # length == 8L
gbv.addressed.extra <-
  c(gbv.inner, "Kidnapping", "Prostitution", oth)     # length == 10L

presenting.issues <-
  c("Physical injuries",
    glue::glue("{rap}/sexual assualt"),
    ipv,
    oth)

presenting.court <- c("Sexual violence",
                      sexhar,
                      ipv,
                      csa,
                      oth)
age.range <-
  c("< 5 yrs", "5-10 yrs", "11-15 yrs", "15-20 yrs", "21+ yrs")

pay.services <- yesno


serv.part1 <- c(
  "Health services",
  'Psychosocial support services',
  'Temporary Shelter/Safe Homes'
)
serv.part2 <- c(
  'Child protection services',
  econ.emp
)
services.provided <-
  c(
    serv.part1,
    glue::glue("{legl}, {lawenf}"),
    serv.part2,
    oth
  )
services.legl <- c("Legal advice",
                   "Litigation",
                   "Compensation",
                   "Settlement",
                   oth)
services.socl <- c(
  serv.part1,
  legl,
  lawenf,
  serv.part2,
  oth
)

services.refdir <- c(
  "Health",
  "Psychosocial support",
  "Shelter",
  "Legal aid",
  "Economic reintegration"
)

social.support <-
  c(
    "Supportive counselling",
    "Case management",
    "Safety planning/ homes",
    legl,
    lawenf,
    econ.emp,
    oth
  )
temp.shelter <- c(
  "Conducive temporary accommodation",
  "Counselling",
  "Safety planning",
  legl,
  lawenf,
  econ.emp,
  oth
)

sop.algo.flow <- c("Standard Operating Procedures",
                   "Algorithms",
                   "Flow charts")
qas <- "Quality assurance standards"
official.docs <-
  c("Policies",
    "Protocols",
    sop.algo.flow,
    "Job aids",
    qas)

standard.docs.core <- c(
  "Job aids",
  "National guidelines",
  glue::glue("GBV or post-{rap} care register"),
  "Referral Directory",
  "Client intake/ assessment form"
)
standard.docs.legal <-
  c(standard.docs.core,
    sop.algo.flow,
    qas,
    oth)

standard.docs.lawenf <-
  c(standard.docs.core,
    "Standard Incident Form",
    "Documenting GBV complaints",
    oth)

standard.docs.socl <- c(
  standard.docs.core,
  oth
)

available.forms <-
  c(
    "Consent form for GBV examination and care",
    "Consent form for HIV testing",
    "Safety assessment forms",
    "Client-intake/assessment form"
  )

sops.sub.sub <- c(
  "Special Provision for children friendliness",
  "Data collection, management and sharing"
)
sops.sub <- c(
  sops.sub.sub,
  "Providing referrals or coordination with other sectors and actors"
)

basic.sops <- c(sops.sub,
                oth)

sops.lawenf <- c("Investigating GBV cases",
                 "Detaining suspects",
                 basic.sops)
sops.legl <- c("Interviewing survivors",
               sops.sub)
sops.court <- c(
  "Court system to respond to GBV cases Camera (efficiently, privately, etc.)",
  "GBV evidence collection and storage",
  "Courts coordination with other GBV referral sectors",
  "Providing referrals or coordination with other sectors and actors",
  "Job aids or quality assurance standards related to responding to sexual violence or other types of GBV",
  "Don't know"
)

sops.socl <- c(
  "For working with child survivors",
  sops.sub.sub
)

sops.shlt <- c(
  "Working with GBV survivors",
  "Working with child survivors",
  "Data collection, management and sharing",
  "Providing referrals for post-GBV services"
)

infrstruct <- data.frame(
  text = c(
    "Private consultation and/or examination room",
    "Toilets",
    "Water",
    "Furniture",
    "Private room for patient to rest and recuperate",
    "Waiting room",
    "Chairs in waiting room",
    "Chair in consulting room",
    "Door/curtain or screen for visual privacy during examination",
    "Examination table",
    "Washable or disposable cover for examination couch",
    "Adequate light source (electricity) in examination room",
    "Angle lamp or torch flash light for pelvic exam",
    "Lockable cabinet for secure storage of patient files",
    "Password protected computer for electronic files",
    "Lockable medical supply cabinet or drawer for temporary storage of forensic/medico-legal evidence"
  ),
  code = c(
    "consroom",
    "toilet",
    "water",
    "furniture",
    "privroom",
    "waitroom",
    "chair.waitroom",
    "chair.consroom",
    "screen",
    "examtbl",
    "cover.couch",
    "elec",
    "light",
    "cabinet",
    "computer",
    "forensic.store"
  ),
  label = c(
    "Consulting Room",
    "Toilet",
    "Water",
    "Furniture",
    "Private Room",
    "waiting Room",
    "Waiting - Chair",
    "Consulting - Chair",
    "Screen",
    "Exam Table",
    "Couch Covers",
    "Electricity",
    "Light source",
    "Lockable Cabinet",
    "Computer",
    "Forensics Store"
  ),
  stringsAsFactors = FALSE
)


clinic.supplies <- data.frame(
  txt = c(
    "Blood pressure cuff",
    "Stethoscope",
    "Clean bed linens and gowns for each patient",
    "Sink with hand soap",
    "Resuscitation equipment",
    "Feminine hygiene supplies (e.g. sanitary pads)",
    "Waste basket with cover and disposable liner for non-bio disposable supplies/improvised",
    "Needles and syringes and sharp container with cover",
    "Sterilizer (autoclave)",
    "Sterile tray for instruments",
    "Blood tubes",
    "Sterile or clean urine containers",
    "Disposable powder-free exam gloves",
    "Speculum",
    "Tongue depressor for inspection of oral frenulum and injury",
    "Scissors",
    "Sutures",
    "Bandages",
    "Clock"
  ),
  code = c(
    "BP",
    "stet",
    "linen",
    "sink",
    "resusc",
    "fem.hyg",
    "waste",
    "sharps",
    "autoclv",
    "tray",
    "tubes",
    "uri.bott",
    "glvs",
    "spec",
    "tong",
    "sciss",
    "sut",
    "bandg",
    "clock"
  ),
  label = c(
    "BP apparatus",
    "Stethoscope",
    "Bed linen",
    "Sink with soap",
    "Rescus eqmt",
    "Fem. Hygiene items",
    "Waste basket",
    "Sharps container",
    "Autoclave",
    "Instrument tray",
    "Blood tubes",
    "Urine bottles",
    "Gloves",
    "Speculum",
    "Tongue depressor",
    "Scissors",
    "Sutures",
    "Bandage",
    "Clock"
  ),
  stringsAsFactors = FALSE
)

hivpep <- "HIV post-exposure prophylaxis"

essential.drugs <- data.frame(
  txt = c(
    "HIV test-kits",
    "Pregnancy test",
    "Emergency contraceptive/IUCD",
    hivpep,
    "Drugs for treating Sexually transmitted infections",
    "Drugs for pain relief e.g. paracetamol",
    "Local anesthetic for suturing",
    "Broad spectrum antibiotics for wound care",
    "Tetanus vaccine"
  ),
  code = c(
    "hivtst",
    "preg",
    "emerg",
    "hivpep",
    "sti",
    "analg",
    "la",
    "antibio",
    "tet"
  ),
  label = c(
    "HIV test",
    "Pregnancy test",
    "Emergency Contraception",
    "HIV-PEP",
    "STI drugs",
    "Analgesics",
    "Local anaestheics",
    "Antiiotics",
    "Tetanus toxoid"
  ),
  stringsAsFactors = FALSE
)


shlt.admin.supplies <- data.frame(
  txt = c(
    "Private room",
    "Toilet",
    "Water supply",
    "Elecricity supply",
    "Television",
    "Air conditioner/ fan",
    "Furniture",
    "Bed",
    "Mattress",
    "Pillows",
    "Beddings",
    "Security (Personnel, CCTV)",
    "Toys for children",
    "Drawing supplies",
    "Educational materials",
    "Chair",
    "Table",
    "Food items",
    "Bathroom",
    "Blanket",
    "First Aid Box",
    "Fire extinguisher"
  ),
  code = c(
    "privroom",
    "toilet",
    "water",
    "elec",
    "tv",
    "ac",
    "furn",
    "bed",
    "mattr",
    "pillw",
    "linen",
    "secur",
    "toys",
    "draw",
    "edu",
    "chair",
    "tabl",
    "food",
    "bath",
    "blanky",
    "f.aid",
    "f.exting"
  ),
  label = c(
    c(
      "Private room",
      "Toilet",
      "Water",
      "Elecricity",
      "Television",
      "A/C or fan",
      "Furniture",
      "Bed",
      "Mattress",
      "Pillows",
      "Beddings",
      "Security",
      "Toys",
      "Drawing supp",
      "Edu materials",
      "Chair",
      "Table",
      "Food",
      "Bathroom",
      "Blanket",
      "First Aid",
      "Extinguisher"
    )
  ),
  stringsAsFactors = FALSE
)

edu.material <- c(
  "Posters and/or pamphlets on what to do in case of GBV",
  "GBV laws & rights",
  "Available services",
  oth
)

postgbv.care <- c(
  "Medical history",
  "Forensic examination",
  "Treatment of acute injuries",
  hivpep,
  "Emergency contraception",
  "HIV testing, counseling, linkage to treatment",
  "STI testing & treatment",
  "Psychosocial counseling",
  "Survivor safety planning",
  "Referrals to other services as appropriate",
  "Clothes and other relevant supplies for child and adult survivors"
)


daysoftheweek <-
  c("Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday")


components.counsel <-
  c(
    "Active listening",
    "Non-judgemental counseling",
    "Validating survivor's experience",
    "Asking about safety",
    "Offering referral",
    oth
  )


phone <- "Phone call"
refforms <- "Referral forms/ cards"
feedback.lawenf <- c("Anonymous form", phone, "Email", oth)
referral.method <- c(phone, refforms, "Escort service", oth)
how.track <- c(phone, refforms, oth)
how.followup <- c(phone, "Visit", oth)
how.followup.socl <- c(phone, "Followup", "Appointment")

economic.type <-
  c("Savings & Loans", "Vocational training", "Small grants", oth)


childabuse.service <- c(
  "Identification of abuse",
  "Health services",
  "Trauma-informed counselling",
  "Educational services",
  "Child protection services",
  "Financial assistance",
  oth
)

mtg.freq <- c(
  "Weekly",
  "Bi-Weekly",
  "Monthly",
  "Bi-Monthly",
  "Quarterly",
  "Annually",
  oth
)
mthly.compile <- "Monthly Compilation & Analysis"

ref.services.hlth <-
  c(polc,
    legl,
    shlt,
    "Economic Empowerment",
    "Child Protection",
    "CBOs")

ref.services.lawenf <-
  c(hlth,
    legl,
    shlt,
    socl)

ref.services.jud <-
  c(hlth, polc, shlt, socl)

trackinginfo.doc <- c("Referral register", oth)


cat.ref.center <- paste(
  "Referral Directory/Center",
  c(
    "one",
    "Two",
    "Three",
    "Four",
    "Five",
    "Six",
    "Seven",
    "Eight",
    "Nine",
    "Ten"
  )
)
