method <- list(
  chromatography = list(
      list(name = "xml_chromatography_ctype", type = "select", label = "Type of Chromatograph", choices = c("LC", "GC", "none")),
      list(name = "xml_chromatography_vendor", type = "selectize", label = "Vendor of Chromatograph", choices = c("Thermo Fisher Scientific", "Agilent Technologies", "SCIEX", "Waters Corporation", "Shimadzu", "Bruker"), options = list(create = TRUE)),
      list(name = "xml_chromatography_mp1solvent", type = "selectize", label = "Mobile Phase 1 Solvent", selected = "none", choices = c("none", "water", "acetonitrile", "isopropanol", "methanol"), options = list(create = TRUE)),
      list(name = "xml_chromatography_mp1additive", type = "text", label = "Mobile Phase 1 Additive", value = "none"),
      list(name = "xml_chromatography_mp2solvent", type = "selectize", label = "Mobile Phase 2 Solvent", selected = "none", choices = c("none", "water", "acetonitrile", "isopropanol", "methanol"), options = list(create = TRUE)),
      list(name = "xml_chromatography_mp2additive", type = "text", label = "Mobile Phase 2 Additive", value = "none"),
      list(name = "xml_chromatography_mp3solvent", type = "selectize", label = "Mobile Phase 3 Solvent", selected = "none", choices = c("none", "water", "acetonitrile", "isopropanol", "methanol"), options = list(create = TRUE)),
      list(name = "xml_chromatography_mp3additive", type = "text", label = "Mobile Phase 3 Additive", value = "none"),
      list(name = "xml_chromatography_mp4solvent", type = "selectize", label = "Mobile Phase 4 Solvent", selected = "none", choices = c("none", "water", "acetonitrile", "isopropanol", "methanol"), options = list(create = TRUE)),
      list(name = "xml_chromatography_mp4additive", type = "text", label = "Mobile Phase 4 Additive", value = "none"),
      list(name = "xml_chromatography_colname", type = "text", label = "Name of Chromatography Column"),
      list(name = "xml_chromatography_colvendor", type = "selectize", label = "Name of Chromatography Column Vendor", choices = c("Thermo Fisher Scientific", "Agilent Technologies", "SCIEX", "Waters Corporation", "Shimadzu", "Bruker"), options = list(create = TRUE)),
      list(name = "xml_chromatography_colchemistry", type = "selectize", label = "Chemistry of Chromatography Column", choices = c("none", "C8", "C18", "biphenyl", "pentafluorophenyl", "diol"), options = list(create = TRUE)),
      list(name = "xml_chromatography_colid", type = "numeric", label = "Column ID (mm)", value = 2.1, min = 0),
      list(name = "xml_chromatography_collen", type = "numeric", label = "Column Length (mm)", value = 50, min = 0),
      list(name = "xml_chromatography_coldp", type = "numeric", label = "Column Particle Diameter (\u03BCm)", value = 1.8, min = 0)
  ),
  massspectrometry = list(
      list(name = "xml_massspectrometry_msvendor", type = "selectize", label = "Vendor of Mass Spectrometer", choices = c("Thermo Fisher Scientific", "Agilent Technologies", "SCIEX", "Waters Corporation", "Shimadzu", "Bruker"), options = list(create = TRUE)),
      list(name = "xml_massspectrometry_imode", type = "selectize", label = "Ionization Mode", choices = c("electrospray ionization", "atmospheric pressure chemical ionization", "atmospheric pressure photoionization")),
      list(name = "xml_massspectrometry_polarity", type = "select", label = "Ionization Polarity", choices = c("positive", "negative", "positive/negative")),
      list(name = "xml_massspectrometry_vvalue", type = "numeric", label = "Ionization Voltage/Current", value = 3000),
      list(name = "xml_massspectrometry_vunits", type = "select", label = "Voltage/Current Units", choices = c("V", "KV", "A", "\u03BCA")),
      list(name = "xml_massspectrometry_massanalyzer1", type = "selectize", label = "Mass Analyzer #1", selected = "none", choices = c("quadrupole", "none")),
      list(name = "xml_massspectrometry_massanalyzer2", type = "selectize", label = "Mass Analyzer #2", choices = c("quadrupole", "orbitrap", "time-of-flight", "linear ion trap", "FT-ICR")),
      list(name = "xml_massspectrometry_fragmode", type = "text", label = "Mass Spectrometer Fragmentation Type"),
      list(name = "xml_massspectrometry_cevalue", type = "numeric", label = "Collision Energy Setting", value = 15),
      list(name = "xml_massspectrometry_cetype", type = "text", label = "Type of Collision Energy"),
      list(name = "xml_massspectrometry_ceunits", type = "text", label = "Collision Energy Units"),
      list(name = "xml_massspectrometry_detector", type = "text", label = "Mass Spectrometer Detector")
  ),
  qc_method = list(
      list(name = "xml_qcmethod_qcused", type = "select", label = "Was a QA/QC Method Employed?", choices = c("TRUE", "FALSE")),
      list(name = "xml_qcmethod_qctype", type = "selectize", label = "Description of QC Method Employed", selected = "none", choices = c("none", "Mass Analyzer Calibration", "Internal Standard Verification", "Matrix Standard Verification", "External Standard Verification"), multiple = TRUE, options = list(create = TRUE))
  )
)

attr(method$chromatography, "title") <- "Chromatography"
attr(method$massspectrometry, "title") <- "Mass Spectrometry"
attr(method$qc_method, "title") <- "QC Method Information"