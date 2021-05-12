compound_table <- data.frame(
  COMPOUND_ID = integer(), #Key
  NAME = character(), #name of compound, uncontrolled
  INCHI = character(), #InChI structure of compound, as submitted
  INCHIKEY = character(), #InChIKey structure of compound, derived
  SMILES = character(), # SMILES structure of compound, derived
  ADDITIONAL = character(), #additional information, as submitted
  SOURCE = character(), #DOI/Link of compound structure's source
  SOURCE_TYPE = character(), #One-letter character indicating type of source
  LOCAL_POS = integer(), #Number of atoms with positive charges, derived
  LOCAL_NEG = integer(), #number of atoms with negative charges, derived
  FORMULA = character(), #elemental formula, derived
  FIXEDMASS = double(),  #exact mass of compound, derived
  NETCHARGE = integer(), #total formal charge of compound, derived
  DTXSID = character(), #DTXSID identifier
  DTXCID = character(), #DTXCID identifier
  CASRN = character(), #CAS registry number
  PUBCHEMID = character(), #PubChem identifier
  INSPECTEDBY = character() #user inspection ID
)

exp_table <- data.frame(
  EXP_ID = integer(), #Key
  MP1 = character(), #Solvent composition of Mobile Phase #1
  MP1_ADD = character(), #Buffer/Salt/Acid addition to Mobile Phase #1
  MP2 = character(), #Solvent composition of Mobile Phase #2
  MP2_ADD = character(), #Buffer/Salt/Acid addition to Mobile Phase #2
  MP3 = character(), #Solvent composition of Mobile Phase #3
  MP3_ADD = character(), #Buffer/Salt/Acid addition to Mobile Phase #3
  MP4 = character(), #Solvent composition of Mobile Phase #4
  MP4_ADD = character(), #Buffer/Salt/Acid addition to Mobile Phase #4
  IONIZATION = character(), #ionization mode (ESI, APCI, EI, etc.)
  VOLTAGE = double(), #Ionization Voltage/Current (depending on mode)
  POLARITY = character(), #Ionization Polarity (Negative, Positive, or Negative/Positive)
  CE_VALUE = character(), #value for collision energy, normally a number but can be a range
  CE_DESC = character(), #description/context of the collision energy value (normalized, stepped, range, etc.)
  MS_VENDOR = character(), #vendor of the mass spectrometer
  MS_TYPE = character(), #type of mass analyzers (QTOF, Q-ORBITRAP, etc.)
  QC_METHOD = logical(), #TRUE/FALSE: does the experiment have a QC method in place
  QC_TYPE = character(), #category of QC analysis (TBD)
  SOURCE = character() #citation for the experimental method
)

sample_table <- data.frame(
  SAMPLE_ID = integer(), #Key
  NAME = character(), #User-defined name of the sample
  CLASS = character(), #Sample class, controlled
  SOURCE = character() #citation for the sample source
)

data_table <- data.frame(
  DATA_ID = integer(), #Key
  COMPOUND_ID = integer(), #Ref to compound_table
  EXP_ID = integer(), #Ref to exp_table
  SAMPLE_ID = integer(), #ref to sample_table
  MEASURED_MZ1 = double(), #precursor ion m/z
  CHARGE = integer(), #ion charge state (-1, 1)
  RTIME = double(), #compound retention time, derived
  MS1_DATA = character(), #locator/actual MS1 isotope data
  MS2_DATA = character() #locator/actual MS2 fragmentation data
)

fragment_table <- data.frame(
  FRAGMENT_ID = integer(), #Key
  MZ = double(), #m/z value for specific fragment, derived
  FORMULA = character(), #elemental formula for specific fragment, user submitted
  CHARGE = integer(), #charge of specific fragment,derived
  RADICAL = logical(), #TRUE/FALSE: the fragment contains a radical electron, user submitted
  STRUCTURE = character(), #SMILES structure of fragment ion, can be NULL, user submitted
  SOURCE = character() #citation/source for fragment identity, USER is an option
)

fragment_locator <- data.frame(
  FRAGMENT_ID = integer(), #Ref to fragment_table
  DATA_ID = integer(), #Ref to data_table
  MZ_ERROR = double() #measured mass error from fragment_table$MZ, derived
)