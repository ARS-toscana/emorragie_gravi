###################################################################
# DESCRIBE THE CONCEPT SETS
###################################################################
concept_sets_of_our_study_drugs <- c("VKA","Apixaban","Rivaroxaban","Edoxaban","Dabigatran")
concept_sets_of_our_study_diagnosis <- c("NVAF","NVAF_noexcl","Hemorrhagic_stroke","Ischemic_stroke","Deep_vein_thrombosis","Pulmonary_embolism","Gastrointestinal_Bleeding")
concept_sets_of_our_study_procedure<-c("PHK","PHK_proc")

# -concept_set_domains- is a 2-level list encoding for each concept set the corresponding data domain

concept_set_domains <- vector(mode="list")

concept_set_domains[["VKA"]]="Medicines"
concept_set_domains[["Apixaban"]]="Medicines"
concept_set_domains[["Rivaroxaban"]]="Medicines"
concept_set_domains[["Edoxaban"]]="Medicines"
concept_set_domains[["Dabigatran"]]="Medicines"
concept_set_domains[["NVAF"]]="Diagnosis"
concept_set_domains[["NVAF_noexcl"]]="Diagnosis" 
concept_set_domains[["Hemorrhagic_stroke"]]="Diagnosis"
concept_set_domains[["Ischemic_stroke"]]="Diagnosis"
concept_set_domains[["Deep_vein_thrombosis"]]="Diagnosis"
concept_set_domains[["Pulmonary_embolism"]]="Diagnosis"
concept_set_domains[["Gastrointestinal_Bleeding"]]="Diagnosis"
concept_set_domains[["PHK"]]="Diagnosis"
concept_set_domains[["PHK_proc"]]="Procedure"

# -concept_set_codes_our_study- is a nested list, with 3 levels: foreach concept set, for each coding system of its data domain, the list of codes is recorded

concept_set_codes_our_study <- vector(mode="list")
concept_set_codes_our_study_excl <- vector(mode="list")

concept_set_codes_our_study[["VKA"]][["ATC"]]= c("B01AA")
concept_set_codes_our_study[["Apixaban"]][["ATC"]]=c("B01AF02")
concept_set_codes_our_study[["Rivaroxaban"]][["ATC"]]=c("B01AF01")
concept_set_codes_our_study[["Edoxaban"]][["ATC"]]=c("B01AF03")
concept_set_codes_our_study[["Dabigatran"]][["ATC"]]=c("B01AE07")
concept_set_codes_our_study[["NVAF"]][["ICD9"]]=c("427.31","427.32")
concept_set_codes_our_study[["NVAF_noexcl"]][["ICD9"]]=c("427.31","427.32")
concept_set_codes_our_study_excl[["NVAF"]][["ICD9"]]=c("394.0", "394.2", "395.0", "395.2", "396.0", "396.1", "746.3", "746.5", "996.02", "996.71", "V35.20","V35.21","V35.22","V35.23","V35.24")
concept_set_codes_our_study[["Hemorrhagic_stroke"]][["ICD9"]]=c("430", "431", "432")
concept_set_codes_our_study_excl[["Hemorrhagic_stroke"]][["ICD9"]]=c("800","801","802","803","804", "850","851","852","853","854", "V57")
concept_set_codes_our_study[["Ischemic_stroke"]][["ICD9"]]=c("433.11","433.21","433.31","433.41","433.51","433.61","433.71","433.81","433.91", "434.11","434.21","434.31","434.41","435.51","436.61","437.71","438.81","439.91", "436", "342", "99.10")
concept_set_codes_our_study_excl[["Ischemic_stroke"]][["ICD9"]]=c("800","801","802","803","804", "850","851","852","853","854", "V57")
concept_set_codes_our_study[["Deep_vein_thrombosis"]][["ICD9"]]=c("451.1", "451.81", "452", "453.2", "453.3", "453.4", "453.8", "453.9")
concept_set_codes_our_study[["Pulmonary_embolism"]][["ICD9"]]=c("415.1", "V12.51")
concept_set_codes_our_study[["Gastrointestinal_Bleeding"]][["ICD9"]]=c("530.21", "530.7", "530.82", "531.0", "531.10", "531.2", "531.4", "531.6", "532.0", "532.2", "532.4", "532.6", "533.0", "533.10", "533.2", "533.4", "533.6", "534.0", "534.10", "534.2", "534.4", "534.6", "535.01", "535.11", "535.21", "535.31", "535.41", "535.51", "535.61", "535.71", "537.83", "562.02", "562.03", "562.12", "562.13", "569.3", "569.85", "578")
concept_set_codes_our_study[["PHK"]][["ICD9"]]=c("V43.64", "V43.65")
concept_set_codes_our_study[["PHK_proc"]][["ICD9"]]=c("81.51","81.52", "81.53","81.54", "81.55")

