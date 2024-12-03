###################################################################
# DESCRIBE THE CONCEPT SETS
###################################################################
concept_sets_of_our_study_drugs <- c("Apixaban","Rivaroxaban","OtherDOACs")

concept_sets_of_our_study_diagnosis <- c("bleeding_narrow", "bleeding_possible")

# -concept_set_domains- is a 2-level list encoding for each concept set the corresponding data domain

concept_set_domains <- vector(mode="list")

concept_set_domains[["Apixaban"]] = "Medicines"
concept_set_domains[["Rivaroxaban"]] = "Medicines"
concept_set_domains[["OtherDOACs"]] = "Medicines"

concept_set_domains[["bleeding_narrow"]]="Diagnosis"
concept_set_domains[["bleeding_possible"]]="Diagnosis" 


# -concept_set_codes_our_study- is a nested list, with 3 levels: foreach concept set, for each coding system of its data domain, the list of codes is recorded

concept_set_codes_our_study <- vector(mode="list")
concept_set_codes_our_study_excl <- vector(mode="list")

concept_set_codes_our_study[["Apixaban"]][["ATC"]] = c("B01AF02")
concept_set_codes_our_study[["Rivaroxaban"]][["ATC"]] = c("B01AF01")
concept_set_codes_our_study[["OtherDOACs"]][["ATC"]] = c("B01AE07", "B01AA", "B01AF03")


concept_set_codes_our_study[["bleeding_narrow"]][["ICD9"]] = c("430", "431", "432.0", "432.1", "432.9", "852.0", "852.2", "852.4", "853.0", "423.0")
concept_set_codes_our_study[["bleeding_possible"]][["ICD9"]] = c("455.2", "455.5", "455.8", "456.0", "456.20", "530.7", "530.82", "531.0", "531.2", "531.4", "531.6", "532.0", "532.2", "532.4", "532.6", "533.0", "533.2", "533.4", "533.6", "534.0", "534.2", "534.4", "534.6", "535.11", "535.21", "535.31", "535.41", "535.51", "535.61", "535.71", "537.83", "537.84", "562.02", "562.12", "562.03", "562.13", "568.81", "569.85", "569.86", "430", "431", "432.0", "432.1", "432.9", "852.0", "852.2", "852.4", "853.0", "336.1", "363.6", "372.72", "376.32", "377.42", "379.23", "423.0", "719.1", "729.92", "866.01", "866.02", "866.11", "866.12", "423.0")
concept_set_codes_our_study_excl[["bleeding_possible"]][["ICD9"]] <- concept_set_codes_our_study[["bleeding_narrow"]][["ICD9"]]
