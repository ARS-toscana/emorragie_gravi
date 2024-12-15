###################################################################
# DESCRIBE THE CONCEPT SETS
###################################################################
concept_sets_of_our_study_drugs <- c("Apixaban","Rivaroxaban","OtherDOACs")

concept_sets_of_our_study_diagnosis <- c("bleeding_narrow")

# -concept_set_domains- is a 2-level list encoding for each concept set the corresponding data domain

concept_set_domains <- vector(mode="list")

concept_set_domains[["Apixaban"]] = "Medicines"
concept_set_domains[["Rivaroxaban"]] = "Medicines"
concept_set_domains[["OtherDOACs"]] = "Medicines"

concept_set_domains[["bleeding_narrow"]]="Diagnosis"
concept_set_domains[["IS"]]="Diagnosis"
concept_set_domains[["AMI"]]="Diagnosis"
concept_set_domains[["TIA"]]="Diagnosis"
concept_set_domains[["VTE"]]="Diagnosis"
concept_set_domains[["PE"]]="Diagnosis"
concept_set_domains[["DIC"]]="Diagnosis"


# -concept_set_codes_our_study- is a nested list, with 3 levels: foreach concept set, for each coding system of its data domain, the list of codes is recorded

concept_set_codes_our_study <- vector(mode="list")
concept_set_codes_our_study_excl <- vector(mode="list")

concept_set_codes_our_study[["Apixaban"]][["ATC"]] = c("B01AF02")
concept_set_codes_our_study[["Rivaroxaban"]][["ATC"]] = c("B01AF01")
concept_set_codes_our_study[["OtherDOACs"]][["ATC"]] = c("B01AE07", "B01AA", "B01AF03")


concept_set_codes_our_study[["bleeding_narrow"]][["ICD9"]] = c("455.2", "455.5", "455.8", "456.0", "456.20", "530.7", "530.8", "530.82", "531.0", "531.2", "531.4", "531.6", "532.0", "532.2", "532.4", "532.6", "533.0", "533.2", "533.4", "533.6", "534.0", "534.2", "534.4", "534.6", "535.01", "535.11", "535.21", "535.31", "535.41", "535.51", "535.61", "535.71", "537.83", "537.84", "562.02", "562.12", "562.03", "562.13", "568.81", "569.3", "569.85", "569.86", "578", "430", "431", "432.0", "432.1", "432.9", "852.2", "852.4", "336.1", "363.6", "376.32", "377.42", "379.23", "423.0", "599.70", "866.01", "866.02", "866.11", "866.12", "786.9")

concept_set_codes_our_study[["IS"]][["ICD9"]] = c("346.6", "346.61", "346.62", "346.63", "433.01", "433.11", "433.21", "433.31", "433.81", "433.91", "434", "434.01", "434.11", "434.91")

concept_set_codes_our_study[["AMI"]][["ICD9"]] = c("410", "410.0", "410.00", "410.01", "410.1", "410.10", "410.11", "410.2", "410.20", "410.21", "410.3", "410.30", "410.31", "410.4", "401.40", "410.41", "410.5", "410.50", "410.51", "410.6", "410.60", "410.61", "410.7", "410.70", "410.71", "410.8", "410.80", "410.81", "410.9", "401.90", "401.91")

concept_set_codes_our_study[["VTE"]][["ICD9"]] = c("451.1", "451.19", "451.2", "453.2", "453.4", "453.4", "453.8", "453.9")

concept_set_codes_our_study[["TIA"]][["ICD9"]] = c("435", "435.8", "435.9")

concept_set_codes_our_study[["PE"]][["ICD9"]] = c("415.1", "415.19")

concept_set_codes_our_study[["DIC"]][["ICD9"]] = c("286.6")

