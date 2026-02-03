############################################################
# HackBio Introductory Assignment
# Name: Erin Joel Moore
############################################################

########################
# Task 1: GC% Function
########################

gc_percent <- function(sequence) {
  sequence <- toupper(sequence)
  bases <- strsplit(sequence, "")[[1]]

  bases <- bases[bases %in% c("A", "C", "G", "T")]

  if (length(bases) == 0) return(0)

  gc_count <- sum(bases %in% c("G", "C"))
  (gc_count / length(bases)) * 100
}

# Example tests
gc_percent("GCATTTAT")
gc_percent("gcaTTTAT")


##########################################
# Task 2: Protein Molecular Weight (kDa)
##########################################

protein_weight_kda <- function(protein = "ERINJOELMOORE") {

  aa_weights <- c(
    A = 89.09,  R = 174.20, N = 132.12, D = 133.10, C = 121.15,
    E = 147.13, Q = 146.15, G = 75.07,  H = 155.16, I = 131.18,
    L = 131.18, K = 146.19, M = 149.21, F = 165.19, P = 115.13,
    S = 105.09, T = 119.12, W = 204.23, Y = 181.19, V = 117.15
  )

  protein <- toupper(protein)
  amino_acids <- strsplit(protein, "")[[1]]

  # If any non-protein character exists, return 0
  if (any(!amino_acids %in% names(aa_weights))) {
    return(0)
  }

  total_da <- sum(aa_weights[amino_acids])
  total_kda <- total_da / 1000

  return(total_kda)
}

# Example tests
protein_weight_kda("ACDE")
protein_weight_kda("BADC")
protein_weight_kda()
