#' Prepare Passport Data
#'
#' This function processes the passport dataframe to ensure compatibility with the CNV analysis pipeline.
#' It filters out samples not present in the by-window data, reshapes the dataframe to long format,
#' and builds per-family sample lists and generation labels.
#'
#' @param passport_df A dataframe containing passport information with columns such as ID, generation, and fam.
#' @param by_window_data A dataframe containing by-window data with a column named `Sample`.
#'
#' @return A list containing:
#'   - `fam_select`: A list of family identifiers.
#'   - `parents`: A vector of parent identifiers for each family.
#'   - `rela`: A list of generation labels for each family.
#'
#' @export
prepare_passport <- function(passport_df, by_window_data) {
    # Quality check: report any mismatches between passport and VCF sample names

    passport_df <- check_passport_columns(passport_df)

    samples <- unique(by_window_data$Sample)

    if (length(which(!passport_df$ID %in% samples)) > 0) passport_df <- passport_df[-which(!passport_df$ID %in% samples), ]

    # Reshape passport to long format so multi-family individuals are expanded
    passport_df_long <- passport_df %>%
        mutate(fam = as.character(fam)) %>%
        separate_rows(fam, sep = ",") %>%
        mutate(fam = as.integer(fam))

    # Build per-family sample lists and generation labels
    fam_list <- split(passport_df_long$ID, passport_df_long$fam)
    rela <- split(passport_df_long$generation, passport_df_long$fam)

    idx <- lapply(rela, function(x) which(x == "Parent"))

    parents <- vector()
    for (i in 1:length(fam_list)) {
        if (length(idx[[i]]) > 0) {
            parents[i] <- paste0(fam_list[[i]][idx[[i]]], collapse = " x ")
        } else {
            parents[i] <- "No parents"
        }
    }

    fam_select <- as.list(as.numeric(names(fam_list)))
    names(fam_select) <- parents

    return(list(fam_select = fam_select, parents = parents, rela = rela, fam_list = fam_list))
}

#' Check and Standardize Passport Columns
#'
#' This function validates the column names in the passport dataframe, ensuring that all required columns
#' are present. It allows for variations in column naming conventions and standardizes the column names
#' to their primary forms.
#'
#' @param passport_df A dataframe containing passport information.
#'
#' @return The passport dataframe with standardized column names.
#'
#' @examples
#' passport_df <- data.frame(Sample = c("S1", "S2"), generation = c("Parent", "Child"), Mother = c("M1", "M2"), fam = c("1", "2"))
#' standardized_df <- check_passport_columns(passport_df)
#'
#' @export
check_passport_columns <- function(passport_df) {
    # Define acceptable column variations
    column_variations <- list(
        ID = c("ID", "SampleID", "Sample"),
        generation = c("generation", "cohort"),
        Female.Parent = c("Female.Parent", "Mother", "P1"),
        Male.Parent = c("Male.Parent", "Father", "P2"),
        fam = c("fam", "familia", "group", "relatives")
    )

    # Check for at least one match for each required column
    required_columns <- setdiff(names(column_variations), c("Female.Parent", "Male.Parent"))
    missing_columns <- lapply(required_columns, function(col) {
        if (!any(column_variations[[col]] %in% colnames(passport_df))) {
            return(col)
        }
        return(NULL)
    })

    missing_columns <- unlist(missing_columns)

    if (length(missing_columns) > 0) {
        stop(paste("The following required columns are missing or have incorrect names:", paste(missing_columns, collapse = ", ")))
    }

    # Standardize column names to the primary names
    for (col in names(column_variations)) {
        match <- column_variations[[col]][column_variations[[col]] %in% colnames(passport_df)]
        if (length(match) > 0) {
            colnames(passport_df)[colnames(passport_df) == match[1]] <- col
        }
    }

    return(passport_df)
}
