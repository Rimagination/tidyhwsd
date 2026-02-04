# Utilities for aggregating HWSD2 component rows to SMU x LAYER summaries.

na_of_same_type <- function(x) {
  if (is.factor(x)) {
    return(factor(NA, levels = levels(x)))
  }
  if (is.integer(x)) {
    return(NA_integer_)
  }
  if (is.numeric(x)) {
    return(NA_real_)
  }
  if (is.character(x)) {
    return(NA_character_)
  }
  if (is.logical(x)) {
    return(NA)
  }
  NA
}

weighted_mode <- function(x, w) {
  if (length(x) == 0) {
    return(NA)
  }
  ok <- !(is.na(x) | is.na(w) | w <= 0)
  if (!any(ok)) {
    return(na_of_same_type(x))
  }
  x_ok <- x[ok]
  w_ok <- w[ok]
  keys <- as.character(x_ok)
  wsum <- tapply(w_ok, keys, sum)
  mode_key <- names(wsum)[which.max(wsum)]
  x_ok[match(mode_key, keys)]
}

dominant_row_index <- function(df_group, dominant_by = "seq1_then_share") {
  n <- nrow(df_group)
  if (n == 0) {
    return(NA_integer_)
  }

  seq_vals <- if ("SEQUENCE" %in% names(df_group)) df_group$SEQUENCE else rep(NA, n)
  share_vals <- if ("SHARE" %in% names(df_group)) df_group$SHARE else rep(NA, n)
  share_vals[share_vals < 0] <- NA

  if (dominant_by == "seq1") {
    idx <- which(seq_vals == 1)
    if (length(idx) > 0) {
      return(idx[1])
    }
    if (all(is.na(seq_vals))) {
      return(1)
    }
    return(which.min(seq_vals))
  }

  if (dominant_by == "seq1_then_share") {
    idx_seq1 <- which(seq_vals == 1)
    if (length(idx_seq1) > 0) {
      share_seq1 <- share_vals[idx_seq1]
      if (all(is.na(share_seq1))) {
        return(idx_seq1[1])
      }
      max_share <- max(share_seq1, na.rm = TRUE)
      idx <- idx_seq1[share_seq1 == max_share]
      return(idx[1])
    }
  }

  if (all(is.na(share_vals))) {
    if (dominant_by == "share_then_seq" && !all(is.na(seq_vals))) {
      return(which.min(seq_vals))
    }
    return(1)
  }

  max_share <- max(share_vals, na.rm = TRUE)
  idx <- which(share_vals == max_share)
  if (dominant_by == "share_then_seq" && length(idx) > 1 && !all(is.na(seq_vals))) {
    seq_sub <- seq_vals[idx]
    if (all(is.na(seq_sub))) {
      return(idx[1])
    }
    return(idx[which.min(seq_sub)])
  }

  idx[1]
}

collapse_group_dominant <- function(df_group, dominant_by = "seq1_then_share") {
  if (nrow(df_group) == 0) {
    return(df_group)
  }
  dom_idx <- dominant_row_index(df_group, dominant_by)
  dom_row <- df_group[dom_idx, , drop = FALSE]

  for (col in names(dom_row)) {
    if (is.numeric(dom_row[[col]])) {
      val <- dom_row[[col]]
      if (!is.na(val) && val < 0) {
        dom_row[[col]] <- NA
      }
    }
  }

  tibble::as_tibble(dom_row)
}

collapse_group_spec <- function(
  df_group,
  agg_spec,
  dominant_by = "seq1_then_share",
  normalize_share = TRUE,
  share_tol = 1
) {
  if (nrow(df_group) == 0) {
    return(df_group)
  }

  if (is.null(agg_spec) || length(agg_spec) == 0) {
    return(collapse_group_dominant(df_group, dominant_by = dominant_by))
  }

  w <- if ("SHARE" %in% names(df_group)) df_group$SHARE else rep(1, nrow(df_group))
  w[w < 0] <- NA
  share_sum <- sum(w, na.rm = TRUE)
  w_use <- w
  if (normalize_share && is.finite(share_sum) && share_sum > 0) {
    if (abs(share_sum - 100) > share_tol) {
      w_use <- w / share_sum
    }
  }
  w_use[is.na(w_use)] <- 0

  dom_idx <- dominant_row_index(df_group, dominant_by)
  dom_row <- df_group[dom_idx, , drop = FALSE]

  out <- list()

  for (col in names(agg_spec)) {
    method <- agg_spec[[col]]
    if (is.na(method) || method == "drop") {
      next
    }
    x <- df_group[[col]]
    dom_val <- dom_row[[col]]

    if (method == "dominant") {
      if (is.numeric(dom_val) && dom_val < 0) {
        dom_val <- NA
      }
      out[[col]] <- dom_val
      next
    }

    if (method == "weighted_mean") {
      if (!is.numeric(x)) {
        stop(sprintf("weighted_mean is only valid for numeric fields: %s", col))
      }
      x_num <- x
      x_num[x_num < 0] <- NA
      val <- stats::weighted.mean(x_num, w_use, na.rm = TRUE)
      if (!is.finite(val)) {
        val <- dom_val
      }
      if (is.numeric(val) && val < 0) {
        val <- NA
      }
      out[[col]] <- val
      next
    }

    if (method == "weighted_mode") {
      if (is.numeric(x)) {
        x[x < 0] <- NA
      }
      out[[col]] <- weighted_mode(x, w_use)
      next
    }

    stop(sprintf("Unknown aggregation method for %s: %s", col, method))
  }

  tibble::as_tibble(out)
}
