# Code to detect and omit mata code from a do or ado file

example = function() {
  # Sample do file content as a single string
  txt <- "
clear all
set more off

clear mata

* Some Stata commands
display \"Hello, Stata!\"

mata:
  // Mata code starts here
  real scalar a = 2
  real scalar b = 3
  real scalar c = a + b
  printf(\"c = %f\\n\", c)
end

* More Stata commands
display \"Back to Stata\"

mata {
  /* Another Mata block with braces */
  void function greet()
  {
    printf(\"Hello from Mata!\\n\")
  }
  greet()
}

x = x+1

mata: printf(\"Inline Mata code\\n\")

display: \"Hello\"

display \"End of do file\"
"

  # Extract Mata blocks
  pos <- locate_mata_blocks(txt)
  pos
  stringi::stri_sub(txt, pos$start, pos$end)
  txt =   txt <- "
clear all
set more off

clear mata

* Some Stata commands
display \"Hello, Stata!\"
"
  pos <- locate_mata_blocks(txt)
  pos

}

# Written with the help of ChatGPT 4.o1 preview
locate_mata_blocks <- function(txt) {
  # Load the stringi package for efficient string processing
  require(stringi)

  # Early exit: Check if 'mata' is present in the content (case-insensitive)
  if (!stri_detect_fixed(txt, 'mata', case_insensitive = TRUE)) {
    # If 'mata' is not found, return an empty data frame immediately
    return(data.frame(type = character(0), start = integer(0), end = integer(0)))
  }

  # Define regex patterns for different types of Mata blocks
  # Note: (?mi) enables multiline and case-insensitive matching
  # ^ and $ match the start and end of lines in multiline mode

  # 1. Standard Mata block start (e.g., 'mata' or 'mata:') on a line by itself
  start_pattern_standard <- "(?mi)^[ \\t]*mata[ \\t]*:?[ \\t]*(?:\\/\\/.*|\\/\\*.*?\\*\\/)?[ \\t]*$"

  # 2. Mata block with braces (e.g., 'mata {') possibly followed by code
  start_pattern_brace <- "(?mi)^[ \\t]*mata[ \\t]*\\{.*$"

  # 3. Inline Mata code (e.g., 'mata: code here') without an 'end' statement
  start_pattern_inline <- "(?mi)^[ \\t]*mata:[ \\t]*(.+)$"

  # 4. End of standard Mata block (e.g., 'end' or 'end mata') on a line by itself
  end_pattern <- "(?mi)^[ \\t]*end[ \\t]*(mata)?[ \\t]*$"

  # Initialize an empty data frame to store the results
  result <- data.frame(type = character(0), start = integer(0), end = integer(0), stringsAsFactors = FALSE)

  # Find all positions where start patterns occur
  # Using stri_locate_all_regex to get positions of matches

  # Positions of standard Mata block starts
  start_positions_standard <- stri_locate_all_regex(txt, start_pattern_standard)[[1]]
  if (!is.na(start_positions_standard[1])) {
    start_positions_standard <- data.frame(start = start_positions_standard[,1], end = start_positions_standard[,2], type = 'standard')
  } else {
    start_positions_standard <- data.frame(start = integer(0), end = integer(0), type = character(0))
  }

  # Positions of brace Mata block starts
  start_positions_brace <- stri_locate_all_regex(txt, start_pattern_brace)[[1]]
  if (!is.na(start_positions_brace[1])) {
    start_positions_brace <- data.frame(start = start_positions_brace[,1], end = start_positions_brace[,2], type = 'brace')
  } else {
    start_positions_brace <- data.frame(start = integer(0), end = integer(0), type = character(0))
  }

  # Positions of inline Mata code
  start_positions_inline <- stri_locate_all_regex(txt, start_pattern_inline)[[1]]
  if (!is.na(start_positions_inline[1])) {
    start_positions_inline <- data.frame(start = start_positions_inline[,1], end = start_positions_inline[,2], type = 'inline')
  } else {
    start_positions_inline <- data.frame(start = integer(0), end = integer(0), type = character(0))
  }

  # Combine all start positions
  start_positions <- rbind(start_positions_standard, start_positions_brace, start_positions_inline)

  # If no starts found, return empty data frame
  if (nrow(start_positions) == 0) {
    return(result)
  }

  # Sort start positions by their start index to process in order
  start_positions <- start_positions[order(start_positions$start), ]

  # Process each detected Mata block
  for (i in seq_len(nrow(start_positions))) {
    start_pos <- start_positions$start[i]
    end_pos <- start_positions$end[i]
    block_type <- start_positions$type[i]

    if (block_type == 'inline') {
      # Inline Mata code: end position is the end of the matched line
      # Record the positions as they are
      result <- rbind(result, data.frame(type = block_type, start = start_pos, end = end_pos, stringsAsFactors = FALSE))

    } else if (block_type == 'standard') {
      # Standard Mata block: find the corresponding 'end' statement

      # Extract the content after the start of the block
      remaining_content <- substr(txt, end_pos + 1, nchar(txt))

      # Find the next occurrence of the 'end' pattern
      end_match <- stri_locate_first_regex(remaining_content, end_pattern)

      if (!is.na(end_match[1])) {
        # Adjust positions relative to the original string
        end_start_pos <- end_pos + end_match[1]
        end_end_pos <- end_pos + end_match[2]
        # Record the block positions
        result <- rbind(result, data.frame(type = block_type, start = start_pos, end = end_end_pos, stringsAsFactors = FALSE))
      } else {
        # 'end' not found: include until the end of the content
        result <- rbind(result, data.frame(type = block_type, start = start_pos, end = nchar(txt), stringsAsFactors = FALSE))
      }

    } else if (block_type == 'brace') {
      # Mata block with braces: find the matching closing brace

      # Initialize brace counter with counts from the start line
      initial_content <- substr(txt, start_pos, end_pos)
      brace_count <- stri_count_fixed(initial_content, '{') - stri_count_fixed(initial_content, '}')

      # Remaining content after the start line
      remaining_content <- substr(txt, end_pos + 1, nchar(txt))

      # Find positions of all braces in the remaining content
      brace_positions <- stri_locate_all_fixed(remaining_content, c('{', '}'))[[1]]

      if (nrow(brace_positions) > 0) {
        # Create a data frame of brace positions and types
        brace_df <- data.frame(
          pos = brace_positions[,1],
          brace = stri_sub(remaining_content, from = brace_positions[,1], to = brace_positions[,1]),
          stringsAsFactors = FALSE
        )
        # Sort by position
        brace_df <- brace_df[order(brace_df$pos), ]

        # Iterate through the braces to update the brace count
        for (j in seq_len(nrow(brace_df))) {
          brace_char <- brace_df$brace[j]
          if (brace_char == '{') {
            brace_count <- brace_count + 1
          } else if (brace_char == '}') {
            brace_count <- brace_count - 1
          }
          # Check if all braces are matched
          if (brace_count == 0) {
            # Found the matching closing brace
            end_pos_brace <- end_pos + brace_df$pos[j]
            # Record the block positions
            result <- rbind(result, data.frame(type = block_type, start = start_pos, end = end_pos_brace, stringsAsFactors = FALSE))
            break
          }
        }
        if (brace_count != 0) {
          # No matching closing brace found: include until the end
          result <- rbind(result, data.frame(type = block_type, start = start_pos, end = nchar(txt), stringsAsFactors = FALSE))
        }
      } else {
        # No braces found: include until the end
        result <- rbind(result, data.frame(type = block_type, start = start_pos, end = nchar(txt), stringsAsFactors = FALSE))
      }
    }
  }

  # Return the data frame with Mata block positions and types
  return(result)
}

