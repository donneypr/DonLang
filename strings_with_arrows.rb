def string_with_arrows(text, pos_start, pos_end)
  result = ''

  # Calculate indices
  idx_start = [text.rindex("\n", pos_start.idx) || 0, 0].max
  idx_end = text.index("\n", idx_start + 1) || text.length

  # Generate each line
  line_count = pos_end.ln - pos_start.ln + 1
  line_count.times do |i|
    # Calculate line columns
    line = text[idx_start...idx_end]
    col_start = (i == 0) ? pos_start.col : 0
    col_end = (i == line_count - 1) ? pos_end.col : line.length - 1

    # Append to result
    result += line + "\n"
    result += ' ' * col_start + '^' * (col_end - col_start)

    # Re-calculate indices
    idx_start = idx_end
    idx_end = text.index("\n", idx_start + 1) || text.length
  end

  result.gsub("\t", "")  
end
