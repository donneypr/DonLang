require_relative 'donlang'

# Infinite loop for REPL-like behavior
loop do
  print 'DonLang > '
  text = gets.chomp
  
  # Skip if the input is empty
  next if text.strip.empty?

  # Call the run method from the Basic class
  result, error = Basic.run('<stdin>', text)

  if error
    puts error.as_string
  elsif result
    # Print the result if no error occurred and there's a result
    puts result.to_s
  end
end
