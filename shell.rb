require_relative 'donlang'

# Infinite loop for REPL-like behavior
loop do
  print 'DonLang > '
  text = gets.chomp
  
  # Call the run method from donlang class
  result, error = Basic.run('<stdin>', text)

  if error
    puts error.as_string
  end
end
