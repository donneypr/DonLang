require_relative 'donlang'

# Infinite loop for REPL-like behavior
loop do
  print 'basic > '
  text = gets.chomp
  
  # Call the run method from donlang class
  result, error = Basic.run('<stdin>', text)

  if error
    puts error.as_string
  else
    puts result
  end
end