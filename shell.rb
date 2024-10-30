require_relative 'donlang'

loop do
  print 'DonLang > '
  text = gets.chomp
  next if text.strip.empty?
  result, error = Basic.run('<stdin>', text)
  if error
    puts error.as_string
  elsif result
    puts result.to_s
  end
end
