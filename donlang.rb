#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'

#######################################
# ERRORS
#######################################

class Error
  def initialize(pos_start, pos_end, error_name, details)
    @pos_start = pos_start
    @pos_end = pos_end
    @error_name = error_name
    @details = details
  end

  def as_string
    "#{@error_name}: #{@details}\nFile #{@pos_start.fn}, line #{@pos_start.ln + 1}"
  end
end

class IllegalCharError < Error
  def initialize(pos_start, pos_end, details)
    super(pos_start, pos_end, 'Illegal Character', details)
  end
end


# POSITION


class Position
  attr_reader :idx, :ln, :col, :fn

  def initialize(idx, ln, col, fn, ftxt)
    @idx = idx
    @ln = ln
    @col = col
    @fn = fn
    @ftxt = ftxt
  end

  def advance(current_char)
    @idx += 1
    @col += 1

    if current_char == "\n"
      @ln += 1
      @col = 0
    end

    self
  end

  def copy
    Position.new(@idx, @ln, @col, @fn, @ftxt)
  end
end

#######################################
# TOKENS
#######################################

TT_INT    = 'INT'
TT_FLOAT  = 'FLOAT'
TT_PLUS   = 'PLUS'
TT_MINUS  = 'MINUS'
TT_MUL    = 'MUL'
TT_DIV    = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'

class Token
  def initialize(type, value = nil)
    @type = type
    @value = value
  end

  def to_s
    @value ? "#{@type}:#{@value}" : "#{@type}"
  end
end

#######################################
# LEXER
#######################################

class Lexer
  def initialize(fn, text)
    @fn = fn
    @text = text
    @pos = Position.new(-1, 0, -1, fn, text)
    @current_char = nil
    advance
  end

  def advance
    @pos.advance(@current_char)
    @current_char = @pos.idx < @text.length ? @text[@pos.idx] : nil
  end

  def make_tokens
    tokens = []

    while @current_char
      if " \t".include?(@current_char)
        advance
      elsif DIGITS.include?(@current_char)
        tokens << make_number
      elsif @current_char == '+'
        tokens << Token.new(TT_PLUS)
        advance
      elsif @current_char == '-'
        tokens << Token.new(TT_MINUS)
        advance
      elsif @current_char == '*'
        tokens << Token.new(TT_MUL)
        advance
      elsif @current_char == '/'
        tokens << Token.new(TT_DIV)
        advance
      elsif @current_char == '('
        tokens << Token.new(TT_LPAREN)
        advance
      elsif @current_char == ')'
        tokens << Token.new(TT_RPAREN)
        advance
      else
        pos_start = @pos.copy
        char = @current_char
        advance
        return [], IllegalCharError.new(pos_start, @pos, "'#{char}'")
      end
    end

    return tokens, nil
  end

  def make_number
    num_str = ''
    dot_count = 0

    while @current_char && (DIGITS + '.').include?(@current_char)
      if @current_char == '.'
        if dot_count == 1
          break
        end
        dot_count += 1
        num_str += '.'
      else
        num_str += @current_char
      end
      advance
    end

    if dot_count == 0
      Token.new(TT_INT, num_str.to_i)
    else
      Token.new(TT_FLOAT, num_str.to_f)
    end
  end
end

#######################################
# RUN
#######################################

class Basic
  def self.run(fn, text)
    lexer = Lexer.new(fn, text)
    tokens, error = lexer.make_tokens
    return tokens, error
  end
end
