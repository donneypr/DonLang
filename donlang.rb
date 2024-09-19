require_relative 'strings_with_arrows'

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
    result  = "#{@error_name}: #{@details}\n"
    result += "File #{@pos_start.fn}, line #{@pos_start.ln + 1}\n\n"
    result += string_with_arrows(@pos_start.ftxt, @pos_start, @pos_end)
    result
  end
end

class IllegalCharError < Error
  def initialize(pos_start, pos_end, details)
    super(pos_start, pos_end, 'Illegal Character', details)
  end
end

class InvalidSyntaxError < Error
  def initialize(pos_start, pos_end, details)
    super(pos_start, pos_end, 'Invalid Syntax', details)
  end
end

######################
# POSITION
#######################

class Position
  attr_accessor :idx, :ln, :col, :fn, :ftxt

  def initialize(idx, ln, col, fn, ftxt)
    @idx = idx       # Character index in the text
    @ln = ln         # Line number
    @col = col       # Column number
    @fn = fn         # File name (or <stdin> for input)
    @ftxt = ftxt     # File text (the source code)
  end

  def advance(current_char = nil)
    @idx += 1
    @col += 1

    if current_char == "\n"
      @ln += 1
      @col = 0
    end
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
TT_EOF = 'EOF'

class Token
  attr_accessor :type, :value, :pos_start, :pos_end

  def initialize(type, value = nil, pos_start = nil, pos_end = nil)
    @type = type
    @value = value

    if pos_start
      @pos_start = pos_start.copy
      @pos_end = pos_start.copy
      @pos_end.advance
    end

    @pos_end = pos_end if pos_end
  end

  def to_s
    @value ? "#{@type}:#{@value}" : "#{@type}"
  end

  def inspect
    to_s
  end
end

#######################################
# LEXER
#######################################

class Lexer
  attr_accessor :fn, :text, :pos, :current_char

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
    puts "Advanced to char: #{@current_char}"  # Debugging line
  end
  

  def make_tokens
    tokens = []

    while @current_char != nil
      if ' \t'.include?(@current_char)
        advance
      elsif DIGITS.include?(@current_char)
        tokens << make_number
      elsif @current_char == '+'
        tokens << Token.new(TT_PLUS, nil, @pos)
        advance
      elsif @current_char == '-'
        tokens << Token.new(TT_MINUS, nil, @pos)
        advance
      elsif @current_char == '*'
        tokens << Token.new(TT_MUL, nil, @pos)
        advance
      elsif @current_char == '/'
        tokens << Token.new(TT_DIV, nil, @pos)
        advance
      elsif @current_char == '('
        tokens << Token.new(TT_LPAREN, nil, @pos)
        advance
      elsif @current_char == ')'
        tokens << Token.new(TT_RPAREN, nil, @pos)
        advance
      else
        pos_start = @pos.copy
        char = @current_char
        advance
        return [], IllegalCharError.new(pos_start, @pos, "'#{char}'")
      end
    end

    tokens << Token.new(TT_EOF, nil, @pos)
    return tokens, nil
  end

  def make_number
    num_str = ''
    dot_count = 0
    pos_start = @pos.copy

    while @current_char != nil && (DIGITS + '.').include?(@current_char)
      if @current_char == '.'
        break if dot_count == 1
        dot_count += 1
        num_str += '.'
      else
        num_str += @current_char
      end
      advance
    end

    if dot_count == 0
      Token.new(TT_INT, num_str.to_i, pos_start, @pos)
    else
      Token.new(TT_FLOAT, num_str.to_f, pos_start, @pos)
    end
  end
end

##################
# Nodes
##################
class NumberNode
  attr_reader :tok

  def initialize(tok)
    @tok = tok
  end

  def to_s
    @tok.to_s
  end

  def inspect
    to_s
  end
end

class BinOpNode
  attr_reader :left_node, :op_tok, :right_node

  def initialize(left_node, op_tok, right_node)
    @left_node = left_node
    @op_tok = op_tok
    @right_node = right_node
  end

  def to_s
    "(#{@left_node}, #{@op_tok}, #{@right_node})"
  end

  def inspect
    to_s
  end
end

class UnaryOpNode
  attr_reader :op_tok, :node

  def initialize(op_tok, node)
    @op_tok = op_tok
    @node = node
  end

  def to_s
    "(#{@op_tok}, #{@node})"
  end

  def inspect
    to_s
  end
end

#######################################
# ParseResult
#######################################

class ParseResult
  attr_accessor :error, :node

  def initialize
    @error = nil
    @node = nil
  end

  def register(res)
    if res.is_a?(ParseResult)
      @error = res.error if res.error
      return res.node
    end

    res
  end

  def success(node)
    @node = node
    self
  end

  def failure(error)
    @error = error
    self
  end
end

#######################################
# Parser
#######################################

class Parser
  attr_reader :tokens, :tok_idx, :current_tok

  def initialize(tokens)
    @tokens = tokens
    @tok_idx = -1
    advance
  end

  def advance
    @tok_idx += 1
    if @tok_idx < @tokens.length
      @current_tok = @tokens[@tok_idx]
    else
      @current_tok = Token.new(TT_EOF)
    end
    puts "Current token: #{@current_tok}"  # Debugging line
    @current_tok
  end

  def parse
    res = expr
    if !res.error && @current_tok.type != TT_EOF
      return res.failure(InvalidSyntaxError.new(
        @current_tok.pos_start, @current_tok.pos_end,
        "Expected '+', '-', '*' or '/'"
      ))
    end  
    res
  end

  ###################################

  def factor
    res = ParseResult.new
    tok = @current_tok
  
    puts "Current token in factor: #{tok}"  # Debugging output
  
    # Handle unary operators like -5 or +5
    if [TT_PLUS, TT_MINUS].include?(tok.type)
      op_tok = tok
      res.register(advance)  # Move past the unary operator
      factor = res.register(factor)  # Recursively parse the factor after the unary operator
      if res.error
        return res
      end
      return res.success(UnaryOpNode.new(op_tok, factor))  # Create a UnaryOpNode for the unary operation
  
    # Handle numbers (INT or FLOAT)
    elsif [TT_INT, TT_FLOAT].include?(tok.type)
      res.register(advance)  # Move past the number
      return res.success(NumberNode.new(tok))  # Return a NumberNode for the number
  
    # Handle parentheses (LPAREN and RPAREN)
    elsif tok.type == TT_LPAREN
      res.register(advance)  # Move past '('
      expr_node = res.register(expr)  # Parse the expression inside the parentheses
      if res.error
        return res
      end
  
      if @current_tok.type == TT_RPAREN
        res.register(advance)  # Move past ')'
        return res.success(expr_node)
      else
        return res.failure(InvalidSyntaxError.new(
          @current_tok.pos_start, @current_tok.pos_end, "Expected ')'"
        ))
      end
    end
  
    return res.failure(InvalidSyntaxError.new(
      tok.pos_start, tok.pos_end, "Expected int, float, '+', '-', or '('"
    ))
  end
  

  def term
    bin_op(method(:factor), [TT_MUL, TT_DIV])
  end

  def expr
    bin_op(method(:term), [TT_PLUS, TT_MINUS])
  end

  ###################################

  def bin_op(func, ops)
    res = ParseResult.new
    left = res.register(func.call)
    return res if res.error

    while ops.include?(@current_tok.type)
      op_tok = @current_tok
      res.register(advance)
      right = res.register(func.call)
      return res if res.error
      left = BinOpNode.new(left, op_tok, right)
    end

    res.success(left)
  end
end

#######################################
# RUN
#######################################

class Basic\
  def self.run(fn, text)
    lexer = Lexer.new(fn, text)
    tokens, error = lexer.make_tokens
    return nil, error if error

    parser = Parser.new(tokens)
    ast = parser.parse

    if ast.node && !ast.error
      puts ast.node.to_s
    end

    return ast.node, ast.error
  end
end
