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

class RTError < Error
  attr_accessor :context

  def initialize(pos_start, pos_end, details, context)
    super(pos_start, pos_end, 'Runtime Error', details)
    @context = context
  end

  def as_string
    result = generate_traceback
    result += "#{@error_name}: #{@details}"
    result += "\n\n" + string_with_arrows(@pos_start.ftxt, @pos_start, @pos_end)
    result
  end

  def generate_traceback
    result = ''
    pos = @pos_start
    ctx = @context

    while ctx
      result = "  File #{pos.fn}, line #{pos.ln + 1}, in #{ctx.display_name}\n" + result
      pos = ctx.parent_entry_pos
      ctx = ctx.parent
    end

    "Traceback (most recent call last):\n" + result
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
    self # return self for method chaining
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
  
    # Check for unary + or -
    if @current_char == '-' || @current_char == '+'
      num_str += @current_char
      advance
    end
  
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
# Run Time Result
##################

class RTResult
  attr_accessor :value, :error

  def initialize
    @value = nil
    @error = nil
  end

  def register(res)
    @error = res.error if res.error
    res.value
  end

  def success(value)
    @value = value
    self
  end

  def failure(error)
    @error = error
    self
  end
end

##################
# Nodes
##################

class NumberNode
  attr_reader :tok, :pos_start, :pos_end

  def initialize(tok)
    @tok = tok
    @pos_start = @tok.pos_start
    @pos_end = @tok.pos_end
  end

  def to_s
    @tok.to_s
  end

  def inspect
    to_s
  end
end

class BinOpNode
  attr_reader :left_node, :op_tok, :right_node, :pos_start, :pos_end

  def initialize(left_node, op_tok, right_node)
    @left_node = left_node
    @op_tok = op_tok
    @right_node = right_node

    @pos_start = @left_node.pos_start
    @pos_end = @right_node.pos_end
  end

  def to_s
    "(#{@left_node}, #{@op_tok}, #{@right_node})"
  end

  def inspect
    to_s
  end
end

class UnaryOpNode
  attr_reader :op_tok, :node, :pos_start, :pos_end

  def initialize(op_tok, node)
    @op_tok = op_tok
    @node = node

    @pos_start = @op_tok.pos_start
    @pos_end = @node.pos_end
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
  
    # Handle unary operators like -5 or +5
    if [TT_PLUS, TT_MINUS].include?(tok.type)
      op_tok = tok
      res.register(advance)  # Move past the unary operator
      factor_node = res.register(factor)  # Recursively parse the factor after the unary operator
      if res.error
        return res
      end
      return res.success(UnaryOpNode.new(op_tok, factor_node))  # Create a UnaryOpNode for the unary operation
  
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


#Number

class Number
  attr_accessor :value, :pos_start, :pos_end, :context

  def initialize(value)
    @value = value
    set_pos
    set_context
  end

  def set_pos(pos_start = nil, pos_end = nil)
    @pos_start = pos_start
    @pos_end = pos_end
    self
  end

  def set_context(context = nil)
    @context = context
    self
  end

  def added_to(other)
    if other.is_a?(Number)
      return Number.new(@value + other.value).set_context(@context), nil
    end
  end

  def subbed_by(other)
    if other.is_a?(Number)
      return Number.new(@value - other.value).set_context(@context), nil
    end
  end

  def multed_by(other)
    if other.is_a?(Number)
      return Number.new(@value * other.value).set_context(@context), nil
    end
  end

  def dived_by(other)
    if other.is_a?(Number)
      if other.value == 0
        return nil, RTError.new(
          other.pos_start, other.pos_end,
          'Division by zero',
          @context
        )
      end
      return Number.new(@value / other.value).set_context(@context), nil
    end
  end

  def to_s
    @value.to_s
  end

  def inspect
    to_s
  end
end

################
# Context
####################

class Context
  attr_accessor :display_name, :parent, :parent_entry_pos

  def initialize(display_name, parent = nil, parent_entry_pos = nil)
    @display_name = display_name
    @parent = parent
    @parent_entry_pos = parent_entry_pos
  end
end


#######################################
# Interpreter
#######################################

class Interpreter
  def visit(node, context)
    method_name = "visit_#{node.class.name}"
    if respond_to?(method_name)
      return send(method_name, node, context)
    else
      return no_visit_method(node, context)
    end
  end

  def no_visit_method(node, context)
    raise "No visit_#{node.class.name} method defined"
  end

  ###################################

  def visit_NumberNode(node, context)
    RTResult.new.success(
      Number.new(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
    )
  end

  def visit_BinOpNode(node, context)
    res = RTResult.new
    left = res.register(visit(node.left_node, context))
    return res if res.error

    right = res.register(visit(node.right_node, context))
    return res if res.error

    result, error = case node.op_tok.type
                    when TT_PLUS
                      left.added_to(right)
                    when TT_MINUS
                      left.subbed_by(right)
                    when TT_MUL
                      left.multed_by(right)
                    when TT_DIV
                      left.dived_by(right)
                    end

    if error
      res.failure(error)
    else
      res.success(result.set_pos(node.pos_start, node.pos_end))
    end
  end

  def visit_UnaryOpNode(node, context)
    res = RTResult.new
    number = res.register(visit(node.node, context))
    return res if res.error

    error = nil

    if node.op_tok.type == TT_MINUS
      number, error = number.multed_by(Number.new(-1))
    end

    if error
      res.failure(error)
    else
      res.success(number.set_pos(node.pos_start, node.pos_end))
    end
  end
end

#######################################
# RUN
#######################################

class Basic
  def self.run(fn, text)
    # Generate tokens
    lexer = Lexer.new(fn, text)
    tokens, error = lexer.make_tokens
    return nil, error if error

    # Generate AST
    parser = Parser.new(tokens)
    ast = parser.parse
    return nil, ast.error if ast.error

    # Run program
    interpreter = Interpreter.new
    context = Context.new('<program>')
    result = interpreter.visit(ast.node, context)

    return result.value, result.error
  end
end
