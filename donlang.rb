require_relative 'strings_with_arrows'

#######################################
# CONSTANTS
#######################################

DIGITS = '0123456789'
LETTERS = ('a'..'z').to_a.join + ('A'..'Z').to_a.join  
LETTERS_DIGITS = LETTERS + DIGITS                      

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

class ExpectedCharError < Error
  def initialize(pos_start, pos_end, details)
    super(pos_start, pos_end, 'Expected Character', details)
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
TT_POW    = 'POW'
TT_EOF    = 'EOF'
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_EQ = 'EQ'
TT_EE	= 'EE' 
TT_NE	= 'NE' 
TT_LT	= 'LT' 
TT_GT	= 'GT' 
TT_LTE = 'LTE' 
TT_GTE = 'GTE' 

KEYWORDS = [
  'VAR',
  'AND',
  'OR',
  'NOT'
]

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

  def matches(type_, value)
    @type == type_ && @value == value
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
  
    while !@current_char.nil?
      if [' ', "\t"].include?(@current_char)
        advance
      elsif DIGITS.include?(@current_char)
        tokens << make_number
      elsif LETTERS.include?(@current_char)
        tokens << make_identifier
      elsif @current_char == '+'
        tokens << Token.new(TT_PLUS, pos_start: @pos)
        advance
      elsif @current_char == '-'
        tokens << Token.new(TT_MINUS, pos_start: @pos)
        advance
      elsif @current_char == '*'
        tokens << Token.new(TT_MUL, pos_start: @pos)
        advance
      elsif @current_char == '/'
        tokens << Token.new(TT_DIV, pos_start: @pos)
        advance
      elsif @current_char == '^'
        tokens << Token.new(TT_POW, pos_start: @pos)
        advance
      elsif @current_char == '('
        tokens << Token.new(TT_LPAREN, pos_start: @pos)
        advance
      elsif @current_char == ')'
        tokens << Token.new(TT_RPAREN, pos_start: @pos)
        advance
      elsif @current_char == '!'
        token, error = make_not_equals
        return [], error if error
        tokens << token
      elsif @current_char == '='
        tokens << make_equals
      elsif @current_char == '<'
        tokens << make_less_than
      elsif @current_char == '>'
        tokens << make_greater_than
      else
        pos_start = @pos.dup
        char = @current_char
        advance
        return [], IllegalCharError.new(pos_start, @pos, "'#{char}'")
      end
    end
  
    tokens << Token.new(TT_EOF, pos_start: @pos)
    [tokens, nil]
  end
  

  def make_number
    num_str = ''
    dot_count = 0
    pos_start = @pos.copy

    while @current_char != nil && (DIGITS + '.').include?(@current_char)
      if @current_char == '.'
        break if dot_count == 1
        dot_count += 1
      end
      num_str += @current_char
      advance
    end

    if dot_count == 0
      Token.new(TT_INT, num_str.to_i, pos_start, @pos)
    else
      Token.new(TT_FLOAT, num_str.to_f, pos_start, @pos)
    end
  end

  def make_identifier
    id_str = ''
    pos_start = @pos.copy
  
    while @current_char != nil && (LETTERS_DIGITS + '_').include?(@current_char)
      id_str += @current_char
      advance
    end
  
    tok_type = KEYWORDS.include?(id_str) ? TT_KEYWORD : TT_IDENTIFIER
    Token.new(tok_type, id_str, pos_start, @pos)
  end

  def make_not_equals
    pos_start = @pos.dup
    advance
  
    if @current_char == '='
      advance
      return Token.new(TT_NE, pos_start: pos_start, pos_end: @pos), nil
    end
  
    advance
    return nil, ExpectedCharError.new(pos_start, @pos, "'=' (after '!')")
  end
  
  def make_equals
    tok_type = TT_EQ
    pos_start = @pos.dup
    advance
  
    if @current_char == '='
      advance
      tok_type = TT_EE
    end
  
    Token.new(tok_type, pos_start: pos_start, pos_end: @pos)
  end
  
  def make_less_than
    tok_type = TT_LT
    pos_start = @pos.dup
    advance
  
    if @current_char == '='
      advance
      tok_type = TT_LTE
    end
  
    Token.new(tok_type, pos_start: pos_start, pos_end: @pos)
  end
  
  def make_greater_than
    tok_type = TT_GT
    pos_start = @pos.dup
    advance
  
    if @current_char == '='
      advance
      tok_type = TT_GTE
    end
  
    Token.new(tok_type, pos_start: pos_start, pos_end: @pos)
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

class VarAccessNode
  attr_reader :var_name_tok, :pos_start, :pos_end

  def initialize(var_name_tok)
    @var_name_tok = var_name_tok
    @pos_start = @var_name_tok.pos_start
    @pos_end = @var_name_tok.pos_end
  end
end

class VarAssignNode
  attr_accessor :var_name_tok, :value_node, :pos_start, :pos_end

  def initialize(var_name_tok, value_node)
    @var_name_tok = var_name_tok
    @value_node = value_node

    @pos_start = @var_name_tok.pos_start
    @pos_end = @value_node ? @value_node.pos_end : @var_name_tok.pos_end
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
  attr_accessor :error, :node, :advance_count

  def initialize
    @error = nil
    @node = nil
    @advance_count = 0
  end

  def register_advancement
    @advance_count += 1
  end

  def register(res)
    if res
      @advance_count += res.advance_count
      @error = res.error if res.error
      res.node
    else
      nil
    end
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
        "Expected '+', '-', '*', '/', or '^'"
      ))
    end  
    res
  end

  def atom
    res = ParseResult.new
    tok = @current_tok
  
    if [TT_INT, TT_FLOAT].include?(tok.type)
      res.register_advancement
      advance
      return res.success(NumberNode.new(tok))
  
    elsif tok.type == TT_IDENTIFIER
      res.register_advancement
      advance
      return res.success(VarAccessNode.new(tok))
  
    elsif tok.type == TT_LPAREN
      res.register_advancement
      advance
      expr = res.register(expr)
      if res.error
        return res
      end
  
      if @current_tok.type == TT_RPAREN
        res.register_advancement
        advance
        return res.success(expr)
      else
        return res.failure(InvalidSyntaxError.new(
          @current_tok.pos_start, @current_tok.pos_end,
          "Expected ')'"
        ))
      end
  
    elsif @current_tok.matches(TT_KEYWORD, 'VAR')
      # Handle variable assignment inside expressions
      res.register_advancement
      advance
  
      if @current_tok.type != TT_IDENTIFIER
        return res.failure(InvalidSyntaxError.new(
          @current_tok.pos_start, @current_tok.pos_end,
          "Expected identifier after 'VAR'"
        ))
      end
  
      var_name = @current_tok
      res.register_advancement
      advance
  
      if @current_tok.type != TT_EQ
        return res.failure(InvalidSyntaxError.new(
          @current_tok.pos_start, @current_tok.pos_end,
          "Expected '='"
        ))
      end
  
      res.register_advancement
      advance
  
      expr = res.register(expr)
      if res.error
        return res
      end
  
      return res.success(VarAssignNode.new(var_name, expr))
    end
  
    return res.failure(InvalidSyntaxError.new(
      tok.pos_start, tok.pos_end,
      "Expected int, float, identifier, '+', '-', or '('"
    ))
  end
  
  def power
    bin_op(method(:atom), [TT_POW], method(:factor))
  end
  
  def factor
    res = ParseResult.new
    tok = @current_tok
  
    if [TT_PLUS, TT_MINUS].include?(tok.type)
      op_tok = tok
      res.register_advancement
      advance
      factor_node = res.register(factor)
      if res.error
        return res
      end
      return res.success(UnaryOpNode.new(op_tok, factor_node)) 
  
    elsif [TT_INT, TT_FLOAT].include?(tok.type)
      res.register_advancement
      advance
      return res.success(NumberNode.new(tok)) 
  
    elsif tok.type == TT_LPAREN
      res.register_advancement
      advance
      expr_node = res.register(expr)
      if res.error
        return res
      end
  
      if @current_tok.type == TT_RPAREN
        res.register_advancement
        advance
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
    bin_op(method(:power), [TT_MUL, TT_DIV])
  end
  
  def arith_expr
    bin_op(method(:term), [TT_PLUS, TT_MINUS])
  end
  
  def comp_expr
    res = ParseResult.new
  
    if @current_tok.matches(TT_KEYWORD, 'NOT')
      op_tok = @current_tok
      res.register_advancement
      advance
  
      node = res.register(comp_expr)
      return res if res.error
      return res.success(UnaryOpNode.new(op_tok, node))
    end
  
    node = res.register(bin_op(method(:arith_expr), [TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE]))
  
    if res.error
      return res.failure(InvalidSyntaxError.new(
        @current_tok.pos_start, @current_tok.pos_end,
        "Expected int, float, identifier, '+', '-', '(' or 'NOT'"
      ))
    end
  
    res.success(node)
  end

  def expr
    res = ParseResult.new
  
    if @current_tok.matches(TT_KEYWORD, 'VAR')
      res.register_advancement
      advance
  
      if @current_tok.type != TT_IDENTIFIER
        return res.failure(InvalidSyntaxError.new(
          @current_tok.pos_start, @current_tok.pos_end,
          "Expected identifier"
        ))
      end
  
      var_name = @current_tok
      res.register_advancement
      advance
  
      if @current_tok.type != TT_EQ
        return res.failure(InvalidSyntaxError.new(
          @current_tok.pos_start, @current_tok.pos_end,
          "Expected '='"
        ))
      end
  
      res.register_advancement
      advance
      expr = res.register(expr)
      return res if res.error
      return res.success(VarAssignNode.new(var_name, expr))
    end
  
    node = res.register(bin_op(method(:comp_expr), [[TT_KEYWORD, 'AND'], [TT_KEYWORD, 'OR']]))
  
    if res.error
      return res.failure(InvalidSyntaxError.new(
        @current_tok.pos_start, @current_tok.pos_end,
        "Expected 'VAR', int, float, identifier, '+', '-', '(' or 'NOT'"
      ))
    end
  
    res.success(node)
  end
  


  def bin_op(func_a, ops, func_b = nil)
    func_b ||= func_a  # Set func_b to func_a if it is nil
  
    res = ParseResult.new
    left = res.register(func_a.call)
    return res if res.error
  
    while ops.include?(@current_tok.type) || ops.include?([@current_tok.type, @current_tok.value])
      op_tok = @current_tok
      res.register_advancement
      advance
      right = res.register(func_b.call)
      return res if res.error
      left = BinOpNode.new(left, op_tok, right)
    end
  
    res.success(left)
  end
end


# Number

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

  def powed_by(other)
    if other.is_a?(Number)
      return Number.new(@value ** other.value).set_context(@context), nil
    end
  end

  def get_comparison_eq(other)
    if other.is_a?(Number)
      return Number.new((@value == other.value ? 1 : 0)).set_context(@context), nil
    end
  end
  
  def get_comparison_ne(other)
    if other.is_a?(Number)
      return Number.new((@value != other.value ? 1 : 0)).set_context(@context), nil
    end
  end
  
  def get_comparison_lt(other)
    if other.is_a?(Number)
      return Number.new((@value < other.value ? 1 : 0)).set_context(@context), nil
    end
  end
  
  def get_comparison_gt(other)
    if other.is_a?(Number)
      return Number.new((@value > other.value ? 1 : 0)).set_context(@context), nil
    end
  end
  
  def get_comparison_lte(other)
    if other.is_a?(Number)
      return Number.new((@value <= other.value ? 1 : 0)).set_context(@context), nil
    end
  end
  
  def get_comparison_gte(other)
    if other.is_a?(Number)
      return Number.new((@value >= other.value ? 1 : 0)).set_context(@context), nil
    end
  end
  
  def anded_by(other)
    if other.is_a?(Number)
      return Number.new((@value != 0 && other.value != 0 ? 1 : 0)).set_context(@context), nil
    end
  end
  
  def ored_by(other)
    if other.is_a?(Number)
      return Number.new((@value != 0 || other.value != 0 ? 1 : 0)).set_context(@context), nil
    end
  end

  def notted
    return (Number.new(@value == 0 ? 1 : 0).set_context(@context)), nil
  end

  def copy
    Number.new(@value).set_pos(@pos_start, @pos_end).set_context(@context)
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
  attr_accessor :display_name, :parent, :parent_entry_pos, :symbol_table

  def initialize(display_name, parent = nil, parent_entry_pos = nil)
    @display_name = display_name
    @parent = parent
    @parent_entry_pos = parent_entry_pos
    @symbol_table = nil  
  end
end

#################
# Symbol Table
#####################

class SymbolTable
  attr_accessor :symbols, :parent

  def initialize
    @symbols = {}
    @parent = nil
  end

  def get(name)
    value = @symbols[name]
    return @parent.get(name) if value.nil? && @parent
    value
  end

  def set(name, value)
    @symbols[name] = value
  end

  def remove(name)
    @symbols.delete(name)
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

  def visit_NumberNode(node, context)
    RTResult.new.success(
      Number.new(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end)
    )
  end

  def visit_VarAccessNode(node, context)
    res = RTResult.new
    var_name = node.var_name_tok.value
    value = context.symbol_table.get(var_name)
  
    if value.nil?
      return res.failure(RTError.new(
        node.pos_start, node.pos_end,
        "'#{var_name}' is not defined",
        context
      ))
    end
  
    value = value.copy.set_pos(node.pos_start, node.pos_end)
    res.success(value)
  end

  def visit_VarAssignNode(node, context)
    res = RTResult.new
    var_name = node.var_name_tok.value
    value = res.register(visit(node.value_node, context))
    return res if res.error
  
    context.symbol_table.set(var_name, value)
    res.success(value)
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
                    when TT_POW
                      left.powed_by(right)
                    when TT_EE
                      left.get_comparison_eq(right)
                    when TT_NE
                      left.get_comparison_ne(right)
                    when TT_LT
                      left.get_comparison_lt(right)
                    when TT_GT
                      left.get_comparison_gt(right)
                    when TT_LTE
                      left.get_comparison_lte(right)
                    when TT_GTE
                      left.get_comparison_gte(right)
                    else
                      if node.op_tok.matches(TT_KEYWORD, 'AND')
                        left.anded_by(right)
                      elsif node.op_tok.matches(TT_KEYWORD, 'OR')
                        left.ored_by(right)
                      end
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
    elsif node.op_tok.matches(TT_KEYWORD, 'NOT')
      number, error = number.notted
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

$global_symbol_table = SymbolTable.new
$global_symbol_table.set("NULL", Number.new(0))
$global_symbol_table.set("FALSE", Number.new(0))
$global_symbol_table.set("TRUE", Number.new(1))
class Basic
  def self.run(fn, text)
    lexer = Lexer.new(fn, text)
    tokens, error = lexer.make_tokens
    return nil, error if error

    parser = Parser.new(tokens)
    ast = parser.parse
    return nil, ast.error if ast.error

    interpreter = Interpreter.new
    context = Context.new('<program>')
    context.symbol_table = $global_symbol_table
    result = interpreter.visit(ast.node, context)

    return result.value, result.error
  end
end
