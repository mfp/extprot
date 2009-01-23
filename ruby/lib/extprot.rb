
module Extprot

class ExtprotError < StandardError; end
class BadWireType < ExtprotError; end

module Codec
  @@types = [
    :vint, :tuple, :bits8, :bytes,
    :bits32, :htuple, :bits64_long, :assoc,
    :bits64_float, :invalid, :enum, :invalid,
    :invalid, :invalid, :invalid, :invalid
  ]

  def ll_type(n); @@types[n & 0xf] end
  def ll_tag(n); n >> 4 end

  module_function :ll_type, :ll_tag
end

class HTuple < Array; attr_accessor :tag end
class Enum < Struct.new(:tag); def inspect; "E#{tag}" end end

class Tuple < Array
  attr_accessor :tag
  def inspect; "T#{tag} " + super end
end

class Assoc < Hash
  attr_accessor :tag
  def initialize(h,t)
    update!(h)
    @tag = t
  end
end

class Reader
  include Codec

  def initialize(io); @io = io end
  def read_bytes(n); @io.read(n) end
  def read_byte; @io.readchar end
  def read_raw_bool; read_byte != 0 end
  def read_raw_i8; read_byte end
  def read_raw_rel_int; n = read_vint; (n >> 1) ^ -(n & 1) end
  def read_raw_i32; read_bytes(4).unpack("V")[0] end
  def read_raw_i64; read_bytes(8).unpack("q")[0] end
  def read_raw_float; read_bytes(8).unpack("E") end
  def read_raw_string; len = read_vint; read_bytes(len) end

  def read_vint
    b = read_byte
    raise EOFError unless b
    return b if b < 128
    x = e = 0
    while b >= 128
      x += (b - 128) << e
      e += 7
      b = read_byte
    end
    x + (b << e)
  end
  alias_method :read_prefix, :read_vint
end

class Decoder
  include Codec

  def initialize(reader); @rd = reader end

  def read_value
    prefix = @rd.read_prefix
    tag = ll_tag(prefix)
    case ll_type(prefix)
    when :tuple; t = read_htuple(prefix); r = Tuple.new(t); r.tag = t.tag; r
    when :htuple; read_htuple(prefix)
    when :assoc; read_assoc(prefix)
    when :vint; @rd.read_raw_rel_int
    when :bits8; @rd.read_byte
    when :bits32; @rd.read_raw_i32
    when :bits64_long; @rd.read_raw_i64
    when :bits64_float; @rd.read_raw_float
    when :enum; Enum.new(tag)
    when :bytes; @rd.read_raw_string
    when :invalid; raise BadWireType
    end
  end

  private

  def read_htuple(prefix)
    tag = ll_tag(prefix)
    _ = @rd.read_vint
    nelms = @rd.read_vint
    a = Array.new(nelms)
    nelms.times{|i| a[i] = read_value }
    r = HTuple.new(a)
    r.tag = tag
    r
  end

  def read_assoc(prefix)
    tag = ll_tag(prefix)
    _ = @rd.read_vint
    nelms = @rd.read_vint
    r = {}
    nelms.times do |i|
      k = read_value
      v = read_value
      r[k] = v
    end
    Assoc.new(r, tag)
  end
end

end # module Extprot

if __FILE__ == $0
  decoder = Extprot::Decoder.new(Extprot::Reader.new(ARGF))
  begin
    loop { decoder.read_value; puts }
  rescue EOFError
  end
end

