
module Extprot

class ExtprotError < StandardError; end
class BadWireType < ExtprotError; end

class Enum
  attr_accessor :tag
  def initialize(tag); @tag = tag end
  def inspect; "E#{tag}" end
  def to_i; tag end
end

class HTuple < Array
  attr_accessor :tag
  def initialize(a, t); super(a); @tag = t end
end

class Tuple < HTuple; def inspect; "T#{tag} " + super end end

class Assoc < Hash
  attr_accessor :tag
  def initialize(h,t); update!(h); @tag = t end
end

module Readers
  module Aux
    def ll_tag(n); n >> 4 end
    def read_vint(io)
      b = io.readchar
      return b if b < 128
      x = e = 0
      while b >= 128
        x += (b - 128) << e
        e += 7
        b = io.readchar
      end
      x + (b << e)
    end
    alias_method :read_prefix, :read_vint
  end

  # We essentially have to do functions[prefix & 0xf].call(prefix, io)
  # The fastest way is to use polymorphism (case is O(n), Proc#call is too slow)
  class << self; include Aux end
  class Base; class << self; include Aux end end
  class Vint < Base; def self.read(_, io); n = read_vint(io); (n >> 1) ^ -(n & 1) end end
  class Bits8 < Base; def self.read(_, io); io.readchar end end
  class Bits32 < Base;def self.read(_, io); io.read(4).unpack("V")[0] end end
  class Bits64_long < Base; def self.read(_, io); io.read(8).unpack("q")[0] end end
  class Bits64_float < Base; def self.read(_, io); io.read(8).unpack("E") end end
  class Enum < Base; def self.read(prefix, io); ::Extprot::Enum.new(ll_tag(prefix)) end end
  class Bytes < Base; def self.read(_, io); len = read_vint(io); io.read(len) end end
  class Invalid < Base; def self.read(_, io); raise BadWireType end end

  class Tuple_base < Base
    def self.read_array(klass, prefix, io)
      tag = ll_tag(prefix)
      _ = read_vint(io)
      nelms = read_vint(io)
      a = Array.new(nelms){ Readers.read_value(io) }
      klass.new(a, tag)
    end
  end

  class Tuple < Tuple_base
    def self.read(prefix, io); read_array(::Extprot::Tuple, prefix, io) end
  end

  class HTuple < Tuple_base
    def self.read(prefix, io); read_array(::Extprot::HTuple, prefix, io) end
  end

  class Assoc < Base
    def self.read(prefix, io)
      tag = ll_tag(prefix)
      _ = read_vint
      nelms = read_vint
      r = {}
      nelms.times do |i|
        k = read_value
        v = read_value
        r[k] = v
      end
      ::Extprot::Assoc.new(r, tag)
    end
  end

  LL_TYPES = [
    Vint, Tuple, Bits8, Bytes, Bits32, HTuple, Bits64_long, Assoc,
    Bits64_float, Invalid, Enum, Invalid, Invalid, Invalid, Invalid, Invalid,
  ]

  def self.read_value(io)
    prefix = read_prefix(io)
    LL_TYPES[prefix & 0xf].read(prefix, io)
  end
end

class Decoder
  def initialize(io); @io = io end
  def read_value; Readers.read_value(@io) end
end

end # module Extprot

if __FILE__ == $0
  decoder = Extprot::Decoder.new(ARGF)
  begin
    loop { decoder.read_value }
  rescue EOFError
  end
end
