
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

class Reader 
  include Codec

  def initialize(io)
    @io = io
    @off = 0
  end

  def read_vint
    b = read_byte
    raise "EOF" unless b
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

  def read_bytes(n); 
    r = @io.read(n) 
    @off += r.size
    r
  end

  def read_byte; 
    @off += 1
    @io.getc 
  end

  def check_prim_type(ty, t) 
    p = read_prefix
    if ll_tag(p) != 0 then
      skip_value(p)
      raise "unknown tag"
    end
    if ll_type(p) != ty then
      skip_value(p)
      raise "bad wire type"
    end
  end

  def read_prim_type(ty, &b)
    p = read_prefix
    llty = ll_type(p)
    puts "prefix #{p} type #{llty}"
    if ll_tag(p) == 0 && llty == ty 
      b.call
    elsif ll_tag(p) != 0
      skip_value p
      raise "unknown tag"
    elsif llty == :tuple
      len = read_vint
      eot = offset(len)
      nelms = read_vint
      if nelms >= 1
        p = read_prefix
        if ll_tag(p) != 0
          skip_to(eot)
          raise "unknown tag"
        end
        if ll_type(p) != ty
          skip_to(eot)
          raise "bad_wire_type"
        end
        v = b.call
        skip_to(eot)
        v
      else
        skip_to(eot)
        raise "bad wire type"
      end
    else
      skip_value(p)
      raise "bad wire type"
    end
  end

  def skip_value(p)
    case ll_type(p)
    when :vint; read_vint
    when :bits8; read_byte
    when :bits32; read_bytes(4)
    when :bits64_float, :bits64_long; read_bytes(8)
    when :enum; 
    when :tuple, :htuple, :bytes, :assoc; @rd.read(read_vint)
    when :invalid; raise "bad wire type"
    end
  end

  def read_raw_bool; read_byte != 0 end
  def read_bool; read_prim_type(:bits8){ read_raw_bool } end

  def read_raw_i8; read_byte end
  def read_i8; read_prim_type(:bits8){ read_byte } end

  def read_raw_rel_int;
    n = read_vint
    (n >> 1) ^ -(n & 1)
  end

  def read_rel_int; read_prim_type(:vint){ read_raw_rel_int } end

  def read_raw_i32; read_bytes(4).unpack("V")[0] end

  def read_i32; read_prim_type(:bits32){ read_raw_i32 } end

  def read_raw_i64; read_raw_i32 + read_raw_i32 << 32 end

  def read_i64; read_prim_type(:bits64_long) { read_raw_i64 } end

  def read_raw_float; read_bytes(8).unpack("E") end
  def read_float; read_prim_type(:bits64_float) { read_raw_float } end

  def read_raw_string
    len = read_vint
    read_bytes(len)
  end

  def read_string; read_prim_type(:bytes){ read_raw_string } end

  def offset(len); @off + len end

  def skip_to(off)
    @io.pos += (off - @off) if @off < off
  end
end

class Inspect
  include Codec 

  def initialize(reader, out, verbose = true)
    @rd = reader
    @verbose = verbose
    @out = out
  end

  def inspect
    doinspect(@rd.read_prefix)
  end

  private

  def doinspect(prefix)
    tag = ll_tag(prefix)
    case ll_type(prefix) 
    when :tuple; inspect_tuple("{", "}", prefix)
    when :htuple; inspect_tuple("[", "]", prefix)
    when :assoc; inspect_assoc(prefix)
    when :vint
      if @verbose || tag != 0 
        @out.print("Vint_%d %d" % [tag, @rd.read_raw_rel_int])
      else
        @out.print("%d" % @rd.read_raw_rel_int)
      end
    when :bits8
      if @verbose || tag != 0
        @out.print("I8_%d 0x%x" % [tag, @rd.read_byte])
      else
        @out.print("0x%x" % @rd.read_byte)
      end
    when :bits32
      if @verbose || tag != 0
        @out.print("I32_%d %d" % [tag, @rd.read_raw_i32])
      else
        @out.print("%d" % @rd.read_raw_i32)
      end
    when :bits64_long
      if @verbose || tag != 0
        @out.print("I64_%d %d" % [tag, @rd.read_raw_i64])
      else
        @out.print("%d" % @rd.read_raw_i64)
      end
    when :bits64_float
      if @verbose || tag != 0
        @out.print("Fl_%d %f" % [tag, @rd.read_raw_float])
      else
        @out.print("%f" % @rd.read_raw_float)
      end
    when :enum
      if @verbose
        @out.print("Enum_%d" % tag)
      else
        @out.print("T%d" % tag)
      end
    when :bytes
      if @verbose || tag != 0
        @out.print("B_%s" % @rd.read_raw_string.inspect)
      else
        @out.print(@rd.read_raw_string.inspect)
      end
    when :invalid
      raise "invalid wire type"
    end
  end

  def inspect_tuple(left, right, prefix)
    tag = ll_tag(prefix)
    _ = @rd.read_vint
    nelms = @rd.read_vint
    if @verbose || tag != 0 
      @out.print("T#{tag}#{left} ")
    else
      @out.print("#{left} ")
    end
    (nelms - 1).times{ doinspect(@rd.read_prefix);; @out.print("; ") }
    doinspect(@rd.read_prefix)
    @out.print(" #{right}")
  end

  def inspect_assoc(prefix)
    tag = ll_tag(prefix)
    _ = @rd.read_vint
    nelms = @rd.read_vint
    @out.print("A#{tag} [")
    (nelms - 1).times do
      doinspect(@rd.read_prefix)
      @out.print(", ")
      doinspect(@rd.read_prefix)
      @out.print("; ")
    end
    doinspect(@rd.read_prefix)
    @out.print(", ")
    doinspect(@rd.read_prefix)
  end
end

if __FILE__ == $0
  reader = Reader.new(ARGF)
  out = $stdout
  inspector = Inspect.new(reader, out)
  begin
    loop { inspector.inspect }
  rescue
    puts
  end
end
