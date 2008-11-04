
The low-level encoding specifies how extprot low-level types are serialized. 

## Wire types

All the types declared in the [protocol definition](protocol-definition.md)
are mapped to one of the following low-level types:

Basic types:

 * Vint: variable size integers, used for ints (see below)
 * Bits8: 8 bits, used for byte and bool
 * Bits32: 32 bits, currently unused
 * Bits64_long: 64 bits, 64-bit signed integers
 * Bits64_float: 64-bit doubles in IEEE 754 (IEC 559:1989) format
 * Enum: "nil value" (see below), constants in a disjoint type (aka. constant constructors)

Composed types:

 * Tuple: used for tuples and messages
 * Htuple: used for lists and arrays
 * Bytes: byte string
 * Assoc: list of (key, value) pairs

All values are serialized as two components:

    [prefix] [optional data]

The prefix encodes the low-level type and the tag (0 for primitive
types and tuples, or the tag of the disjoint union value).

Composed types are serialized as

    Bytes:            [prefix] [length = N] <N bytes>

    Tuple, Htuple:    [prefix] [length = N] [num elements] <elements>
    Assoc:            [prefix] [length = N] [num pairs]     <pairs>
                                            =========================
                                                 N  bytes

The prefix, length and number of elements/pairs are encoded as variable
integers.

## Variable integers (vints)

vints are an encoding of positive integers using little-endian order and
7-bits per byte. The most-significant bit indicates whether the value
continues (MSB = 1) or not (MSB = 0). 
You can find an 
[explanation of vints here](http://code.google.com/apis/protocolbuffers/docs/encoding.html#varints).

Some examples:

     Integer value   Encoding (hex bytes)
    --------------  ---------------------
                 0                   0x00
                 1                   0x01
               127                   0x7f
               128              0x80 0x01
               129              0x81 0x01
               256              0x80 0x02

## Prefixes

The prefix encodes the low-level type using a numeric wire type and the
tag, using a vint.  The value of the prefix is 

    tag << 4 | wire_type << 1

These are the wire types:

    Type              Wire type
    --------------  -----------
     Vint                    0
     Bits8                   2
     Bits32                  4
     Bits64_long             6
     Bits64_float            8
     Enum                   10
     Tuple                   1
     Bytes                   3
     Htuple                  5
     Assoc                   7

Note how the least-significant bit is 0 for all basic types, and 1 for all
composed types. This means that whenever the LSB of the prefix is 1, the
length of the value follows as an vint.

## Encoding of signed integers (int)

Signed integers (int in the abstract syntax) are encoded using 
[zigzag encoding](http://code.google.com/apis/protocolbuffers/docs/encoding.html#types).

The signed integer "n" is encoded as a vint of value

    (n << 1) | (n >> 63)

## Fixed length values

The Bits8, Bits32, Bits64_long, Bits64_float and Enum low-level types correspond to
values of 8, 32, 64, 64, and 8 bits, which follow the prefix.

### bool

The "bool" type is encoded with the Bits8 low-level type and a single byte of value 0
for false, 1 for true.

### byte

The "byte" type is encoded with the Bits8 low-level type and a single byte.

### enum

The "enum" type is used for constant constructors in a disjoint union type
like

    type color = Red | Black

Enum values are encoded using only the prefix. The tag specifies the
constructor; they are numbered in the order of appearance in the type
declaration, starting from 0. In the above example, Red has got tag 0, and
Black tag 1.

## Examples

### Simple messages

    message a_bool = { v : bool }

Is encoded as (bytes in decimal notation)

    001 003 001 002 001

for v = true, which can be analyzed as

    001     tag 0, wire-type 1 (Tuple)
    003     length in bytes of the rest of the message (after this byte)
    001     1 field

     002    tag 0, wire-type 2 (Bits8)
     001    bool true

For v = false, the message is encoded as 

    001 003 001 002 000

### Tuples

Tuples are encoded the same way as messages (they are but messages with
anonymous fields):

Given

     message a_tuple = { v : (bool * bool) }

the value  

    { v = (true, false) }  
    
is encoded as 

    001 008 001 001 005 002 002 001 002 000
    
    001     tag 0, wire-type 1 (Tuple)
    008     length in bytes of the rest of the message (after this byte)
    001     1 field

     001    tag 0, wire-type 1 (Tuple)
     005    length in bytes of the rest of the tuple
     002    2 elements

      002   tag 0, wire-type 2 (Bits8)
      001   bool true

      002   tag 0, wire-type 2 (Bits8)
      000   bool false

### Sum types (disjoint unions)
Constant constructors in a sum type are numbered starting from 0 in the order
of appearance in the type definition: this is the value used in the tag.
Non-constant constructors are also numbered from 0, in order of appearance.
Each non-constant case is encoded with the Tuple low-level type and the
corresponding tag.

Example:

    type maybe 'a = Unknown | Known 'a

    message foo = { a : maybe<int>; b : maybe<bool> }

The value

    { a = Unknown; b = Known true }

is encoded as

    001 007 002 005 001 003 001 002 001

    001     tag 0, wire-type 1 (Tuple)
    007     length in bytes of the rest of the message (after this byte)
    002     2 fields

     010    tag 0, wire-type 10 (Enum)

     001    tag 0, wire-type 1 (Tuple)
     003    length in bytes of the rest of the message (after this byte)
     001    1 element

      002    tag 0, wire-type 2 (Bits8)
      001    bool true




### Lists and arrays

Both 
    message some_ints = { l : [ int ] }
and
    message some_ints = { l : [| int |] }

have the same encoding: in this case, the difference between lists and arrays
only affects the [mapping to the target language](language-mapping.md). This
means that we can change the type definition freely and interoperate with
older consumers/producers.

The value

    { l = [1, 2, 3, -1] }

is encoded as

    001 012 001 005 009 004 000 002 000 004 000 006 000 001

    001     tag 0, wire-type 1 (Tuple)
    012     length in bytes of the rest of the message (after this byte)
    001     1 field

     005    tag 0, wire-type 5 (Htuple)
     009    length in bytes of the rest of the tuple
     004    4 elements

      000    tag 0, wire-type 0 (Vint)
      002    signed int 1

      000    tag 0, wire-type 0 (Vint)
      004    signed int 2

      000    tag 0, wire-type 0 (Vint)
      006    signed int 3

      000    tag 0, wire-type 0 (Vint)
      001    signed int -1

### Nested messages

Nested messages are encoded as any other type.

    message a_bool = { v : bool }

    message a_bool_and_int = { b : a_bool; i : int }

The value
  
    { b = { v = true }; i = -1 }

is encoded by first encoding the nested message (see above), and then building
the outer Tuple type:

    001 008 002 001 003 001 002 001 000 001

    001     tag 0, wire-type 1 (Tuple)
    008     length in bytes of the rest of the message (after this byte)
    002     2 fields

     001    tag 0, wire-type 1 (Tuple)
     003    length in bytes of the rest of the message (after this byte)
     001    1 field

      002   tag 0, wire-type 2 (Bits8)
      001   bool true

     000    tag 0, wire-type 0 (Vint)
     001    signed int -1

