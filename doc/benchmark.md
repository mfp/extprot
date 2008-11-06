
Short story:

* extprot protocols are fairly compact, taking often 5 times less
  space than XML-based serializations (and half as much space as a gzipped XML
  representation)
* extprot is fast: it can deserialize one to two orders of magnitude faster
  than XML.

## Benchmark: complex message

Here's a complex protocol definition, with nested tuples, lists,
arrays, and sum types:

    type sum_type 'a 'b 'c = A 'a | B 'b | C 'c | D

    message complex_rtt =
      A {
        a1 : [ ( int * [|bool|] ) ];
        a2 : [ sum_type<int, string, long> ]
        }
    | B {
        b1 : bool;
        b2 : (string * [int])
      }

The value

    {
      Complex_rtt.a1 =
       [
         ((-247634575),
           [| true; false; false; false; true; true; false; false; true |]);
         (3484, [| false; true; true; false; true; true; false; false |]);
         ((-4), [| false; false; false; false; false; false; true |]) ];
      Complex_rtt.a2 =
       [ Sum_type.C 2896779717145980192L; Sum_type.C (-3674915657590371643L) ] }

is serialized by extprot as the 110-byte message

    00000000  01 6c 02 05 4e 03 01 1c  02 00 9d ea 94 ec 01 05  |.l..N...........|
    00000010  13 09 02 01 02 00 02 00  02 00 02 01 02 01 02 00  |................|
    00000020  02 00 02 01 01 17 02 00  b8 36 05 11 08 02 00 02  |.........6......|
    00000030  01 02 01 02 00 02 01 02  01 02 00 02 00 01 14 02  |................|
    00000040  00 07 05 0f 07 02 00 02  00 02 00 02 00 02 00 02  |................|
    00000050  00 02 01 05 19 02 21 0a  01 06 20 ed 0a 11 ce 6d  |......!... ....m|
    00000060  33 28 21 0a 01 06 c5 0a  ce 11 ae 13 00 cd        |3(!...........|
    0000006e

### Deserialization speed

200000 random messages (where all the lists, arrays and strings have a random
length ranging from 0 to 9) are generated, totalling 16425500 bytes.
The OCaml implementation of extprot deserializes them in 0.14s with an AMD
Athlon(tm) 64 X2 6000+ processor, i.e., 780 ns per 81-byte message on average.

### Comparison to XML

Lacking a canonical XML serialization, there are many ways to serialize the
structure as XML.

For the above example (the 110-byte message), a straightforward mapping of
low-level types to XML tags results in the following 716-byte XML message
(newlines and spaces added for readability, not included in the length):

    <complex_rtt_A>
      <list>
        <tuple>
          <int>-247634575</int>
          <array>
            <bool>true</bool><bool>false</bool><bool>false</bool>
            <bool>false</bool><bool>true</bool><bool>true</bool>
            <bool>false</bool><bool>false</bool><bool>true</bool>
          </array>
        </tuple>
        <tuple>
          <int>3484</int>
          <array>
            <bool>false</bool><bool>true</bool><bool>true</bool>
            <bool>false</bool><bool>true</bool><bool>true</bool>
            <bool>false</bool><bool>false</bool>
          </array>
        </tuple>
        <tuple>
          <int>-4</int>
          <array>
            <bool>false</bool><bool>false</bool><bool>false</bool>
            <bool>false</bool><bool>false</bool><bool>false</bool>
            <bool>true</bool>
          </array>
        </tuple>
      </list>
      <list>
        <sum_C><long>2896779717145980192</long></sum_C>
        <sum_C><long>-3674915657590371643</long></sum_C>
      </list>
    </complex_rtt_A>

The XML doesn't include any information the extprot serialization doesn't
(appart from the tag names); for instance, all boolean values are tagged in
the extprot protocol.

In this case, the XML representation takes 6.5 times more space than extprot.
We could bring it down to around 300 bytes (a mere 3 times more than extprot)
by using something like the following, at the cost of some ambiguity and
additional complexity in the lexer for data sections:

    <complex_rtt_A>
      <list>
        <tuple>
           -247634575
          <array> t f f f t t f f t </array>
        </tuple>
        <tuple>
           3484
          <array> f t t f t t f f </array>
        </tuple>
        <tuple>
           -4
          <array> f f f f f f t </array>
        </tuple>
      </list>
      <list>
        <sum_C> 2896779717145980192 </sum_C>
        <sum_C> -3674915657590371643 </sum_C>
      </list>
    </complex_rtt_A>

### Space

Averaged over 100000 messages, the quotient is

    47487011 / 8095530 = 5.87

Compression is often mentioned as a solution to XML's verbosity. Indeed, if
compressed as a whole with GZip, the 100000 messages take but 5MB, below the
8MB required with extprot. There are two problems, though:

1. This approach only works for long sequences of messages that can be decoded
  linearly (without random access)
1. Decompression (and compression) can be relatively costly.

(2) is illustrated in the next section; (1) is inherent to the nature of
adaptive compressors: unless the message is long enough, there's not enough
data to build an adapted model. The 716-byte XML message shown above
compresses to 198 bytes with gzip -9 (165 bytes in the second, condensed
version): significantly less, but still far from the 110 bytes for
uncompressed extprot (98 when compressed). Compression becomes increasingly
ineffective as the size of the message decreases:

        extprot        XML          compressed XML    compressed extprot
    -------------- --------------- ----------------- --------------------
        15              98              86              NO GAIN
        19              117             103             NO GAIN
        24              136             114             NO GAIN
        31              157             126             NO GAIN
        49              225             150             NO GAIN
        78              385             185             NO GAIN
        110             716             198             98
        134             844             180             115
        152             913             255             140
        176             1040            256             158
        218             1329            285             184
        246             1493            311             198

### Speed

Here are the measurements for 100000 random messages on a mid-spec desktop
computer with the OCaml extprot implementation (as of 2008-11-05):

        description                                            CPU time
    -------------------------------------------------     ----------------------
     extprot serialization (completely unoptimized)          260 ms
     gzip compression of XML representation                 1080 ms
     extprot deserialization                                  75 ms (average)
     gzip decompression of XML representation                220 ms
     parsing XML data with expat, no callbacks              1090 ms (best of 10)
       (i.e., no proper deserialization)

Parsing the XML data alone, without actually reconstructing the data
structures, is 15 times slower than full deserialization with extprot, in this
case. extprot can also deserialize 3 times faster than gzip can decompress the
data.
