
extprot protocols define *messages* which are specified using an abstract
syntax. Each message is composed of a number of *fields* of different
*types*.

Messages are defined as follows:

    message message_name = {
      field1 : some_type;
      field2 : some_other_type;
    }

Types are named with

    type typename = <<type>>

## Primitive types

The primitive types are:

* bool
* byte: 8 bit integer
* int: signed integer of at least 31 bits
* long: signed 64-bit integer
* float: 64-bit double
* string: byte array

These are therefore valid type definitions:

    type finished = bool
    type id = int

    type name = string

    type date = float  (* comments are surrounded by (* and *), which can be
                          nested  *)

## Composed types

Recursive types are deliberately not supported. This restriction might be
lifted in the future once the implications for target languages with limited
type systems are understood.

### Tuples

N elements of arbitrary types can be combined into a tuple:

    (* silly types *)
    type id_and_name = (id * name)
    type id_and_name_and_date = (id * name * date)

    type two_tuples = (id_and_name * id_and_name)

### Lists and arrays

Homogeneous list and array types can be built from the type of its elements as
follows:

    type list_of_ints = [ int ]
    type array_of_ints = [| int |]

    type list_of_list_of_ints = [ [int] ]
    type list_of_arrays_of_ints = [ array_of_ints ]  (* also [ [| int |] ] *)
    type list_of_int_pairs = [ (int * int) ]

Note that there is no difference between lists and arrays in the
[low-level encoding](encoding.md): they only serve as hints for
the [mapping to the target language](language-mapping.md).

### Sum types, aka. disjoint sums

These types are a tagged union of different types. Consider a _shape_ type,
which can be either a triangle with three vertices, or a circle with a center
and a radius:

    type vertex = float * float (* 2D coordinates *)

    type shape =
        (* a triangle has got 3 vertices *)
        Triangle vertex vertex vertex
        (* a circle has got a center and a radius *)
      | Circle vertex float

In this example, "Triangle" and "Circle" are called
*non-constant constructors*. Constant constructors represent good-old
enumerations:

    type color = Red | Blue
    type active = Yes | No
    type status = Unregistered | Registered date (* registration date *)

### Polymorphic types

Type definitions can have type variables which must be instantiated later;
the names of the type variables are prefixed by a single quote:

    type pair 'a = ('a * 'a)
    type one_or_many 'a = ('a * [ 'a ])
    (* a tuple with one element and a list which might be empty *)

    type pair_of_ints = pair<int>;
    type one_or_many_pairs 'a = one_or_many< pair<'a> >
    type one_or_many_int_pairs = one_or_many_pairs<int>

### Messages

Messages are also a valid type:

    message contact_info = {
      email : string;
      phone : string
    }

    message person = {
      id : int;
      name : string;
      contact_info : contact_info;
    }

Messages are always monomorphic: they don't accept type parameters. This
restriction might be lifted in the future.

Message fields can be declared *mutable*:
    message person = {
      id : int;
      username : string;
      mutable name : string
    }

This doesn't affect the protocol, only the [mapping to the target
language](doc/language-mapping.md): the _mutable_ keyword will control whether
attribute writers (or an equivalent mechanism) are created in the generated
code.

### Disjoin message unions

The disjoint union of messages can be expressed with a syntax similar to
that of sum types:

    message shape =
        Triangle { a : vertex; b : vertex; c : vertex }
      | Circle { center : vertex; radius : float }
      | Polygon { vertices : [vertex] }

extprot only (de)serializes messages, not directly types, but the above could
also be done in a roundabout way with

    type shape =
        Triangle vertex vertex vertex
      | Circle vertex float
      | Polygon [vertex]

    message element = { shape : shape }

Messages don't differ much from non-constant constructors.

### Message subsets

When only a subset of the fields of a message is needed, it is possible to use
customized deserialization for the specific subset instead of full
deserialization followed by projection. This can be expressed with the
following syntaxes:

     message foo     = { a : int; b : bool; c : string }
     message subset1 = {| foo with b; c |}
     message subset2 = {| foo not b |}

Message subsets can be defined either by enumerating the fields of interest
(`with` syntax) or by listing those to be excluded (`not` syntax).

In this example, the deserialization functions generated for `subset1` will
deserialize only fields `b` and `c` in `foo` messages, and `subset2` would
contain `a` and `c`.

Message subsets are only defined for simple messages defined either directly
or as the application of a record type, e.g.:

   type rec1      = { a : int; b : bool; c : int }
   type rec2 'a   = { a : int; b : 'a; c : 'a }
   message m1     = rec1
   message m2     = rec2<string>
   message m1_c   = {| rec1 with c |}
   message m2_a_c = {| rec2 not b |}

The custom deserializers can skip directly over unwanted fields, which can
save a lot of work when these correspond to complex or large values.

