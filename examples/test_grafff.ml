
open Extprot
open Grafff
open Opt_byte

let bright_red = (100, Set 0, Set 100)
let hsv_0_0_100 = (100, Unset, Unset)
let black = (0, Unset, Unset)

let triangle1 = Shape.Polygon { Shape.vertices = [ (0, 0); (0, 1); (1, 0) ] }
let circle1 = Shape.Circle { Shape.center = (2, 2); radius = 1 }

let grafff_0_0_1 = { Grafff.objects = [ (triangle1, 100); (circle1, 0); ] }

let grafff_1_0 =
  {
    Grafff1.objects = [ (triangle1, 100, Brush.Hello_Kitty 10);
                        (circle1, 0, Brush.Calligraphic 4); ]
  }

let grafff_2_0 =
  {
    Grafff2.objects =
      [
        (triangle1, bright_red, Brush.Hello_Kitty 10, Filling.No_filling);
        (circle1, black, Brush.Calligraphic 4, Filling.Alpha_filling 50);
      ]
  }

let write = Conv.serialize
let read = Conv.deserialize

let aeq pp ?(msg = "") expected actual =
  if expected <> actual then begin
    Format.printf "Wrong output: %s@.Expected:@.%a@.Got:@.%a@."
      msg pp expected pp actual
  end

let check () =
  let s0 = write Grafff.write_grafff grafff_0_0_1 in
  let s1 = write Grafff1.write_grafff1 grafff_1_0 in
  let s2 = write Grafff2.write_grafff2 grafff_2_0 in
  (* new fields ignored by older clients *)
  let g0 = read Grafff.read_grafff s0 in
  let g0' = read Grafff.read_grafff s1 in
  let g0'' = read Grafff.read_grafff s2 in
    aeq Grafff.pp_grafff g0 g0';
    aeq Grafff.pp_grafff g0 g0'';
  (* default values for missing new fields *)
  let g1' = read Grafff1.read_grafff1 s0 in
    aeq Grafff1.pp_grafff1
      { Grafff1.objects = [(triangle1, 100, Brush.Default); (circle1, 0, Brush.Default)] }
      g1';
  (* primitive types expanded to composed types (tuples) *)
  let g2' = read Grafff2.read_grafff2 s1 in
    aeq Grafff2.pp_grafff2
      { Grafff2.objects =
          [(triangle1, hsv_0_0_100, Brush.Hello_Kitty 10, Filling.No_filling);
           (circle1, black, Brush.Calligraphic 4, Filling.No_filling)] }
      g2';
    ()

let () =
  try check ()
  with Error.Extprot_error(err, loc) ->
    Format.printf "Extprot error:@.%a@." Error.pp_extprot_error (err, loc)
