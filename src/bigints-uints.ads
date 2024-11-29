with Bigints.Machine_Ints; use Bigints.Machine_Ints;
with Bigints.Primitives;
with Bigints.Const_Choice;

generic
   N : Positive;
package Bigints.Uints with SPARK_Mode => On is
   BITS : constant Positive := 64 * N;
   pragma Assert (64 * N < Positive'Last);

   type Uint is array (1 .. N) of U64;
   type Wide_Uint is array (1 .. 2 * N) of U64;

   type Uint_Carry is record
      Res   : Uint;
      Carry : U64;
   end record;
   type Quotient_Rem is record
      Quotient  : Uint;
      Remainder : U64;
   end record;

   ZERO : constant Uint := [others => 0];
   ONE  : constant Uint := [1 => 1, others => 0];
   MAX  : constant Uint := [others => U64'Last];

   function Concat (Lo, Hi : Uint) return Wide_Uint
   with
     Post =>
       (for all I in 1 .. N => Concat'Result (I) = Lo (I))
       and then (for all I in 1 .. N => Concat'Result (N + I) = Hi (I));
   function Truncate (A : Wide_Uint) return Uint
   with Post => (for all I in 1 .. N => A (I) = Truncate'Result (I));
   function Truncate_Upper (A : Wide_Uint) return Uint
   with Post => (for all I in 1 .. N => A (N + I) = Truncate_Upper'Result (I));

   function Neg (A : Uint) return Uint;

   function Add_Carry (A, B : Uint; Carry : U64) return Uint_Carry;

   function Sub_Borrow (A, B : Uint; Borrow : U64) return Uint_Carry
   with Pre => Borrow = 0 or else Borrow = U64'Last;

   function Mul_Wide (A, B : Uint) return Wide_Uint;

   function Div_Rem_Limb_With_Reciprocal
     (U : Uint; Re : Primitives.Recip) return Quotient_Rem;
   function Rem_Limb_With_Reciprocal
     (U : Uint; Re : Primitives.Recip) return U64;

   function "not" (A : Uint) return Uint
   with Inline, Post => (for all I in 1 .. N => "not"'Result (I) = not A (I));

   function "and" (A, B : Uint) return Uint
   with
     Inline,
     Post => (for all I in 1 .. N => "and"'Result (I) = (A (I) and B (I)));
   function "and" (A : Uint; B : U64) return Uint
   with
     Inline,
     Post => (for all I in 1 .. N => "and"'Result (I) = (A (I) and B));

   function "or" (A, B : Uint) return Uint
   with
     Inline,
     Post => (for all I in 1 .. N => "or"'Result (I) = (A (I) or B (I)));

   function "xor" (A, B : Uint) return Uint
   with
     Inline,
     Post => (for all I in 1 .. N => "xor"'Result (I) = (A (I) xor B (I)));

   function "+" (A, B : Uint) return Uint;

   function "-" (A, B : Uint) return Uint;

   function "*" (A, B : Uint) return Uint;

   function Inv_Mod2k_Vartime (Value : Uint; K : Positive) return Uint
   with Pre => Value (1) mod 2 = 1 and then K <= BITS;
   --  Variable time relative to K

   function Bit_Vartime (Value : Uint; Amount : Natural) return Boolean
   with Pre => Amount < BITS;
   --  Variable time relative to Amount

   function Equal (A, B : Uint) return Boolean
   with Post => Equal'Result = (for all I in 1 .. N => A (I) = B (I));
   --  Constant time equality check

   procedure CSwap (A, B : in out Uint; C : Const_Choice.Choice)
   with
     Post =>
       (if Const_Choice.To_Bool (C)
        then
          (for all I in 1 .. N => B (I) = A'Old (I) and then A (I) = B'Old (I))
        else
          (for all I in 1 .. N
           => A (I) = A'Old (I) and then B (I) = B'Old (I)));
   --  Constant time swap

   function Cond_Select (A, B : Uint; C : Const_Choice.Choice) return Uint
   with
     Post =>
       (for all I in 1 .. N
        => (if Const_Choice.To_Bool (C) then Cond_Select'Result (I) = B (I)
            else Cond_Select'Result (I) = A (I)));
   --  Constant time select

   function Shl (Value : Uint; Amount : Natural) return Uint
   with Pre => Amount <= BITS;

   function Shl_Limb (Value : Uint; Shift : Natural) return Uint_Carry
   with Pre => Shift < 64;

   function Shl1 (Value : Uint) return Uint_Carry
   with Post => Shl1'Result.Carry in 0 .. 1;

   function Shl1 (Value : Uint) return Uint
   is (Uint_Carry'(Shl1 (Value)).Res);

   function Shr (Value : Uint; Amount : Natural) return Uint
   with Pre => Amount <= BITS;

   function Shr_Limb (Value : Uint; Shift : Natural) return Uint_Carry
   with Pre => Shift < 64;

   function Shr1 (Value : Uint) return Uint_Carry
   with Post => Shr1'Result.Carry in 0 .. 1;

   function Shr1 (Value : Uint) return Uint
   is (Uint_Carry'(Shr1 (Value)).Res);

   function Leading_Zeros (Value : Uint) return Natural
   with
     Post =>
       Leading_Zeros'Result <= BITS
       and then (for all I in N - Leading_Zeros'Result / 64 + 1 .. N
                 => Value (I) = 0)
       and then (Leading_Zeros'Result = BITS
                 or else Value (N - Leading_Zeros'Result / 64) /= 0);

end Bigints.Uints;
