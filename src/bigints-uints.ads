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

   function Is_Hex (C : Character) return Boolean
   is ((C in '0' .. '9' or else C in 'a' .. 'f' or else C in 'A' .. 'F'))
   with Ghost;

   function From_Hex (Value : String) return Uint
   with
     Pre =>
       (Value'Length > 0
        and then Value'Length <= 16 * N
        and then (for all I in Value'Range
                  => Value (I) = '_' or else Is_Hex (Value (I))));
   --  Parse a big-endian (most significant digit first) hexadecimal string
   --  into a Uint

   function Concat (Lo, Hi : Uint) return Wide_Uint
   with
     Post =>
       (for all I in 1 .. N => Concat'Result (I) = Lo (I))
       and then (for all I in 1 .. N => Concat'Result (N + I) = Hi (I));
   --  Concatenate two Uints into a Wide_Uint, the first one becoming
   --  the lower limbs

   function Truncate (A : Wide_Uint) return Uint
   with Post => (for all I in 1 .. N => A (I) = Truncate'Result (I));
   --  Return the lower limbs as a Uint

   function Truncate_Upper (A : Wide_Uint) return Uint
   with Post => (for all I in 1 .. N => A (N + I) = Truncate_Upper'Result (I));
   --  Return the higher limbs as a Uint

   function Neg (A : Uint) return Uint;
   --  Computes wrapping negation

   function Add_Carry (A, B : Uint; Carry : U64) return Uint_Carry
   with Post => Add_Carry'Result.Carry in 0 .. 2;
   --  Computes `A + B + Carry`, returning the result along with the new carry

   function Sub_Borrow (A, B : Uint; Borrow : U64) return Uint_Carry
   with
     Pre => Borrow = 0 or else Borrow = U64'Last,
     Post =>
       Sub_Borrow'Result.Carry = 0 or else Sub_Borrow'Result.Carry = U64'Last;
   --  Computes `A - (B + Borrow)`, returning the result along with the new
   --  borrow

   function Mul_Wide (A, B : Uint) return Wide_Uint;
   --  Compute A * B, widening the result

   function Mul_Limb (A : Uint; B : U64) return Uint_Carry;
   --  Compute A * B, returning the result and the carry

   function Div_Rem_Limb_With_Reciprocal
     (U : Uint; Re : Primitives.Recip) return Quotient_Rem;
   function Rem_Limb_With_Reciprocal
     (U : Uint; Re : Primitives.Recip) return U64;
   function Rem_Limb_With_Reciprocal_Wide
     (Lo, Hi : Uint; Re : Primitives.Recip) return U64;
   function Div_Rem_Limb (Value : Uint; Rhs : U64) return Quotient_Rem
   with Pre => Rhs /= 0;
   function Rem_Limb (Value : Uint; Rhs : U64) return U64
   with Pre => Rhs /= 0;

   procedure Impl_Div_Rem
     (Lhs, Rhs            : Uint;
      Quotient, Remainder : out Uint)
      --  Implement constant time division
   with Pre => (for some I in 1 .. N => Rhs (I) /= 0);

   function "not" (A : Uint) return Uint
   with Inline, Post => (for all I in 1 .. N => "not"'Result (I) = not A (I));
   --  Compute the bitwise NOT, ~A

   function "and" (A, B : Uint) return Uint
   with
     Inline,
     Post => (for all I in 1 .. N => "and"'Result (I) = (A (I) and B (I)));
   --  Compute the bitwise AND, A & B

   function "and" (A : Uint; B : U64) return Uint
   with
     Inline,
     Post => (for all I in 1 .. N => "and"'Result (I) = (A (I) and B));
   --  Compute the bitwise AND of every limb of A with B

   function "or" (A, B : Uint) return Uint
   with
     Inline,
     Post => (for all I in 1 .. N => "or"'Result (I) = (A (I) or B (I)));
   --  Compute the bitwise OR, A | B

   function "xor" (A, B : Uint) return Uint
   with
     Inline,
     Post => (for all I in 1 .. N => "xor"'Result (I) = (A (I) xor B (I)));
   --  Compute the bitwise XOR, A ^ B

   function "+" (A, B : Uint) return Uint;
   --  Compute A + B, wrapping at the type boundary

   function "-" (A : Uint) return Uint renames Neg;
   --  Compute -A, wrapping at the type boundary

   function "-" (A, B : Uint) return Uint;
   --  Compute A - B, wrapping at the type boundary

   function "*" (A, B : Uint) return Uint;
   --  Compute A * B, wrapping at the type boundary

   function "/" (A, B : Uint) return Uint
   with Pre => (for some I in 1 .. N => B (I) /= 0);
   --  Compute A / B for B != 0

   function "mod" (A, B : Uint) return Uint
   with Pre => (for some I in 1 .. N => B (I) /= 0);
   --  Compute A % B for B != 0

   function Inv_Mod2k_Vartime (Value : Uint; K : Positive) return Uint
   with Pre => Value (1) mod 2 = 1 and then K <= BITS;
   --  Variable time relative to K

   function Bit_Vartime (Value : Uint; Amount : Natural) return Boolean
   with Pre => Amount < BITS;
   --  Variable time relative to Amount

   overriding
   function "=" (A, B : Uint) return Boolean
   with Inline, Post => "="'Result = (for all I in 1 .. N => A (I) = B (I));
   --  Constant time equality check

   overriding
   function "<" (A, B : Uint) return Boolean
   with Inline;

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

   function Shl_Vartime (Value : Uint; Amount : Natural) return Uint
   with Pre => Amount <= BITS;
   --  Shift integer left by `Amount` bits in variable time with Amount

   function Shl (Value : Uint; Amount : Natural) return Uint
   with Pre => Amount <= BITS;
   --  Shift integer left by `Amount` bits

   function Shl_Limb (Value : Uint; Shift : Natural) return Uint_Carry
   with Pre => Shift < 64;
   --  Shift integer left by `Amount` bits, where `Amount` fits in a single
   --  limb. Return the shifted integer and a carry

   function Shl1 (Value : Uint) return Uint_Carry
   with Post => Shl1'Result.Carry in 0 .. 1;
   --  Shift integer left by one bit. Return the shifted integer and a carry

   function Shl1 (Value : Uint) return Uint
   is (Uint_Carry'(Shl1 (Value)).Res);
   --  Shift integer left by one bit.

   function Shr_Vartime (Value : Uint; Amount : Natural) return Uint
   with Pre => Amount <= BITS;
   --  Shift integer right by `Amount` bits in variable time with Amount

   function Shr (Value : Uint; Amount : Natural) return Uint
   with Pre => Amount <= BITS;
   --  Shift integer right by `Amount` bits

   function Shr_Limb (Value : Uint; Shift : Natural) return Uint_Carry
   with Pre => Shift < 64;
   --  Shift integer right by `Amount` bits, where `Amount` fits in a single
   --  limb. Return the shifted integer and a carry

   function Shr1 (Value : Uint) return Uint_Carry
   with Post => Shr1'Result.Carry in 0 .. 1;
   --  Shift integer right by one bit. Return the shifted integer and a carry

   function Shr1 (Value : Uint) return Uint
   is (Uint_Carry'(Shr1 (Value)).Res);
   --  Shift integer right by one bit

   function Leading_Zeros (Value : Uint) return Natural
   with
     Post =>
       Leading_Zeros'Result <= BITS
       and then (for all I in N - Leading_Zeros'Result / 64 + 1 .. N
                 => Value (I) = 0)
       and then (Leading_Zeros'Result = BITS
                 or else Value (N - Leading_Zeros'Result / 64) /= 0);
   --  Count the leading zeros of the integer in constant time

end Bigints.Uints;
