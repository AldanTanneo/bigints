with Ada.Numerics.Big_Numbers.Big_Integers_Ghost;
use Ada.Numerics.Big_Numbers.Big_Integers_Ghost;

pragma Warnings (GNATProve, Off, "subprogram * has no effect");

procedure Addition_Proof (N : Positive) with
  SPARK_Mode => On, Pre => N * 64 <= Integer'Last
is
   BITS : constant Positive := N * 64;

   --  Types

   type U64 is mod 2**64;
   for U64'Size use 64;

   type Tuple is record
      Fst, Snd : U64;
   end record;

   type Uint is array (1 .. N) of U64;

   type Uint_Carry is record
      Res   : Uint;
      Carry : U64;
   end record;

   ZERO : constant Uint := [others => 0];

   BASE : constant := 2**64;

   --  Ghost code

   package Conv is new Unsigned_Conversions (U64);

   function To_BI (X : U64) return Big_Integer is
     (Conv.To_Big_Integer (X)) with
     Ghost, Post => To_BI'Result = Conv.To_Big_Integer (X);

   function Partial_Conv (A : Uint; I : Natural) return Big_Integer is
     (if I = 0 then 0
      else (To_BI (A (I)) * BASE**(I - 1) + Partial_Conv (A, I - 1))) with
     Ghost, Pre         => I <= N and then I * 64 <= BITS,
     Post               =>
      Partial_Conv'Result =
      (if I = 0 then 0
       else (To_BI (A (I)) * BASE**(I - 1) + Partial_Conv (A, I - 1))),
     Subprogram_Variant => (Decreases => I);

   procedure Equal_Conv (A, B : Uint; I : Natural) with
     Ghost, Always_Terminates, Subprogram_Variant => (Decreases => I),
     Pre => I <= N and then (for all J in 1 .. I => A (J) = B (J)),
     Post => Partial_Conv (A, I) = Partial_Conv (B, I);

   function To_BI (A : Uint) return Big_Integer is (Partial_Conv (A, N)) with
     Ghost;

   --  Functions

   function Add_Carry (X, Y, Carry : U64) return Tuple with
     Inline_Always,
     Post =>
      Add_Carry'Result.Snd in 0 .. 2
      and then To_BI (X) + To_BI (Y) + To_BI (Carry) =
        BASE * To_BI (Add_Carry'Result.Snd) + To_BI (Add_Carry'Result.Fst);
   --  Computes `X + Y + Carry`, returning the result along with the new carry
   --  (0, 1, or 2).

   function Add_Carry (A, B : Uint; Carry : U64) return Uint_Carry with
     Post =>
      Add_Carry'Result.Carry in 0 .. 2
      and then
        To_BI (Add_Carry'Result.Carry) * BASE**N +
          To_BI (Add_Carry'Result.Res) =
        To_BI (A) + To_BI (B) + To_BI (Carry);
   --  Computes `A + B + Carry`, returning the result along with the new carry

   --  Function bodies

   function Add_Carry (X, Y, Carry : U64) return Tuple is
      type U128 is mod 2**128;
      for U128'Size use 128;

      function Shift_Right (Value : U128; Amount : Natural) return U128 with
        Pre    => Amount < 128, Post => Shift_Right'Result = Value / 2**Amount,
        Global => null, Import, Convention => Intrinsic, Inline_Always;

      function Low (X : U128) return U64 with
        Inline, Post => U128 (Low'Result) = X mod (2**64)
      is
         Y : constant U128 := X and U128 (U64'Last);
      begin
         return U64 (Y);
      end Low;

      function High (X : U128) return U64 with
        Inline, Post => U128 (High'Result) = X / (2**64)
      is
         Y : constant U128 := Shift_Right (X, 64);
      begin
         return U64 (Y);
      end High;

      A   : constant U128 := U128 (X);
      B   : constant U128 := U128 (Y);
      C   : constant U128 := U128 (Carry);
      Ret : constant U128 := A + B + C;
   begin
      return (Low (Ret), High (Ret));
   end Add_Carry;

   procedure Equal_Conv (A, B : Uint; I : Natural) is
   begin
      if I = 0 then
         return;
      end if;
      Equal_Conv (A, B, I - 1);
   end Equal_Conv;

   function Add_Carry (A, B : Uint; Carry : U64) return Uint_Carry is
      Res : Uint  := ZERO;
      Tmp : Tuple := (0, Carry);
      W   : U64 renames Tmp.Fst;
      C   : U64 renames Tmp.Snd;

      procedure Factorize (A, B : Big_Integer; I : Positive) with
        Ghost,
        Post =>
         A * BASE**I + B * BASE**(I - 1) = (A * BASE + B) * BASE**(I - 1);
      procedure Factorize (A, B : Big_Integer; I : Positive) is null;

      procedure Replace (A, B, C : Big_Integer; I : Natural) with
        Ghost, Pre => A = B, Post => A * BASE**I + C = B * BASE**I + C;
      procedure Replace (A, B, C : Big_Integer; I : Natural) is null;

      procedure Expand (A, B, C : Big_Integer; I : Natural) with
        Ghost, Post => (A + B) * BASE**I + C = A * BASE**I + B * BASE**I + C;
      procedure Expand (A, B, C : Big_Integer; I : Natural) is null;

      procedure Replace2 (A, B, C : Big_Integer) with
        Ghost, Pre => A = B, Post => C + A = C + B;
      procedure Replace2 (A, B, C : Big_Integer) is null;

      Old_C   : U64 with
        Ghost;
      Old_Res : Uint with
        Ghost;
   begin
      for I in 1 .. N loop
         Old_C   := C;
         Old_Res := Res;
         pragma Assert
           (To_BI (Old_C) * BASE**(I - 1) + Partial_Conv (Res, I - 1) =
            To_BI (Carry) + Partial_Conv (A, I - 1) + Partial_Conv (B, I - 1));

         Tmp     := Add_Carry (A (I), B (I), C);
         Res (I) := W;

         Equal_Conv (Res, Old_Res, I - 1);

         pragma Assert
           (To_BI (Old_C) * BASE**(I - 1) + Partial_Conv (Res, I - 1) =
            To_BI (Carry) + Partial_Conv (A, I - 1) + Partial_Conv (B, I - 1));

         pragma Assert
           (To_BI (C) * BASE**I + Partial_Conv (Res, I) =
            To_BI (C) * BASE**I + To_BI (Res (I)) * BASE**(I - 1) +
              Partial_Conv (Res, I - 1));

         Factorize (To_BI (C), To_BI (Res (I)), I);
         pragma Assert
           (To_BI (C) * BASE**I + Partial_Conv (Res, I) =
            (To_BI (C) * BASE + To_BI (Res (I))) * BASE**(I - 1) +
              Partial_Conv (Res, I - 1));

         Replace
           (To_BI (C) * BASE + To_BI (Res (I)),
            To_BI (A (I)) + To_BI (B (I)) + To_BI (Old_C),
            Partial_Conv (Res, I - 1), I - 1);
         pragma Assert
           (To_BI (C) * BASE**I + Partial_Conv (Res, I) =
            (To_BI (A (I)) + To_BI (B (I)) + To_BI (Old_C)) * BASE**(I - 1) +
              Partial_Conv (Res, I - 1));

         Expand
           (To_BI (A (I)) + To_BI (B (I)), To_BI (Old_C),
            Partial_Conv (Res, I - 1), I - 1);
         pragma Assert
           (To_BI (C) * BASE**I + Partial_Conv (Res, I) =
            (To_BI (A (I)) + To_BI (B (I))) * BASE**(I - 1) +
              To_BI (Old_C) * BASE**(I - 1) + Partial_Conv (Res, I - 1));

         Replace2
           (To_BI (Old_C) * BASE**(I - 1) + Partial_Conv (Res, I - 1),
            To_BI (Carry) + Partial_Conv (A, I - 1) + Partial_Conv (B, I - 1),
            (To_BI (A (I)) + To_BI (B (I))) * BASE**(I - 1));
         pragma Assert
           (To_BI (C) * BASE**I + Partial_Conv (Res, I) =
            (To_BI (A (I)) + To_BI (B (I))) * BASE**(I - 1) + To_BI (Carry) +
              Partial_Conv (A, I - 1) + Partial_Conv (B, I - 1));

         Expand
           (To_BI (A (I)), To_BI (B (I)),
            To_BI (Carry) + Partial_Conv (A, I - 1) + Partial_Conv (B, I - 1),
            I - 1);
         pragma Assert
           (To_BI (C) * BASE**I + Partial_Conv (Res, I) =
            To_BI (Carry) + To_BI (A (I)) * BASE**(I - 1) +
              Partial_Conv (A, I - 1) + To_BI (B (I)) * BASE**(I - 1) +
              Partial_Conv (B, I - 1));

         Replace2
           (To_BI (B (I)) * BASE**(I - 1) + Partial_Conv (B, I - 1),
            Partial_Conv (B, I),
            To_BI (Carry) + To_BI (A (I)) * BASE**(I - 1) +
            Partial_Conv (A, I - 1));
         Replace2
           (To_BI (A (I)) * BASE**(I - 1) + Partial_Conv (A, I - 1),
            Partial_Conv (A, I), To_BI (Carry) + Partial_Conv (B, I));

         pragma Loop_Invariant (C in 0 .. 2);
         pragma Loop_Invariant
           (To_BI (C) * BASE**I + Partial_Conv (Res, I) =
            To_BI (Carry) + Partial_Conv (A, I) + Partial_Conv (B, I));
      end loop;

      return (Res, C);
   end Add_Carry;

   pragma Unreferenced (Add_Carry);
   pragma Unreferenced (Equal_Conv);

begin
   null;
end Addition_Proof;
