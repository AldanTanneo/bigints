with Bigints.Primitives; use Bigints.Primitives;

package body Bigints.Uints
  with SPARK_Mode => On
is

   function Concat (Lo, Hi : Uint) return Wide_Uint is
      Res : Wide_Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := Lo (I);
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma Loop_Invariant (for all J in 1 .. I => Res (J) = Lo (J));
      end loop;
      for I in 1 .. N loop
         Res (N + I) := Hi (I);
         pragma Loop_Invariant (Res (1 .. N + I)'Initialized);
         pragma
           Loop_Invariant
             ((for all J in 1 .. N => Res (J) = Lo (J))
                and then (for all J in 1 .. I => Res (N + J) = Hi (J)));
      end loop;
      return Res;
   end Concat;

   function Truncate (A : Wide_Uint) return Uint is
      Res : Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := A (I);
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma Loop_Invariant (for all J in 1 .. I => Res (J) = A (J));
      end loop;
      return Res;
   end Truncate;

   function Truncate_Upper (A : Wide_Uint) return Uint is
      Res : Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := A (N + I);
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma Loop_Invariant (for all J in 1 .. I => Res (J) = A (N + J));
      end loop;
      return Res;
   end Truncate_Upper;

   function Neg (A : Uint) return Uint is
      Ret   : Uint
      with Relaxed_Initialization;
      Carry : U128 := 1;
      R     : U128;
   begin
      for I in 1 .. N loop
         R := (not U128 (A (I))) + Carry;
         Ret (I) := U64 (R and U128 (U64'Last));
         Carry := Shift_Right (R, 64);
         pragma Loop_Invariant (Ret (1 .. I)'Initialized);
      end loop;
      return Ret;
   end Neg;

   function "not" (A : Uint) return Uint is
      Res : Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := not A (I);
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma Loop_Invariant (for all J in 1 .. I => Res (J) = not A (J));
      end loop;
      return Res;
   end "not";

   function "and" (A, B : Uint) return Uint is
      Res : Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := A (I) and B (I);
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma
           Loop_Invariant (for all J in 1 .. I => Res (J) = (A (J) and B (J)));
      end loop;
      return Res;
   end "and";

   function "and" (A : Uint; B : U64) return Uint is
      Res : Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := A (I) and B;
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma
           Loop_Invariant (for all J in 1 .. I => Res (J) = (A (J) and B));
      end loop;
      return Res;
   end "and";

   function "or" (A, B : Uint) return Uint is
      Res : Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := A (I) or B (I);
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma
           Loop_Invariant (for all J in 1 .. I => Res (J) = (A (J) or B (J)));
      end loop;
      return Res;
   end "or";

   function "xor" (A, B : Uint) return Uint is
      Res : Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := A (I) xor B (I);
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma
           Loop_Invariant (for all J in 1 .. I => Res (J) = (A (J) xor B (J)));
      end loop;
      return Res;
   end "xor";

   function Add_Carry (A, B : Uint; Carry : U64) return Uint_Carry is
      Res : Uint
      with Relaxed_Initialization;
      Tmp : Primitives.Tuple := (0, Carry);
      W   : U64 renames Tmp.Fst;
      C   : U64 renames Tmp.Snd;
   begin
      for I in 1 .. N loop
         Tmp := Primitives.Add_Carry (A (I), B (I), C);
         Res (I) := W;
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
      end loop;
      return (Res, C);
   end Add_Carry;

   function "+" (A, B : Uint) return Uint is
   begin
      return Add_Carry (A, B, 0).Res;
   end "+";

   function Sub_Borrow (A, B : Uint; Borrow : U64) return Uint_Carry is
      Res : Uint
      with Relaxed_Initialization;
      Tmp : Primitives.Tuple := (0, Borrow);
      W   : U64 renames Tmp.Fst;
      C   : U64 renames Tmp.Snd;
   begin
      for I in 1 .. N loop
         Tmp := Primitives.Sub_Borrow (A (I), B (I), C);
         Res (I) := W;
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
      end loop;
      return (Res, C);
   end Sub_Borrow;

   function "-" (A, B : Uint) return Uint is
   begin
      return Sub_Borrow (A, B, 0).Res;
   end "-";

   procedure Impl_Schoolbook_Multiplication
     (Value : out Wide_Uint; Lhs, Rhs : Uint)
   is
      SumCarry : Primitives.Tuple;
      Sum      : U64 renames SumCarry.Fst;
      Carry    : U64 renames SumCarry.Snd;
      Xi       : U64;
   begin
      Value := [others => 0];
      for I in 1 .. N loop
         Carry := 0;
         Xi := Lhs (I);
         for J in 1 .. N loop
            declare
               K : constant Natural := I + J - 1;
            begin
               SumCarry := Mac (Value (K), Xi, Rhs (J), Carry);
               Value (K) := Sum;
            end;
         end loop;
         Value (I + N) := Carry;
      end loop;
   end Impl_Schoolbook_Multiplication;

   function Mul_Wide (A, B : Uint) return Wide_Uint is
      Res : Wide_Uint;
   begin
      Impl_Schoolbook_Multiplication (Res, A, B);
      return Res;
   end Mul_Wide;

   function "*" (A, B : Uint) return Uint is
   begin
      return Truncate (Mul_Wide (A, B));
   end "*";

   function Div_Rem_Limb_With_Reciprocal
     (U : Uint; Re : Recip) return Quotient_Rem
   is
      UShiftHi : constant Uint_Carry := Shl_Limb (U, Get_Shift (Re));
      R        : U64 := UShiftHi.Carry;
      Q        : Uint
      with Relaxed_Initialization;
      QjRj     : Tuple;
   begin
      for J in reverse 1 .. N loop
         QjRj := Div2By1 (R, UShiftHi.Res (J), Re);
         Q (J) := QjRj.Fst;
         R := QjRj.Snd;
         pragma Loop_Invariant (Q (J .. N)'Initialized);
      end loop;
      return (Q, Shift_Right (R, Get_Shift (Re)));
   end Div_Rem_Limb_With_Reciprocal;

   function Rem_Limb_With_Reciprocal (U : Uint; Re : Recip) return U64 is
      UShiftHi : constant Uint_Carry := Shl_Limb (U, Get_Shift (Re));
      R        : U64 := UShiftHi.Carry;
      QjRj     : Tuple;
   begin
      for J in reverse 1 .. N loop
         QjRj := Div2By1 (R, UShiftHi.Res (J), Re);
         R := QjRj.Snd;
      end loop;
      return Shift_Right (R, Get_Shift (Re));
   end Rem_Limb_With_Reciprocal;

   function Rem_Limb_With_Reciprocal_Wide
     (Lo, Hi : Uint; Re : Recip) return U64
   is
      LoShiftCarry : constant Uint_Carry := Shl_Limb (Lo, Get_Shift (Re));
      HiShiftXhi   : Uint_Carry := Shl_Limb (Hi, Get_Shift (Re));
      Hi_Shifted   : Uint renames HiShiftXhi.Res;
      R            : U64 renames HiShiftXhi.Carry;
      T            : Tuple;
   begin
      Hi_Shifted (1) := Hi_Shifted (1) or LoShiftCarry.Carry;
      for J in reverse 1 .. N loop
         T := Div2By1 (R, Hi_Shifted (J), Re);
         R := T.Snd;
      end loop;
      for J in reverse 1 .. N loop
         T := Div2By1 (R, LoShiftCarry.Res (J), Re);
         R := T.Snd;
      end loop;
      return Shift_Right (R, Get_Shift (Re));
   end Rem_Limb_With_Reciprocal_Wide;

   function Div_Rem_Limb (Value : Uint; Rhs : U64) return Quotient_Rem
   with Pre => Rhs /= 0
   is
   begin
      return Div_Rem_Limb_With_Reciprocal (Value, Create_Recip (Rhs));
   end Div_Rem_Limb;

   function Rem_Limb (Value : Uint; Rhs : U64) return U64 with Pre => Rhs /= 0
   is
   begin
      return Rem_Limb_With_Reciprocal (Value, Create_Recip (Rhs));
   end Rem_Limb;

   procedure Impl_Div_Rem (Lhs, Rhs : Uint; Quotient, Remainder : out Uint)
   with Pre => (for some I in 1 .. N => Rhs (I) /= 0)
   is

   begin
      Quotient := Lhs;
      Remainder := Rhs;
   end Impl_Div_Rem;

   function Inv_Mod2k_Vartime (Value : Uint; K : Positive) return Uint is
      X : Uint := ZERO;
      B : Uint := ONE;
   begin
      for I in 1 .. K loop
         declare
            Xi : constant U64 := B (1) and 1;
         begin
            B :=
              Cond_Select
                (B, B - Value, Const_Choice.Choice_From_Condition (Xi = 1));
            B := Shr1 (B);
            X := X or Shl (Uint'[1 => Xi, others => 0], I - 1);
         end;
      end loop;
      return X;
   end Inv_Mod2k_Vartime;

   function Bit_Vartime (Value : Uint; Amount : Natural) return Boolean is
      Limb : constant Natural := Amount / 64;
      Bit  : constant Natural := Amount mod 64;
   begin
      return Boolean'Val (Shift_Right (Value (Limb + 1), Bit) and 1);
   end Bit_Vartime;

   function Equal (A, B : Uint) return Boolean is
      Res : U64 := 0;
   begin
      for I in 1 .. N loop
         Res := Res or (A (I) xor B (I));
         pragma
           Loop_Invariant ((Res = 0) = (for all J in 1 .. I => A (J) = B (J)));
      end loop;
      return Res = 0;
   end Equal;

   procedure CSwap (A, B : in out Uint; C : Const_Choice.Choice) is
      use Const_Choice;
   begin
      pragma Warnings (GNATProve, Off, "pragma * ignored (not yet supported)");

      for I in 1 .. N loop
         declare
            Tmp : U64 := Cond_Select (A (I), B (I), C);
         begin
            B (I) := Cond_Select (B (I), A (I), C);
            A (I) := Tmp;
         end;

         pragma Loop_Optimize (Ivdep);
         pragma
           Loop_Invariant
             (for all J in 1 .. I
              => (if To_Bool (C)
                  then
                    A (J) = B'Loop_Entry (J) and then B (J) = A'Loop_Entry (J)
                  else
                    A (J) = A'Loop_Entry (J)
                    and then B (J) = B'Loop_Entry (J)));
      end loop;
   end CSwap;

   function Cond_Select (A, B : Uint; C : Const_Choice.Choice) return Uint is
      use Const_Choice;
      Res : Uint
      with Relaxed_Initialization;
   begin
      for I in 1 .. N loop
         Res (I) := Cond_Select (A (I), B (I), C);

         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma
           Loop_Invariant
             (for all J in 1 .. I
              => (if To_Bool (C) then Res (J) = B (J) else Res (J) = A (J)));
      end loop;
      return Res;
   end Cond_Select;

   function Shl (Value : Uint; Amount : Natural) return Uint is
      Res                : Uint := ZERO;
      Shift_Num          : constant Natural := Amount / 64;
      Shift_Rem          : constant Natural := Amount mod 64;
      Carry              : U64 := 0;
      New_Carry, Shifted : U64;
   begin
      for I in Shift_Num + 1 .. N loop
         Res (I) := Value (I - Shift_Num);
      end loop;

      for I in Shift_Num + 1 .. N loop
         Shifted := Shift_Left (Res (I), Shift_Rem);
         New_Carry := Shift_Right (Res (I), (64 - Shift_Rem) mod 64);
         Res (I) := Shifted or Carry;
         Carry := New_Carry;
      end loop;

      return Res;
   end Shl;

   function Shl_Limb (Value : Uint; Shift : Natural) return Uint_Carry is
      Res   : Uint
      with Relaxed_Initialization;
      Carry : U64 := 0;
   begin
      for I in 1 .. N loop
         Res (I) := Shift_Left (Value (I), Shift) or Carry;
         Carry := Shift_Right (Value (I), (64 - Shift) mod 64);
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
      end loop;
      return (Res, Carry);
   end Shl_Limb;

   function Shl1 (Value : Uint) return Uint_Carry is
      Res   : Uint
      with Relaxed_Initialization;
      Carry : U64 := 0;
   begin
      for I in 1 .. N loop
         Res (I) := Shift_Left (Value (I), 1) or Carry;
         Carry := Shift_Right (Value (I), 63);
         pragma
           Loop_Invariant (Res (1 .. I)'Initialized and then Carry in 0 .. 1);
      end loop;
      return (Res, Carry);
   end Shl1;

   function Shr (Value : Uint; Amount : Natural) return Uint is
      Res                : Uint := ZERO;
      Shift_Num          : constant Natural := Amount / 64;
      Shift_Rem          : constant Natural := Amount mod 64;
      Carry              : U64 := 0;
      New_Carry, Shifted : U64;
   begin
      for I in 1 .. N - Shift_Num loop
         Res (I) := Value (I + Shift_Num);
      end loop;

      for I in reverse 1 .. N - Shift_Num loop
         Shifted := Shift_Right (Res (I), Shift_Rem);
         New_Carry := Shift_Left (Res (I), (64 - Shift_Rem) mod 64);
         Res (I) := Shifted or Carry;
         Carry := New_Carry;
      end loop;

      return Res;
   end Shr;

   function Shr_Limb (Value : Uint; Shift : Natural) return Uint_Carry is
      Res   : Uint
      with Relaxed_Initialization;
      Carry : U64 := 0;
   begin
      for I in reverse 1 .. N loop
         Res (I) := Shift_Right (Value (I), Shift) or Carry;
         Carry := Shift_Left (Value (I), (64 - Shift) mod 64);
         pragma Loop_Invariant (Res (I .. N)'Initialized);
      end loop;
      return (Res, Carry);
   end Shr_Limb;

   function Shr1 (Value : Uint) return Uint_Carry is
      Res   : Uint
      with Relaxed_Initialization;
      Carry : U64 := 0;
   begin
      for I in reverse 1 .. N loop
         Res (I) := Shift_Right (Value (I), 1) or Shift_Left (Carry, 63);
         Carry := Value (I) and 1;
         pragma
           Loop_Invariant (Res (I .. N)'Initialized and then Carry in 0 .. 1);
      end loop;
      return (Res, Carry);
   end Shr1;

end Bigints.Uints;
