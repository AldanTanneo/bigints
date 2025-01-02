with Bigints.Primitives; use Bigints.Primitives;

package body Bigints.Uints
  with SPARK_Mode => On
is

   function From_Hex (Value : String) return Uint is
      Res : Uint := ZERO;
      I   : Natural := 0;
      V   : U64;
   begin
      for C in reverse Value'Range loop
         pragma Loop_Invariant (I <= Value'Last - C);

         case Value (C) is
            when '0' .. '9' =>
               V := Character'Pos (Value (C)) - Character'Pos ('0');

            when 'a' .. 'f' =>
               V := Character'Pos (Value (C)) - Character'Pos ('a') + 10;

            when 'A' .. 'F' =>
               V := Character'Pos (Value (C)) - Character'Pos ('A') + 10;

            when others =>
               V := 0;
         end case;
         Res (I / 16 + 1) :=
           Res (I / 16 + 1) or Shift_Left (V, 4 * (I mod 16));
         I := I + Boolean'Pos (Value (C) /= '_');
      end loop;
      return Res;
   end From_Hex;

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
         Tmp := Sub_Borrow (A (I), B (I), C);
         Res (I) := W;
         pragma Loop_Invariant (Res (1 .. I)'Initialized);
         pragma Loop_Invariant (C = 0 or else C = U64'Last);
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

   function Div_Rem_Limb (Value : Uint; Rhs : U64) return Quotient_Rem is
   begin
      return Div_Rem_Limb_With_Reciprocal (Value, Create_Recip (Rhs));
   end Div_Rem_Limb;

   function Rem_Limb (Value : Uint; Rhs : U64) return U64 is
   begin
      return Rem_Limb_With_Reciprocal (Value, Create_Recip (Rhs));
   end Rem_Limb;

   procedure Impl_Div_Rem (Lhs, Rhs : Uint; Quotient, Remainder : out Uint) is
      use Const_Choice;

      Dshift : constant Natural := Leading_Zeros (Rhs);
      Dbits  : constant Natural := BITS - Dshift;
      pragma Assert (Dbits > 0);
      Dwords : constant Natural := (Dbits + 63) / 64; --  div ceiling
      Lshift : constant Natural := (64 - (Dbits mod 64)) mod 64;

      Y : Uint := Shl (Rhs, Dshift);
      pragma Assume (Y (N) >= 2 ** 63); --  Shifting left by leading zeros.

      Lhs_Shifted : constant Uint_Carry := Shl_Limb (Lhs, Lshift);
      X           : Uint := Lhs_Shifted.Res;
      X_Hi        : U64 := Lhs_Shifted.Carry;
      X_Lo        : U64 := X (N);
      Re          : constant Recip := Create_Recip (Y (N));
   begin

      for Xi in reverse 2 .. N loop
         declare
            Done          : constant Choice :=
              Choice_From_Condition (Xi < Dwords);
            Ct_Borrow     : Choice;
            Q             : constant U64 :=
              Div3By2 (X_Hi, X_Lo, X (Xi - 1), Re, Y (N - 1));
            Quo           : U64 := Cond_Select (Q, 0, Done);
            Carry, Borrow : U64 := 0;
            Tmp           : Tuple;
         begin

            for I in 1 .. Xi loop
               Tmp := Mac (0, Y (N - Xi + I), Quo, Carry);
               Carry := Tmp.Snd;
               Tmp := Sub_Borrow (X (I), Tmp.Fst, Borrow);
               X (I) := Tmp.Fst;
               Borrow := Tmp.Snd;
               pragma Loop_Invariant (Borrow = 0 or else Borrow = U64'Last);
            end loop;
            Tmp := Sub_Borrow (X_Hi, Carry, Borrow);
            Borrow := Tmp.Snd;

            Ct_Borrow := Choice_From_Condition (Borrow /= 0);
            Carry := 0;
            for I in 1 .. Xi loop
               Tmp :=
                 Add_Carry
                   (X (I), Cond_Select (0, Y (N - Xi + I), Ct_Borrow), Carry);
               X (I) := Tmp.Fst;
               Carry := Tmp.Snd;
            end loop;
            Quo := Cond_Select (Quo, Saturating_Sub (Quo, 1), Ct_Borrow);

            X_Hi := Cond_Select (X (Xi), X_Hi, Done);
            X (Xi) := Cond_Select (Quo, X (Xi), Done);
            X_Lo := Cond_Select (X (Xi - 1), X_Lo, Done);
         end;
      end loop;

      declare
         Limb_Div : constant Choice := Choice_From_Condition (Dwords = 1);
         --  X_Hi_Adjusted : constant U64 := Cond_Select (0, X_Hi, Limb_Div);
         QuoRem2  : constant Tuple := Div2By1 (X_Hi, X_Lo, Re);
      begin
         X (1) := Cond_Select (X (1), QuoRem2.Fst, Limb_Div);
         Y (1) := Cond_Select (X (1), QuoRem2.Snd, Limb_Div);
         for I in 2 .. N loop
            Y (I) :=
              Cond_Select (0, X (I), Choice_From_Condition (I <= Dwords));
            Y (I) :=
              Cond_Select (Y (I), X_Hi, Choice_From_Condition (I = Dwords));
         end loop;
      end;
      Quotient := Shr (X, (Dwords - 1) * 64);
      Remainder := Shr (Y, Lshift);
   end Impl_Div_Rem;

   function "/" (A, B : Uint) return Uint is
      Quotient, Remainder : Uint;
   begin
      Impl_Div_Rem (A, B, Quotient, Remainder);
      pragma Unreferenced (Remainder);
      return Quotient;
   end "/";

   function "mod" (A, B : Uint) return Uint is
      Quotient, Remainder : Uint;
   begin
      Impl_Div_Rem (A, B, Quotient, Remainder);
      pragma Unreferenced (Quotient);
      return Remainder;
   end "mod";

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

   overriding function "=" (A, B : Uint) return Boolean is
      Res : U64 := 0;
   begin
      for I in 1 .. N loop
         Res := Res or (A (I) xor B (I));
         pragma
           Loop_Invariant ((Res = 0) = (for all J in 1 .. I => A (J) = B (J)));
      end loop;
      return Res = 0;
   end "=";

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

   function Shl_Vartime (Value : Uint; Amount : Natural) return Uint is
      Res                : Uint := ZERO;
      Shift_Num          : constant Natural := Amount / 64;
      Shift_Rem          : constant Natural := Amount mod 64;
      Carry              : U64 := 0;
      New_Carry, Shifted : U64;
   begin
      for I in Shift_Num + 1 .. N loop
         Res (I) := Value (I - Shift_Num);
      end loop;

      if Shift_Rem = 0 then
         return Res;
      end if;

      for I in Shift_Num + 1 .. N loop
         Shifted := Shift_Left (Res (I), Shift_Rem);
         New_Carry := Shift_Right (Res (I), 64 - Shift_Rem);
         Res (I) := Shifted or Carry;
         Carry := New_Carry;
      end loop;

      return Res;
   end Shl_Vartime;

   function Shl (Value : Uint; Amount : Natural) return Uint is
      use Const_Choice;

      Shift_Bits : constant Positive := 64 - Leading_Zeros (U64 (BITS - 1));
      Res : Uint := Value;
      Cond : Choice;
   begin
      for I in 1 .. Shift_Bits loop
         Cond := Choice_From_Condition ((Amount / 2 ** (I - 1)) mod 2 /= 0);
         Res := Cond_Select (Res, Shl_Vartime (Res, 2 ** (I - 1)), Cond);
      end loop;
      return Res;
   end Shl;

   function Shl_Limb (Value : Uint; Shift : Natural) return Uint_Carry is
      use Const_Choice;

      Shift_Is_Zero : constant Choice := Choice_From_Condition (Shift = 0);
      Res           : Uint
      with Relaxed_Initialization;
      Carry         : U64 := 0;
   begin
      for I in 1 .. N loop
         Res (I) := Shift_Left (Value (I), Shift) or Carry;
         Carry := Shift_Right (Value (I), (64 - Shift) mod 64);
         Carry := Cond_Select (Carry, 0, Shift_Is_Zero);
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

   function Shr_Vartime (Value : Uint; Amount : Natural) return Uint is

      Res                : Uint := ZERO;
      Shift_Num          : constant Natural := Amount / 64;
      Shift_Rem          : constant Natural := Amount mod 64;
      Carry              : U64 := 0;
      New_Carry, Shifted : U64;
   begin
      for I in 1 .. N - Shift_Num loop
         Res (I) := Value (I + Shift_Num);
      end loop;

      if Shift_Rem = 0 then
         return Res;
      end if;

      for I in reverse 1 .. N - Shift_Num loop
         Shifted := Shift_Right (Res (I), Shift_Rem);
         New_Carry := Shift_Left (Res (I), 64 - Shift_Rem);
         Res (I) := Shifted or Carry;
         Carry := New_Carry;
      end loop;

      return Res;
   end Shr_Vartime;

   function Shr (Value : Uint; Amount : Natural) return Uint is
      use Const_Choice;

      Shift_Bits : constant Positive := 64 - Leading_Zeros (U64 (BITS - 1));
      Res : Uint := Value;
      Cond : Choice;
   begin
      for I in 1 .. Shift_Bits loop
         Cond := Choice_From_Condition ((Amount / 2 ** (I - 1)) mod 2 /= 0);
         Res := Cond_Select (Res, Shr_Vartime (Res, 2 ** (I - 1)), Cond);
      end loop;
      return Res;
   end Shr;

   function Shr_Limb (Value : Uint; Shift : Natural) return Uint_Carry is
      use Const_Choice;

      Shift_Is_Zero : constant Choice := Choice_From_Condition (Shift = 0);
      Res           : Uint
      with Relaxed_Initialization;
      Carry         : U64 := 0;
   begin
      for I in reverse 1 .. N loop
         Res (I) := Shift_Right (Value (I), Shift) or Carry;
         Carry := Shift_Left (Value (I), (64 - Shift) mod 64);
         Carry := Cond_Select (Carry, 0, Shift_Is_Zero);
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

   function Leading_Zeros (Value : Uint) return Natural is
      use Const_Choice;

      Limb, Z                  : U64;
      Count                    : U64 := 0;
      Limb_Is_Zero             : Choice;
      Non_Zero_Not_Encountered : Choice := Choice_From_Condition (True);

      Non_Zero_Index : Natural := N
      with Ghost;
   begin

      for I in reverse 1 .. N loop
         pragma
           Loop_Invariant
             (if To_Bool (Non_Zero_Not_Encountered)
                then
                  (for all J in I + 1 .. N => Value (J) = 0)
                  and then Count = 64 * U64 (N - I)
                else
                  Non_Zero_Index in I + 1 .. N
                  and then Value (Non_Zero_Index) /= 0
                  and then (for all J in Non_Zero_Index + 1 .. N
                            => Value (J) = 0)
                  and then Count / 64 = U64 (N - Non_Zero_Index));

         Limb := Value (I);
         Limb_Is_Zero := Choice_From_Condition (Limb = 0);
         Limb := Cond_Select (Limb, 1, Limb_Is_Zero);
         Z := Cond_Select (U64 (Leading_Zeros (Limb)), 64, Limb_Is_Zero);

         Count := Count + Cond_Select (0, Z, Non_Zero_Not_Encountered);

         Non_Zero_Index :=
           (if To_Bool (Non_Zero_Not_Encountered)
              and then not To_Bool (Limb_Is_Zero)
            then I
            else Non_Zero_Index);

         Non_Zero_Not_Encountered := Non_Zero_Not_Encountered and Limb_Is_Zero;

      end loop;

      return Natural (Count);
   end Leading_Zeros;

end Bigints.Uints;
