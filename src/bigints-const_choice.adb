with Bigints.Machine_Ints; use Bigints.Machine_Ints;
with System.Machine_Code;

package body Bigints.Const_Choice
  with SPARK_Mode => On
is

   overriding
   function "not" (C : Choice) return Choice
   is (Choice (not U64 (C)));

   overriding
   function "or" (A, B : Choice) return Choice
   is (Choice (U64 (A) or U64 (B)));

   overriding
   function "and" (A, B : Choice) return Choice
   is (Choice (U64 (A) and U64 (B)));

   overriding
   function "xor" (A, B : Choice) return Choice
   is (Choice (U64 (A) xor U64 (B)));

   function Lsb (C : Choice) return U32
   is (U32 (U64 (C) and 1));
   function Lsb (C : Choice) return U64
   is (U64 (C) and 1);
   function Lsb (C : Choice) return U128
   is (U128 (U64 (C) and 1));

   procedure Value_Barrier (V : in out U64)
   with SPARK_Mode => On, Global => null, Always_Terminates, Post => V'Old = V, Inline_Always;

   procedure Value_Barrier (V : in out U64) with SPARK_Mode => Off is
      --  This function is a best-effort attempt to prevent the compiler from
      --  knowing anything about the value of the returned `U64`.
      --
      --  We use the classic value barrier, exploiting the fact that a volatile
      --  inline assembly block that has an output parameter mentioning V
      --  can never be removed by the optimizer, and the optimizer cannot see
      --  inside an assembly block.
   begin
      System.Machine_Code.Asm ("", Volatile => True, Outputs => U64'Asm_Output ("+r", V));
   end Value_Barrier;

   function Choice_From_Bit (Bit : U64) return Choice is
      V : U64 := Bit;
   begin
      Value_Barrier (V);
      return Choice (-V);
   end Choice_From_Bit;

   function Choice_From_Mask (Mask : U64) return Choice is
   begin
      return Choice (Mask);
   end Choice_From_Mask;

   function Generic_Ct_Eq (A, B : T) return Choice is
      X : constant T := A xor B;
      pragma Assert ((X = 0) = (A = B));
      --  if X = 0, X and -X are both eq to 0; otherwise one of them has its
      --  high bit set.
      Y : constant T := Shift_Right (X or (-X), T'Size - 1);
      pragma Assert (if X = 0 then Y = 0 else Y = 1);
   begin
      pragma Assert (Y in 0 .. 1);
      pragma Assert ((U64 (Y) xor 1) in 0 .. 1);
      return Choice_From_Bit (U64 (Y) xor 1);
   end Generic_Ct_Eq;

   function Ct_Eq_U32 is new Generic_Ct_Eq (U32);
   function Ct_Eq (A, B : U32) return Choice renames Ct_Eq_U32;
   function Ct_Eq_U64 is new Generic_Ct_Eq (U64);
   function Ct_Eq (A, B : U64) return Choice renames Ct_Eq_U64;
   function Ct_Eq_U128 is new Generic_Ct_Eq (U128);
   function Ct_Eq (A, B : U128) return Choice renames Ct_Eq_U128;

   function Ct_Ge (A, B : U32) return Choice is
      Sub : constant U64 := U64 (A) - U64 (B);
   begin
      return Ct_Eq (Shift_Right (Sub, 32), 0);
   end Ct_Ge;

   function Ct_Ge (A, B : U64) return Choice is
      Sub : constant Tuple := Sub_Borrow (A, B, 0);
   begin
      return not Choice_From_Mask (Sub.Snd);
   end Ct_Ge;

   function Ct_Gt (A, B : U128) return Choice is
   begin
      return
        Ct_Gt (High (A), High (B)) or (Ct_Eq (High (A), High (B)) and Ct_Gt (Low (A), Low (B)));
   end Ct_Gt;

   function Cond_Select (A, B : U32; C : Choice) return U32 is
      Mask : constant U32 := U32 (Shift_Right (C, 32));
   begin
      return A xor (Mask and (A xor B));
   end Cond_Select;

   function Cond_Select (A, B : U64; C : Choice) return U64 is
   begin
      return A xor (U64 (C) and (A xor B));
   end Cond_Select;

   function Cond_Select (A, B : U128; C : Choice) return U128 is
      Mask : constant U128 := Shift_Left (U128 (C), 64) or U128 (C);
   begin
      return A xor (Mask and (A xor B));
   end Cond_Select;

   procedure CSwap (A, B : in out U32; C : Choice) is
      Tmp : constant U32 := A;
   begin
      A := Cond_Select (A, B, C);
      B := Cond_Select (B, Tmp, C);
   end CSwap;

   procedure CSwap (A, B : in out U64; C : Choice) is
      Tmp : constant U64 := A;
   begin
      A := Cond_Select (A, B, C);
      B := Cond_Select (B, Tmp, C);
   end CSwap;

   procedure CSwap (A, B : in out U128; C : Choice) is
      Tmp : constant U128 := A;
   begin
      A := Cond_Select (A, B, C);
      B := Cond_Select (B, Tmp, C);
   end CSwap;

end Bigints.Const_Choice;
