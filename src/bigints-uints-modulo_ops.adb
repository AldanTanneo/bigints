package body Bigints.Uints.Modulo_Ops with SPARK_Mode => On is

   function Add_Mod (A, B, P : Uint) return Uint is
      WCarry  : constant Uint_Carry := Add_Carry (A, B, 0);
      WBorrow : constant Uint_Carry := Sub_Borrow (WCarry.Res, P, 0);
      Mask    : constant Primitives.Tuple :=
        Primitives.Sub_Borrow (WCarry.Carry, 0, WBorrow.Carry);
   begin
      return WBorrow.Res + (P and Mask.Snd);
   end Add_Mod;

   function Sub_Mod (A, B, P : Uint) return Uint is
      OutMask : constant Uint_Carry := Sub_Borrow (A, B, 0);
   begin
      return OutMask.Res + (P and OutMask.Carry);
   end Sub_Mod;

   function Neg_Mod (A, P : Uint) return Uint is
      Mask : constant Const_Choice.Choice :=
        Const_Choice.Choice_From_Condition (A = ZERO);
   begin
      return Cond_Select (P - A, A, Mask);
   end Neg_Mod;

end Bigints.Uints.Modulo_Ops;
