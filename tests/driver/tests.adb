with AUnit.Assertions;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Numerics.Discrete_Random;

package body Tests is

   package Rng is new Ada.Numerics.Discrete_Random (U64);

   function Random_U256 return U256 is
      G   : Rng.Generator;
      Res : U256;
   begin
      Rng.Reset (G);
      for I in Res'Range loop
         Res (I) := Rng.Random (G);
      end loop;
      return Res;
   end Random_U256;

   package Conversions is new
     Ada.Numerics.Big_Numbers.Big_Integers.Unsigned_Conversions (U64);

   function To_Big_Number (A : U256) return Big_Natural is
      Res : Big_Natural := 0;
   begin
      for I in reverse A'Range loop
         Res := Res * (2 ** 64) + Conversions.To_Big_Integer (A (I));
      end loop;
      return Res;
   end To_Big_Number;

   function To_Big_Number (A : U256s.Wide_Uint) return Big_Natural is
      Res : Big_Natural := 0;
   begin
      for I in reverse A'Range loop
         Res := Res * (2 ** 64) + Conversions.To_Big_Integer (A (I));
      end loop;
      return Res;
   end To_Big_Number;

   function To_Big_Number (A : U64) return Big_Natural is
   begin
      return Conversions.To_Big_Integer (A);
   end To_Big_Number;

   function To_Uint (A : Big_Natural) return U256 is
      Val : Big_Natural := A;
      Res : U256;
   begin
      for I in Res'Range loop
         Res (I) := Conversions.From_Big_Integer (Val rem (2 ** 64));
         Val := Val / (2 ** 64);
      end loop;
      if Val /= 0 then
         raise Program_Error with "Value does not fit in U256";
      end if;
      return Res;
   end To_Uint;

   function To_Wide_Uint (A : Big_Natural) return U256s.Wide_Uint is
      Val : Big_Natural := A;
      Res : U256s.Wide_Uint;
   begin
      for I in Res'Range loop
         Res (I) := Conversions.From_Big_Integer (Val rem (2 ** 64));
         Val := Val / (2 ** 64);
      end loop;
      if Val /= 0 then
         raise Program_Error with "Value does not fit in U256";
      end if;
      return Res;
   end To_Wide_Uint;

   function To_U64 (A : Big_Natural) return U64 is
   begin
      return Conversions.From_Big_Integer (A);
   end To_U64;

   procedure Assert
     (Condition : Boolean;
      Msg       : String := "";
      Test_Name : String := GNAT.Source_Info.Enclosing_Entity;
      Line      : Natural := GNAT.Source_Info.Line) is
   begin
      if not Condition then
         Put_Line ("In `" & Test_Name & "`, line" & Line'Image);
         Put_Line ("Assertion failed: " & Msg);
      end if;
      AUnit.Assertions.Assert (Condition, Msg, Test_Name, Line);
   end Assert;

   procedure Assert_Eq
     (Left, Right : U64;
      Msg         : String := "";
      Test_Name   : String := GNAT.Source_Info.Enclosing_Entity;
      Line        : Natural := GNAT.Source_Info.Line)
   is
      Cond : constant Boolean := Left = Right;
   begin
      Assert
        (Cond,
         Msg & LF & "Left =" & Left'Image & LF & "Right =" & Right'Image,
         Test_Name,
         Line);
   end Assert_Eq;

   procedure Assert_Eq
     (Left, Right : U256;
      Msg         : String := "";
      Test_Name   : String := GNAT.Source_Info.Enclosing_Entity;
      Line        : Natural := GNAT.Source_Info.Line)
   is
      Cond : constant Boolean := U256s."=" (Left, Right);
   begin
      Assert
        (Cond,
         Msg & LF & "Left =" & Left'Image & LF & "Right =" & Right'Image,
         Test_Name,
         Line);
   end Assert_Eq;

   procedure Assert_Eq
     (Left, Right : U256s.Wide_Uint;
      Msg         : String := "";
      Test_Name   : String := GNAT.Source_Info.Enclosing_Entity;
      Line        : Natural := GNAT.Source_Info.Line)
   is
      Cond : constant Boolean :=
        (for all I in U256s.Wide_Uint'Range => Left (I) = Right (I));
   begin
      Assert
        (Cond,
         Msg & LF & "Left =" & Left'Image & LF & "Right =" & Right'Image,
         Test_Name,
         Line);
   end Assert_Eq;
end Tests;
