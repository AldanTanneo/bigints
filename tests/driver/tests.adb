with AUnit.Assertions;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Text_IO;            use Ada.Text_IO;

package body Tests is
   procedure Assert
     (Condition : Boolean;
      Msg       : String;
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
      Msg         : String;
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
      Msg         : String;
      Test_Name   : String := GNAT.Source_Info.Enclosing_Entity;
      Line        : Natural := GNAT.Source_Info.Line)
   is
      Cond : constant Boolean := U256s.Equal (Left, Right);
   begin
      Assert
        (Cond,
         Msg & LF & "Left =" & Left'Image & LF & "Right =" & Right'Image,
         Test_Name,
         Line);
   end Assert_Eq;

   procedure Assert_Eq
     (Left, Right : U256s.Wide_Uint;
      Msg         : String;
      Test_Name   : String := GNAT.Source_Info.Enclosing_Entity;
      Line        : Natural := GNAT.Source_Info.Line)
   is
      Cond : constant Boolean := (for all I in 1 .. 8 => Left (I) = Right (I));
   begin
      Assert
        (Cond,
         Msg & LF & "Left =" & Left'Image & LF & "Right =" & Right'Image,
         Test_Name,
         Line);
   end Assert_Eq;
end Tests;
