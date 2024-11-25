with Ada.Characters.Latin_1;
with Ada.Command_Line;
with Ada.Environment_Variables; use Ada.Environment_Variables;
with Ada.Strings.Fixed;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

procedure Runner is
   ESC        : constant String := [Ada.Characters.Latin_1.ESC];
   Ansi_Dim   : constant String := ESC & "[2m";
   Ansi_Red   : constant String := ESC & "[1;31m";
   Ansi_Green : constant String := ESC & "[1;32m";
   Ansi_Reset : constant String := ESC & "[0m";

   Run_Id       : Natural := 0;
   Passed_Tests : Natural := 0;
   Failed_Tests : Natural := 0;

   procedure Free_Arg_List is new
     Ada.Unchecked_Deallocation (Argument_List, Argument_List_Access);

   procedure Put_Success (Msg : String) is
   begin
      Put_Line (Ansi_Green & "  PASS " & Ansi_Reset & Msg);
      Passed_Tests := Passed_Tests + 1;
   end Put_Success;
   procedure Put_Failure (Msg : String) is
   begin
      Put_Line (Ansi_Red & "  FAIL " & Ansi_Reset & Msg);
      Failed_Tests := Failed_Tests + 1;
   end Put_Failure;

   function Remove_Suffix (Source, Suffix : String) return String is
   begin
      if Source'Length < Suffix'Length
        or else (Suffix'Length /= 0
                 and then Source
                            (Source'Last + 1 - Suffix'Length .. Source'Last)
                          /= Suffix)
      then
         raise Program_Error
           with "invalid input: missing " & Suffix & " suffix";
      end if;
      return Source (Source'First .. Source'Last - Suffix'Length);
   end Remove_Suffix;

   procedure Run_Test (Test_File : String) is
      Test_Name   : constant String := Remove_Suffix (Test_File, ".adb");
      Output_File : constant String :=
        "./obj/TMP_TEST_FILE__"
        & Test_Name
        & "__"
        & Run_Id'Image (2 .. Run_Id'Image'Last)
        & ".out";
      Code        : Integer;
      Success     : Boolean;
   begin
      Spawn ("./bin/" & Test_Name, [], Output_File, Success, Code);
      Run_Id := Run_Id + 1;
      if not Success then
         Put_Failure
           (Test_Name & Ansi_Red & " (could not start test)" & Ansi_Reset);
      elsif Code /= 0 then
         Put_Failure (Test_Name);

         Put_Line
           ("*** Test output (returned" & Code'Image & ") ***" & Ansi_Dim);
         declare
            Arg_List : Argument_List_Access :=
              Argument_String_To_List (Output_File);
         begin
            Code := Spawn ("/bin/cat", Arg_List.all);
            Free_Arg_List (Arg_List);
         end;
         Put_Line (Ansi_Reset & "*** End Test output ***");
      else
         Put_Success (Test_Name);
      end if;
      Delete_File (Output_File, Success);
   end Run_Test;

   Tests       : constant String := Value ("TEST_FILES", "");
   Separator   : constant String := ":";
   Idx         : Positive := 1;
   Total_Tests : Natural := 0;
begin
   if Tests = "" then
      return;
   end if;

   for I in 1 .. Ada.Strings.Fixed.Count (Tests, Separator) + 1 loop
      declare
         Pos : constant Natural :=
           Ada.Strings.Fixed.Index (Tests (Idx .. Tests'Last), Separator);
      begin
         if Pos = 0 then
            Run_Test (Tests (Idx .. Tests'Last));
         else
            Run_Test (Tests (Idx .. Pos - 1));
         end if;
         Idx := Pos + Separator'Length;
      end;
   end loop;

   Total_Tests := Failed_Tests + Passed_Tests;

   Put_Line
     ("Tests:"
      & Total_Tests'Image
      & ", Passed:"
      & Passed_Tests'Image
      & ", Failed:"
      & Failed_Tests'Image);

   if Failed_Tests /= 0 then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;

end Runner;
