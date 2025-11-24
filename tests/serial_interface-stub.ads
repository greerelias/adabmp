with Serial_Interface;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Serial_Interface.Stub is

   type Mock_Port is limited new Serial_Interface.Serial_Port with record
      Opened       : Boolean := False;
      Written_Data : Unbounded_String;
      Read_Data    : Unbounded_String;
      Read_Index   : Positive := 1;
   end record;

   overriding
   procedure Open (Port : in out Mock_Port; Name : String);

   overriding
   procedure Close (Port : in out Mock_Port);

   overriding
   procedure Write (Port : in out Mock_Port; Data : String);

   overriding
   procedure Read
     (Port : in out Mock_Port; Buffer : out String; Last : out Natural);

   --  Helper to set up the mock
   procedure Set_Input (Port : in out Mock_Port; Data : String);
   function Get_Output (Port : Mock_Port) return String;

end Serial_Interface.Stub;
