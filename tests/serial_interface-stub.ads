with Serial_Interface;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Serial_Interface.Stub is

   type Mock_Port is limited new Serial_Interface.Serial_Port with record
      Opened       : Boolean := False;
      Written_Data : Unbounded_String;
      Read_String  : Unbounded_String;
      Read_Stream  : Stream_Element_Array (1 .. 64);
   end record;

   overriding
   procedure Open (Port : in out Mock_Port; Name : String);

   overriding
   procedure Close (Port : in out Mock_Port);

   overriding
   procedure Write (Port : in out Mock_Port; Data : String);

   overriding
   procedure Write (Port : in out Mock_Port; Data : Stream_Element_Array);

   overriding
   procedure Read
     (Port : in out Mock_Port; Buffer : out String; Last : out Natural);

   procedure Read
     (Port   : in out Mock_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   procedure Set_Input (Port : in out Mock_Port; Data : Stream_Element_Array);

   --  Helper to set up the mock
   procedure Set_Input (Port : in out Mock_Port; Data : String);
   function Get_Output (Port : Mock_Port) return String;

end Serial_Interface.Stub;
