with Serial_Interface;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Serial_Interface.Stub is

   type Mock_Port is limited new Serial_Interface.Serial_Port with record
      Opened           : Boolean := False;
      Written_Data     : Unbounded_String;
      Read_String      : Unbounded_String;
      Read_Stream      : Stream_Element_Array (1 .. 512);
      Read_Count       : Stream_Element_Offset := 0;
      Read_Index       : Stream_Element_Offset := 1;
      Loopback_Enabled : Boolean := True;

      Board_Info_Response : Stream_Element_Array (1 .. 128);
      Board_Info_Length   : Stream_Element_Offset := 0;
   end record;

   overriding
   procedure Open (Port : in out Mock_Port; Name : String);

   overriding
   procedure Close (Port : in out Mock_Port);

   overriding
   procedure Write (Port : in out Mock_Port; Data : Stream_Element_Array);

   procedure Read
     (Port   : in out Mock_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset);

   procedure Set_Input (Port : in out Mock_Port; Data : Stream_Element_Array);

   procedure Set_Board_Info_Response
      (Port : in out Mock_Port;
       Data : Stream_Element_Array);

end Serial_Interface.Stub;
