with Serial_Interface;
with Ada.Streams;           use Ada.Streams;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Serial_Interface.Stub is

   type Port_State is (Idle, Testing_Connection, Configuring_Target);
   type Mock_Port is limited new Serial_Interface.Serial_Port with record
      Opened       : Boolean := False;
      Written_Data : Unbounded_String;
      Read_String  : Unbounded_String;
      Read_Stream  : Stream_Element_Array (1 .. 512);
      Read_Count   : Stream_Element_Offset := 0;
      Read_Index   : Stream_Element_Offset := 1;
      Write_Stream : Stream_Element_Array (1 .. 1024);
      Write_Count  : Natural := 0;
      Write_Index  : Stream_Element_Offset := 1;
      Enabled      : Boolean := True;

      -- For Configure Target Test
      Bitstream_Size    : Natural;
      Fail_During_Write : Boolean := False;

      -- For Board Info Test
      Board_Info_Response : Stream_Element_Array (1 .. 128);
      Board_Info_Length   : Stream_Element_Offset := 0;

      State : Port_State := Idle;
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
     (Port : in out Mock_Port; Data : Stream_Element_Array);

private
   procedure Send_Ready_Packet (Port : in out Mock_Port);

end Serial_Interface.Stub;
