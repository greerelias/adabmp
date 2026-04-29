with Ada.Streams; use Ada.Streams;
with Packet_Formatter;
with Serial_Interface;

package Protocol is
   Frame_Size : constant Stream_Element := 64;
   --  Encodes the input data using COBS (Consistent Overhead Byte Stuffing).
   --  The result does not contain any 0x00 bytes.
   --  The result does NOT include the trailing 0x00 delimiter; the caller should add it if needed for transmission.
   function Encode (Data : Stream_Element_Array) return Stream_Element_Array;

   --  Decodes the input data using COBS.
   --  The input should NOT include the trailing 0x00 delimiter.
   function Decode (Data : Stream_Element_Array) return Stream_Element_Array;

   Decode_Error    : exception;
   Buffer_Overflow : exception;

   --  Encode and send a packet
   procedure Send_Packet
     (Port : in out Serial_Interface.Serial_Port'Class;
      Data : Stream_Element_Array);

   -- Receive and decode a packet
   procedure Receive_Packet
     (Port : in out Serial_Interface.Serial_Port'Class;
      Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   -- Attempts to read a packet containing the ready command from programmer
   -- Returns True if ready command received
   function Receive_Ready_Packet
     (Port : in out Serial_Interface.Serial_Port'Class) return Boolean;

   procedure Send_Command_Packet
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Command : Packet_Formatter.Command_Id);
end Protocol;
