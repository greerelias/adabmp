with Ada.Streams; use Ada.Streams;
with Serial_Interface;

package Protocol is

   --  Encodes the input data using COBS (Consistent Overhead Byte Stuffing).
   --  The result does not contain any 0x00 bytes.
   --  The result does NOT include the trailing 0x00 delimiter; the caller should add it if needed for transmission.
   function Encode (Data : Stream_Element_Array) return Stream_Element_Array;

   --  Decodes the input data using COBS.
   --  The input should NOT include the trailing 0x00 delimiter.
   function Decode (Data : Stream_Element_Array) return Stream_Element_Array;

   Decode_Error    : exception;
   Buffer_Overflow : exception;

   --  High-level operations
   procedure Send_Packet
     (Port : in out Serial_Interface.Serial_Port'Class;
      Data : Stream_Element_Array);

   procedure Receive_Packet
     (Port : in out Serial_Interface.Serial_Port'Class;
      Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset);

   function Receive_Ready_Packet
     (Port : in out Serial_Interface.Serial_Port'Class) return Boolean;
end Protocol;
