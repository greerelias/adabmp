with HAL;               use HAL;
with USB.Device.Serial; use USB.Device.Serial;

package Protocol is

   --  Encodes the input data using COBS (Consistent Overhead Byte Stuffing).
   --  The result does not contain any 0x00 bytes.
   --  The result does NOT include the trailing 0x00 delimiter; the caller should add it if needed for transmission.
   function Encode (Data : UInt8_Array) return UInt8_Array;

   --  Decodes the input data using COBS.
   --  The input should NOT include the trailing 0x00 delimiter.
   function Decode (Data : UInt8_Array) return UInt8_Array;

   Decode_Error    : exception;
   Buffer_Overflow : exception;

   --  High-level operations
   --  procedure Send_Packet
   --    (Port : in out Default_Serial_Class; Data : UInt8_Array);

   --  procedure Receive_Packet
   --    (Port : in out Default_Serial_Class;
   --     Data : out UInt8_Array;
   --     Last : out Natural);

end Protocol;
