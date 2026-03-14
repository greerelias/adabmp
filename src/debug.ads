with Ada.Streams;
with Serial_Interface;
with Interfaces;       use Interfaces;
with Ada.Streams;      use Ada.Streams;

package Debug is

   procedure Debug
      (Port : in out Serial_Interface.Serial_Port'Class);

   --  procedure Reset_TAP
   --    (Port : in out Serial_Interface.Serial_Port'Class;
   --     Success : out Boolean);

   --  procedure Read_IDCODE
   --    (Port : in out Serial_Interface.Serial_Port'Class;
   --     ID : out Interfaces.Unsigned_32;
   --     Success : out Boolean);

   --  procedure Shift_IR
   --    (Port : in out Serial_Interface.Serial_Port'Class;
   --     Tx_Data : Ada.Streams.Stream_Element_Array;
   --     Rx_Data : out Ada.Streams.Stream_Element_Array;
   --     Bit_Length : Natural;
   --     Success : out Boolean);

   --  procedure Shift_DR
   --    (Port : in out Serial_Interface.Serial_Port'Class;
   --     Tx_Data : Ada.Streams.Stream_Element_Array;
   --     Rx_Data : out Ada.Streams.Stream_Element_Array;
   --     Bit_Length : Natural;
   --     Success : out Boolean);
end Debug;