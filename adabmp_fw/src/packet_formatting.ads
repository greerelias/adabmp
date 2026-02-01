with HAL; use HAL;

package Packet_Formatting is

   Start_Of_Frame : constant UInt8 := 16#AA#;

   type Command_Id is new UInt8;

   --  Maximum data size allowed (strictly less than 254 bytes)
   Max_Data_Size : constant Natural := 253;

   --  Constructs a packet: [SOF] [CMD] [DATA]
   --  Raises Constraint_Error if Data'Length > Max_Data_Size
   function Make_Packet
     (Cmd : Command_Id; Data : UInt8_Array) return UInt8_Array;

   --  Checks if the packet has a valid SOF and minimum length
   function Is_Valid (Packet : UInt8_Array) return Boolean;

   --  Extracts the Command byte
   --  Precondition: Is_Valid (Packet)
   function Get_Command (Packet : UInt8_Array) return Command_Id;

   --  Extracts the Payload
   --  Precondition: Is_Valid (Packet)
   function Get_Payload (Packet : UInt8_Array) return UInt8_Array;

end Packet_Formatting;
