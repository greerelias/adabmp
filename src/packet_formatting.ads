with Ada.Streams; use Ada.Streams;

package Packet_Formatting is

   Start_Of_Frame : constant Stream_Element := 16#AA#;

   type Command_Id is new Stream_Element;

   --  Maximum data size allowed (strictly less than 254 bytes)
   Max_Data_Size : constant Stream_Element_Offset := 253;

   --  Constructs a packet: [SOF] [CMD] [DATA]
   --  Raises Constraint_Error if Data'Length > Max_Data_Size
   function Make_Packet (Cmd : Command_Id; Data : Stream_Element_Array) return Stream_Element_Array;

   --  Checks if the packet has a valid SOF and minimum length
   function Is_Valid (Packet : Stream_Element_Array) return Boolean;

   --  Extracts the Command byte
   --  Precondition: Is_Valid (Packet)
   function Get_Command (Packet : Stream_Element_Array) return Command_Id;

   --  Extracts the Payload
   --  Precondition: Is_Valid (Packet)
   function Get_Payload (Packet : Stream_Element_Array) return Stream_Element_Array;

end Packet_Formatting;
