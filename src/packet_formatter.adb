package body Packet_Formatter is

   function Make_Packet
     (Cmd : Command_Id; Data : Stream_Element_Array)
      return Stream_Element_Array is
   begin
      if Data'Length > Max_Data_Size then
         raise Constraint_Error
           with "Data length exceeds maximum allowed size";
      end if;

      declare
         Packet : Stream_Element_Array (1 .. 2 + Data'Length);
      begin
         Packet (1) := Start_Of_Frame;
         Packet (2) := Stream_Element (Cmd);
         if Data'Length > 0 then
            Packet (3 .. Packet'Last) := Data;
         end if;
         return Packet;
      end;
   end Make_Packet;

   function Is_Valid (Packet : Stream_Element_Array) return Boolean is
   begin
      if Packet'Length < 2 then
         return False;
      end if;
      if Packet (Packet'First) /= Start_Of_Frame then
         return False;
      end if;
      if Packet'Length - 2 > Max_Data_Size then
         return False;
      end if;
      return True;
   end Is_Valid;

   function Get_Command (Packet : Stream_Element_Array) return Command_Id is
   begin
      return Command_Id (Packet (Packet'First + 1));
   end Get_Command;

   function Get_Payload
     (Packet : Stream_Element_Array) return Stream_Element_Array is
   begin
      if Packet'Length <= 2 then
         return (1 .. 0 => 0); -- Empty array

      end if;
      return Packet (Packet'First + 2 .. Packet'Last);
   end Get_Payload;

end Packet_Formatter;
