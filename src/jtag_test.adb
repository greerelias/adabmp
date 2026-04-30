with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Streams;           use Ada.Streams;
with Packet_Formatter;      use Packet_Formatter;
with Interfaces;            use Interfaces;
with Commands;
with Protocol;
with Serial_Interface;

package body Jtag_Test is

   procedure Halt_CPU
     (Port : in out Serial_Interface.Serial_Port'Class) is

      Cmd     : constant Command_Id := Commands.Jtag_Halt;
      Payload : Stream_Element_Array (1 .. 0); -- Empty

      Rx_Buffer : Stream_Element_Array (1 .. 256);
      Rx_Last   : Stream_Element_Offset;

   begin
      -- Construct packet: [SOF] [CMD] [Payload]
      declare
         Packet : constant Stream_Element_Array := Make_Packet (Cmd, Payload);
      begin
         Protocol.Send_Packet (Port, Packet);
      end;

   exception
      when Board_Bad_Format =>
         raise;
      when Constraint_Error =>
         raise Communication_Error;
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
   end Halt_CPU;

   procedure Resume_CPU
     (Port : in out Serial_Interface.Serial_Port'Class) is

      Cmd     : constant Command_Id := Commands.Jtag_Resume;
      Payload : Stream_Element_Array (1 .. 0); -- Empty

      Rx_Buffer : Stream_Element_Array (1 .. 256);
      Rx_Last   : Stream_Element_Offset;

   begin
      -- Construct packet: [SOF] [CMD] [Payload]
      declare
         Packet : constant Stream_Element_Array := Make_Packet (Cmd, Payload);
      begin
         Protocol.Send_Packet (Port, Packet);
      end;

   exception
      when Board_Bad_Format =>
         raise;
      when Constraint_Error =>
         raise Communication_Error;
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
   end Resume_CPU;
procedure Get_Target_DM_Status
  (Port : in out Serial_Interface.Serial_Port'Class)
is
   Status : Interfaces.Unsigned_32 := 0;

   Cmd     : constant Command_Id := Commands.Jtag_Dm_Status;
   Payload : Stream_Element_Array (1 .. 0);

   Rx_Buffer : Stream_Element_Array (1 .. 256);
   Rx_Last   : Stream_Element_Offset;

   Communication_Error : exception;
   Board_Bad_Format    : exception;

   function To_Hex_String
     (Value : Interfaces.Unsigned_32) return String
   is
      Hex_Digits : constant array (Interfaces.Unsigned_32 range 0 .. 15) of Character :=
        ('0', '1', '2', '3',
         '4', '5', '6', '7',
         '8', '9', 'A', 'B',
         'C', 'D', 'E', 'F');

      Result : String (1 .. 10);
      Temp   : Interfaces.Unsigned_32 := Value;
   begin
      Result (1) := '0';
      Result (2) := 'x';

      for I in reverse 3 .. 10 loop
         Result (I) := Hex_Digits (Temp and 16#F#);
         Temp := Shift_Right (Temp, 4);
      end loop;

      return Result;
   end To_Hex_String;

   function Bit_Is_Set
     (Value : Interfaces.Unsigned_32;
      Bit   : Natural) return Boolean
   is
   begin
      return (Value and Shift_Left (Interfaces.Unsigned_32 (1), Bit)) /= 0;
   end Bit_Is_Set;

begin
   declare
      Packet : constant Stream_Element_Array := Make_Packet (Cmd, Payload);
   begin
      Protocol.Send_Packet (Port, Packet);
   end;

   Protocol.Receive_Packet (Port, Rx_Buffer, Rx_Last);

   if Rx_Last < Rx_Buffer'First then
      Put_Line ("Failure: No response from programmer");
      return;
   end if;

   declare
      Response : Stream_Element_Array renames
        Rx_Buffer (Rx_Buffer'First .. Rx_Last);
   begin
      if not Is_Valid (Response) then
         raise Board_Bad_Format;
      end if;

      if Get_Command (Response) = Commands.Data_Packet then
         declare
            Resp_Payload : constant Stream_Element_Array := Get_Payload (Response);
         begin
            if Resp_Payload'Length < 4 then
               raise Board_Bad_Format;
            end if;

            Status :=
              Shift_Left (Interfaces.Unsigned_32 (Resp_Payload (Resp_Payload'Last - 0)), 0)  or
              Shift_Left (Interfaces.Unsigned_32 (Resp_Payload (Resp_Payload'Last - 1)), 8)  or
              Shift_Left (Interfaces.Unsigned_32 (Resp_Payload (Resp_Payload'Last - 2)), 16) or
              Shift_Left (Interfaces.Unsigned_32 (Resp_Payload (Resp_Payload'Last - 3)), 24);

            Put_Line ("DMSTATUS = " & To_Hex_String (Status));
            Put_Line ("  anyhalted = " & Boolean'Image (Bit_Is_Set (Status, 8)));
            Put_Line ("  allhalted = " & Boolean'Image (Bit_Is_Set (Status, 9)));
            Put_Line ("  anyrunning = " & Boolean'Image (Bit_Is_Set (Status, 10)));
            Put_Line ("  allrunning = " & Boolean'Image (Bit_Is_Set (Status, 11)));
            Put_Line ("  anyunavail = " & Boolean'Image (Bit_Is_Set (Status, 12)));
            Put_Line ("  allunavail = " & Boolean'Image (Bit_Is_Set (Status, 13)));
            Put_Line ("  anyresumeack = " & Boolean'Image (Bit_Is_Set (Status, 16)));
            Put_Line ("  allresumeack = " & Boolean'Image (Bit_Is_Set (Status, 17)));
            Put_Line ("  anyhavereset = " & Boolean'Image (Bit_Is_Set (Status, 18)));
            Put_Line ("  allhavereset = " & Boolean'Image (Bit_Is_Set (Status, 19)));
            Put_Line ("  authenticated = " & Boolean'Image (Bit_Is_Set (Status, 7)));
            Put_Line ("  version[3:0] = " & Integer'Image (Integer (Status and 16#F#)));
         end;

      elsif Get_Command (Response) = Commands.JTAG_Error then
         Put_Line ("Failure: Error communicating with target.");
      else
         raise Communication_Error;
      end if;
   end;

exception
   when Board_Bad_Format =>
      raise;
   when Constraint_Error =>
      raise Communication_Error;
   when E : others =>
      Put_Line ("Exception: " & Exception_Name (E));
      Put_Line ("Message:   " & Exception_Message (E));
      Put_Line ("Info:      " & Exception_Information (E));
end Get_Target_DM_Status;

end Jtag_Test;