with Ada.Text_IO;      use Ada.Text_IO;
with Ada.Exceptions;   use Ada.Exceptions;
with Packet_Formatter; use Packet_Formatter;
with Commands;
with Protocol;

package body Board_Info is

   procedure Get_Board_Info
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Info    : out Board_Info_Record_Access;
      Success : out Boolean)
   is
      --  We'll send a Get_Board_Info command with no payload
      Cmd     : constant Command_Id := Commands.Get_Board_Info;
      Payload : Stream_Element_Array (1 .. 0); -- Empty

      Rx_Buffer : Stream_Element_Array (1 .. 256);
      Rx_Last   : Stream_Element_Offset;
   begin
      --  Construct packet: [SOF] [CMD] [Payload]
      declare
         Packet : constant Stream_Element_Array := Make_Packet (Cmd, Payload);
      begin
         Protocol.Send_Packet (Port, Packet);
      end;

      Protocol.Receive_Packet (Port, Rx_Buffer, Rx_Last);

      if Rx_Last >= Rx_Buffer'First then
         declare
            Response : Stream_Element_Array renames
              Rx_Buffer (Rx_Buffer'First .. Rx_Last);
         begin
            if Is_Valid (Response) then
               if Get_Command (Response) = Commands.Data_Packet then
                  declare
                     Resp_Payload : constant Stream_Element_Array :=
                       Get_Payload (Response);
                  begin
                     Info.Bytes (1) := Resp_Payload (Resp_Payload'Last - 0);
                     Info.Bytes (2) := Resp_Payload (Resp_Payload'Last - 1);
                     Info.Bytes (3) := Resp_Payload (Resp_Payload'Last - 2);
                     Info.Bytes (4) := Resp_Payload (Resp_Payload'Last - 3);
                     Success := True;
                  end;
               elsif Get_Command (Response) = Commands.JTAG_Error then
                  Success := False;
                  Put_Line ("Falure: Error communicating with target.");
               else
                  raise Communication_Error;
               end if;
            else
               raise Board_Bad_Format;
            end if;
         end;
      else
         Success := False;
         Put_Line ("Failure: No response from programmer");
      end if;

   exception
      when Board_Bad_Format =>
         raise;
      when Constraint_Error =>
         raise Communication_Error;
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
   end Get_Board_Info;

end Board_Info;
