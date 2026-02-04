with Serial_Interface;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Streams;           use Ada.Streams;
with Protocol;
with Ada.Exceptions;        use Ada.Exceptions;

package body Board_Info is

   procedure Get_Board_Info
     (Port : in out Serial_Interface.Serial_Port'Class; Success : out Boolean)
   is
      use Packet_Formatter;

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
               declare
                  Resp_Payload : constant Stream_Element_Array :=
                    Get_Payload (Response);
                  Info         : Board_Info_Record_Access;
                  Result_Str   : String (1 .. Integer (Resp_Payload'Length));
               begin
                  Info.Bytes (1) := Resp_Payload (Resp_Payload'Last - 0);
                  Info.Bytes (2) := Resp_Payload (Resp_Payload'Last - 1);
                  Info.Bytes (3) := Resp_Payload (Resp_Payload'Last - 2);
                  Info.Bytes (4) := Resp_Payload (Resp_Payload'Last - 3);
                  for I in Info.Bytes'Range loop
                     Put_Line (Stream_Element'Image (Info.Bytes (I)));
                  end loop;
                  Success := True;
               end;
            else
               Success := False;
            end if;
         end;
      else
         Put_Line ("Rx_Last is NOT greater than or equal to Rx_Buffer'First");
         Success := False;
      end if;

   exception
      when E : others =>
         Put_Line ("Exception: " & Exception_Name (E));
         Put_Line ("Message:   " & Exception_Message (E));
         Put_Line ("Info:      " & Exception_Information (E));
   end Get_Board_Info;

end Board_Info;
