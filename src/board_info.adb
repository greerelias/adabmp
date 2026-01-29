with Serial_Interface;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Board_Info is

   procedure Get_Board_Info
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : out Boolean;
      Info    : out Unbounded_String) 
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
                  Result_Str   : String (1 .. Integer (Resp_Payload'Length));
               begin
                  for I in Resp_Payload'Range loop
                     Result_Str (Integer (I - Resp_Payload'First + 1)) :=
                       Character'Val (Resp_Payload (I));
                  end loop;
                  Info := To_Unbounded_String (Result_Str);
                  Success := True;
               end;
            else
               Info := To_Unbounded_String ("Invalid packet received");
               Success := False;
            end if;
         end;
      else
         Success := False;
      end if;

   exception
      when others =>
         Info := To_Unbounded_String ("Failed to to get board info.");
         Success := False;
   end Get_Board_Info;

end Board_Info;
