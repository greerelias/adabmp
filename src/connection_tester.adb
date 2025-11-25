with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Streams;           use Ada.Streams;
with Protocol;
with Packet_Formatting;
with Commands;

package body Connection_Tester is

   procedure Get_Programmer_Info
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : out Boolean;
      Info    : out Ada.Strings.Unbounded.Unbounded_String)
   is
      use Packet_Formatting;

      --  We'll send a Get_Info command with no payload
      Cmd     : constant Command_Id := Command_Id (Commands.Get_Info);
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
         --  Parse the response
         --  Assuming response is also a packet?
         --  If so, we validate and extract payload.
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
         Info := To_Unbounded_String ("Failed to communicate");
         Success := False;
   end Get_Programmer_Info;

   procedure Run_Test
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : out Boolean;
      Message : out Ada.Strings.Unbounded.Unbounded_String)
   is
      use Packet_Formatting;

      --  We'll use 64 bytes of payload
      Payload_Size : constant Stream_Element_Offset := 64;

      package Random_Bytes is new
        Ada.Numerics.Discrete_Random (Result_Subtype => Stream_Element);

      Gen : Random_Bytes.Generator;

      procedure Fill_Random (Buffer : out Stream_Element_Array) is
      begin
         for I in Buffer'Range loop
            Buffer (I) := Random_Bytes.Random (Gen);
         end loop;
      end Fill_Random;

      Tx_Payload : Stream_Element_Array (1 .. Payload_Size);
      Rx_Buffer  : Stream_Element_Array (1 .. 512);
      Rx_Last    : Stream_Element_Offset;

      Cmd : constant Command_Id := Command_Id (Commands.Data_Packet);

   begin
      Success := False;
      Random_Bytes.Reset (Gen);
      Fill_Random (Tx_Payload);
      Put_Line ("Generated 64 bytes of random payload.");

      --  Construct and Send
      declare
         Packet : constant Stream_Element_Array :=
           Make_Packet (Cmd, Tx_Payload);
      begin
         Protocol.Send_Packet (Port, Packet);
      end;

      delay 0.01;

      Protocol.Receive_Packet (Port, Rx_Buffer, Rx_Last);

      if Rx_Last >= Rx_Buffer'First then
         declare
            Response : Stream_Element_Array renames
              Rx_Buffer (Rx_Buffer'First .. Rx_Last);
         begin
            if Is_Valid (Response) then
               if Get_Command (Response) = Cmd then
                  declare
                     Rx_Payload : constant Stream_Element_Array :=
                       Get_Payload (Response);
                  begin
                     if Rx_Payload = Tx_Payload then
                        Success := True;
                        Message :=
                          To_Unbounded_String
                            ("SUCCESS: Data integrity verified.");
                     else
                        Message :=
                          To_Unbounded_String
                            ("FAILURE: Data mismatch detected.");
                     end if;
                  end;
               else
                  Message :=
                    To_Unbounded_String
                      ("FAILURE: Unexpected command received.");
               end if;
            else
               Message :=
                 To_Unbounded_String ("FAILURE: Invalid packet structure.");
            end if;
         end;
      else
         Message := To_Unbounded_String ("FAILURE: No data received.");
      end if;

   exception
      when Protocol.Decode_Error =>
         Message := To_Unbounded_String ("FAILURE: Decode Error");
         Success := False;
      when Protocol.Buffer_Overflow =>
         Message := To_Unbounded_String ("FAILURE: Buffer Overflow");
         Success := False;
      when others =>
         Message := To_Unbounded_String ("Exception during test execution");
         Success := False;
   end Run_Test;

end Connection_Tester;
