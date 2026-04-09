with Ada.Streams; use Ada.Streams;
with Commands;
with Protocol;
with Serial_Interface.Stub;

package body Serial_Interface.Stub is

   overriding
   procedure Open (Port : in out Mock_Port; Name : String) is
   begin
      Port.Opened := True;
   end Open;

   overriding
   procedure Close (Port : in out Mock_Port) is
   begin
      Port.Opened := False;
   end Close;

   -- Write Stream
   overriding
   procedure Write (Port : in out Mock_Port; Data : Stream_Element_Array) is
      use Packet_Formatter;
      use Commands;
      use Protocol;
   begin
      if Port.Enabled then
         case Port.State is
            when Idle               =>
               declare
                  Decoded         : Stream_Element_Array :=
                    Decode (Data (Data'First .. Data'Last - 1));
                  Connection_Test : Boolean := false;
               begin
                  if Is_Valid (Decoded) then
                     case Get_Command (Decoded) is
                        when Test_Connection  =>
                           Port.State :=
                             Serial_Interface.Stub.Testing_Connection;
                           Send_Ready_Packet (Port);

                        when Get_Board_Info   =>
                           if Port.Board_Info_Length > 0 then
                              Port.Read_Stream (1 .. Port.Board_Info_Length) :=
                                Port.Board_Info_Response
                                  (1 .. Port.Board_Info_Length);
                              Port.Read_Count := Port.Board_Info_Length;
                              Port.Read_Index := 1;
                           end if;

                        when Configure_Target =>
                           -- TODO: update and add bad response logic
                           -- and bad response test
                           declare
                              Payload     : Stream_Element_Array :=
                                Get_Payload (Decoded);
                              Data_Length : Natural
                              with Address => Payload'Address;
                           begin
                              Port.Data_Size := Data_Length;
                              Port.State :=
                                Serial_Interface.Stub.Configuring_Target;
                              Send_Ready_Packet (Port);
                           end;

                        when Flash_Target     =>
                           declare
                              Payload          :
                                constant Stream_Element_Array :=
                                  Get_Payload (Decoded);
                              Data_Length_Arr  : Stream_Element_Array (1 .. 4);
                              Data_Length      : Natural
                              with Address => Data_Length_Arr'Address;
                              Base_Address_Arr : Stream_Element_Array (1 .. 4);
                              Base_Address     : Natural
                              with Address => Base_Address_Arr'Address;
                           begin
                              if Payload'Length >= 8 then
                                 Data_Length_Arr :=
                                   Payload
                                     (Payload'First .. Payload'First + 3);
                                 Base_Address_Arr :=
                                   Payload
                                     (Payload'First + 4 .. Payload'First + 7);
                                 Port.Data_Size := Data_Length;
                                 Port.State :=
                                   Serial_Interface.Stub.Flashing_Target;
                                 Send_Ready_Packet (Port);
                              end if;
                           end;

                        when Start_UART       =>
                           -- Logic handled in UART_Tests.adb
                           null;

                        when Flash_Erase      =>
                           declare
                              Erase_Data : constant Stream_Element_Array :=
                                Get_Payload (Decoded);
                              Base_Arr   : Stream_Element_Array (1 .. 4) :=
                                Erase_Data
                                  (Erase_Data'First .. Erase_Data'First + 3);
                              Base_Addr  : Integer
                              with Address => Base_Arr'Address;
                              Sec_Arr    : Stream_Element_Array (1 .. 4) :=
                                Erase_Data
                                  (Erase_Data'First + 4
                                   .. Erase_Data'First + 7);
                              Sectors    : Integer
                              with Address => Sec_Arr'Address;
                           begin
                              Port.Base_Addr := Base_Addr;
                              Port.Sectors := Sectors;
                              Port.Blocks_32 :=
                                Integer (Erase_Data (Erase_Data'First + 8));
                              Port.Blocks_64 :=
                                Integer (Erase_Data (Erase_Data'First + 9));
                              Port.Cur_Address := Port.Base_Addr;
                              Port.State :=
                                Serial_Interface.Stub.Erasing_Flash;
                           end;

                        when others           =>
                           null;
                     end case;
                  end if;
               exception
                  when E : Protocol.Decode_Error =>
                     -- ignore
                     null;
               end;

            when Testing_Connection =>
               if Data (3) = 2 then
                  -- Data_Packet
                  if Data'Length <= Port.Read_Stream'Length then
                     Port.Read_Stream (1 .. Data'Length) := Data;
                     Port.Read_Count := Data'Length;
                     Port.Read_Index := 1;
                  end if;
               else
                  Port.State := Idle;
               end if;

            when Configuring_Target =>
               -- Mock failure during write
               if Port.Fail_During_Write
                 and then Port.Write_Count > Port.Data_Size / 2
               then
                  Port.State := Idle;
               else
                  Port.Write_Count := Port.Write_Count + Data'Length;
                  if Port.Write_Count >= Port.Data_Size then
                     -- Write complete
                     Port.Send_Command_Packet (Configure_Target_Complete);
                     Port.Write_Count := 0;
                     Port.State := Idle;
                     return;
                  end if;
                  -- Got data, tell host to send more
                  Send_Ready_Packet (Port);
               end if;

            when Flashing_Target    =>
               -- Mock failure during flash
               if Port.Fail_During_Write
                 and then Port.Write_Count > Port.Data_Size / 2
               then
                  Port.State := Idle;
               else
                  Port.Write_Count := Port.Write_Count + Data'Length;
                  if Port.Write_Count >= Port.Data_Size then
                     -- Write complete
                     Port.Send_Command_Packet (Flash_Target_Complete);
                     Port.Write_Count := 0;
                     Port.State := Idle;
                     return;
                  end if;
                  -- Got data, tell host to send more
                  Port.Send_Ready_Packet;
               end if;

            when Erasing_Flash      =>
               declare
                  Block64_Size : constant Integer := 2 ** 16;
                  Block32_Size : constant Integer := 2 ** 15;
                  Sector_Size  : constant Integer := 2 ** 12;
               begin
                  Port.Send_Ready_Packet;
                  for I in 1 .. Port.Blocks_64 loop
                     Port.Cur_Address := Port.Cur_Address + Block64_Size;
                     Port.Send_Command_Packet (Block64_Erase_Done);
                  end loop;
                  for I in 1 .. Port.Blocks_32 loop
                     Port.Cur_Address := Port.Cur_Address + Block32_Size;
                     Port.Send_Command_Packet (Block32_Erase_Done);
                  end loop;
                  for I in 1 .. Port.Sectors loop
                     Port.Cur_Address := Port.Cur_Address + Sector_Size;
                     Port.Send_Command_Packet (Sector_Erase_Done);
                  end loop;
                  Port.Send_Command_Packet (Flash_Erase_Complete);
                  Port.State := Idle;
               end;
         end case;
      end if;
   end Write;

   -- Read Stream
   overriding
   procedure Read
     (Port   : in out Mock_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset)
   is
      Available : Stream_Element_Offset;
      To_Read   : Stream_Element_Offset;
   begin
      if not Port.Enabled or Port.Read_Index > Port.Read_Count then
         Last := Buffer'First - 1; -- No data
         return;
      end if;

      Available := Port.Read_Count - Port.Read_Index + 1;
      To_Read := Stream_Element_Offset'Min (Available, Buffer'Length);

      Buffer (Buffer'First .. Buffer'First + To_Read - 1) :=
        Port.Read_Stream (Port.Read_Index .. Port.Read_Index + To_Read - 1);

      Port.Read_Index := Port.Read_Index + To_Read;
      Last := Buffer'First + To_Read - 1;
   end Read;

   procedure Set_Input (Port : in out Mock_Port; Data : Stream_Element_Array)
   is
   begin
      if Data'Length <= Port.Read_Stream'Length then
         Port.Read_Stream (1 .. Data'Length) := Data;
         Port.Read_Count := Data'Length;
         Port.Read_Index := 1;
      end if;
   end Set_Input;

   procedure Set_Board_Info_Response
     (Port : in out Mock_Port; Data : Stream_Element_Array) is
   begin
      Port.Board_Info_Response (1 .. Data'Length) := Data;
      Port.Board_Info_Length := Data'Length;
   end Set_Board_Info_Response;

   procedure Send_Ready_Packet (Port : in out Mock_Port) is
      Ready_Packet  : Stream_Element_Array :=
        Packet_Formatter.Make_Packet
          (Commands.Ready, (1 .. 0 => 0)); -- Empty payload
      Encoded_Ready : Stream_Element_Array := Protocol.Encode (Ready_Packet);
      Final_Packet  : Stream_Element_Array (1 .. Encoded_Ready'Length + 1);
   begin
      Final_Packet (1 .. Encoded_Ready'Length) := Encoded_Ready;
      Final_Packet (Final_Packet'Last) := 0;
      Port.Set_Input (Final_Packet);
   end Send_Ready_Packet;

   procedure Send_Command_Packet
     (Port : in out Mock_Port; Command : in Packet_Formatter.Command_Id)
   is
      Ready_Packet  : Stream_Element_Array :=
        Packet_Formatter.Make_Packet (Command, (1 .. 0 => 0)); -- Empty payload
      Encoded_Ready : Stream_Element_Array := Protocol.Encode (Ready_Packet);
      Final_Packet  : Stream_Element_Array (1 .. Encoded_Ready'Length + 1);
   begin
      Final_Packet (1 .. Encoded_Ready'Length) := Encoded_Ready;
      Final_Packet (Final_Packet'Last) := 0;
      Port.Set_Input (Final_Packet);
   end Send_Command_Packet;

end Serial_Interface.Stub;
