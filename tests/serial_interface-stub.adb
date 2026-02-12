with Ada.Streams; use Ada.Streams;
with Packet_Formatter;
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
                           declare
                              Payload     : Stream_Element_Array :=
                                Get_Payload (Decoded);
                              Data_Length : Natural
                              with Address => Payload'Address;
                           begin
                              Port.Bitstream_Size := Data_Length;
                              Port.State :=
                                Serial_Interface.Stub.Configuring_Target;
                              Send_Ready_Packet (Port);
                           end;

                        when others           =>
                           null;
                     end case;
                  end if;
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
                 and then Port.Write_Count > Port.Bitstream_Size / 2
               then
                  Port.State := Idle;
               elsif Port.Write_Count >= Port.Bitstream_Size then
                  -- Write complete
                  Port.Write_Count := 0;
                  Port.State := Idle;
               else
                  Port.Write_Count := Port.Write_Count + Data'Length;
                  -- Got data, tell host to send more
                  Send_Ready_Packet (Port);
               end if;
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
      if Port.Read_Index > Port.Read_Count then
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

end Serial_Interface.Stub;
