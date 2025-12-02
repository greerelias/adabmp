with Ada.Streams; use Ada.Streams;
with Packet_Formatter;
with Commands;
with Protocol;

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
      if Port.Loopback_Enabled then
         -- Simple loopback: whatever is written is available to read
         -- BUT, we need to handle the new handshake logic.
         -- If we receive Test_Connection, we should reply with Ready.
         -- If we receive Data_Packet, we should reply with the same Data_Packet.

         declare
            -- Decode the packet to inspect it
            -- Note: Data includes the trailing 0, so we exclude it for decoding
            Decoded         : Stream_Element_Array :=
              Decode (Data (Data'First .. Data'Last - 1));
            Connection_Test : Boolean := false;
         begin
            if Is_Valid (Decoded) then
               if Get_Command (Decoded) = Test_Connection then
                  -- Reply with Ready
                  declare
                     Ready_Packet  : Stream_Element_Array :=
                       Make_Packet (Ready, (1 .. 0 => 0)); -- Empty payload
                     Encoded_Ready : Stream_Element_Array :=
                       Encode (Ready_Packet);
                     Final_Packet  :
                       Stream_Element_Array (1 .. Encoded_Ready'Length + 1);
                  begin
                     Final_Packet (1 .. Encoded_Ready'Length) := Encoded_Ready;
                     Final_Packet (Final_Packet'Last) := 0;

                     Port.Set_Input (Final_Packet);
                  end;
               elsif Get_Command (Decoded) = End_Test then
                  -- Consume End_Test, do not reply
                  null;
               else
                  -- Default loopback behavior for other packets (like Data_Packet)
                  if Data'Length <= Port.Read_Stream'Length then
                     Port.Read_Stream (1 .. Data'Length) := Data;
                     Port.Read_Count := Data'Length;
                     Port.Read_Index := 1;
                  end if;
               end if;
            end if;
         exception
            when others =>
               -- If decoding fails, just loopback raw data (fallback)
               if Data'Length <= Port.Read_Stream'Length then
                  Port.Read_Stream (1 .. Data'Length) := Data;
                  Port.Read_Count := Data'Length;
                  Port.Read_Index := 1;
               end if;
         end;
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

end Serial_Interface.Stub;
