with Ada.Strings.Fixed;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Streams;           use Ada.Streams;

package body Connection_Tester is

   procedure Get_Programmer_Info
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : out Boolean;
      Info    : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Request    : String := "GET_INFO";
      Reply      : String (1 .. 128);
      Chars_Read : Integer;
   begin
      Port.Write (Request);
      delay 0.01;
      Port.Read (Reply, Chars_Read);
      if Chars_Read > 0 then
         Info := To_Unbounded_String (Reply (1 .. Chars_Read));
         Success := True;
      else
         Success := False;
      end if;

   exception
      when others =>
         Info := To_Unbounded_String ("Failed to write to port");
         return;

   end Get_Programmer_Info;

   procedure Run_Test
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : out Boolean;
      Message : out Ada.Strings.Unbounded.Unbounded_String)
   is
      Packet_Size : constant Stream_Element_Offset := 64;

      -- 2. Instantiate Random for Stream_Element (Byte)
      package Random_Bytes is new
        Ada.Numerics.Discrete_Random (Result_Subtype => Stream_Element);

      -- Generator state
      Gen : Random_Bytes.Generator;

      -- 3. Helper procedure to fill a packet with random noise
      procedure Fill_Random (Buffer : out Stream_Element_Array) is
      begin
         for I in Buffer'Range loop
            Buffer (I) := Random_Bytes.Random (Gen);
         end loop;
      end Fill_Random;

      -- Variables for the test
      Tx_Buffer  : Stream_Element_Array (1 .. Packet_Size); -- Transmit buffer
      Rx_Buffer  : Stream_Element_Array (1 .. Packet_Size); -- Recieve buffer
      Bytes_Read : Stream_Element_Offset;

   begin
      Success := False;
      -- Seed the generator
      Random_Bytes.Reset (Gen);

      -- Generate the random data
      Fill_Random (Tx_Buffer);
      Put_Line ("Generated 64 bytes of random data for transmission.");

      begin
         Port.Write (Tx_Buffer);
      exception
         when others =>
            Message := To_Unbounded_String ("Failed to write to port");
            return;
      end;

      delay 0.01;

      port.Read (Rx_Buffer, Bytes_Read);

      if Tx_Buffer = Rx_Buffer then
         Success := True;
         Message := To_Unbounded_String ("SUCCESS: Data integrity verified.");
      else
         Message := To_Unbounded_String ("FAILURE: Data mismatch detected.");

         -- Optional: Print where the mismatch happened
         for I in Tx_Buffer'Range loop
            if Tx_Buffer (I) /= Rx_Buffer (I) then
               Message :=
                 To_Unbounded_String
                   ("Mismatch at index: " & Stream_Element_Offset'Image (I));
               exit; -- Stop after finding the first error

            end if;
         end loop;
      end if;

   exception
      when others =>
         Message := To_Unbounded_String ("Exception during test execution");
         Success := False;
   end Run_Test;

end Connection_Tester;
