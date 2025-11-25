package body Protocol is

   function Encode (Data : Stream_Element_Array) return Stream_Element_Array is
      -- Max overhead is 1 byte every 254 bytes + 1 byte at start.
      Max_Len : constant Stream_Element_Offset :=
        Data'Length + (Data'Length / 254) + 2;
      Result  : Stream_Element_Array (1 .. Max_Len);

      Read_Index  : Stream_Element_Offset := Data'First;
      Write_Index : Stream_Element_Offset := 1;
      Code_Index  : Stream_Element_Offset := 1;
      Code        : Stream_Element := 1;
   begin
      --  Reserve space for the first code
      Write_Index := Code_Index + 1;

      while Read_Index <= Data'Last loop
         if Data (Read_Index) = 0 then
            Result (Code_Index) := Code;
            Code := 1;
            Code_Index := Write_Index;
            Write_Index := Write_Index + 1;
         else
            Result (Write_Index) := Data (Read_Index);
            Write_Index := Write_Index + 1;
            Code := Code + 1;

            if Code = 255 then
               Result (Code_Index) := Code;
               Code := 1;
               Code_Index := Write_Index;
               Write_Index := Write_Index + 1;
            end if;
         end if;
         Read_Index := Read_Index + 1;
      end loop;

      Result (Code_Index) := Code;

      return Result (1 .. Write_Index - 1);
   end Encode;

   function Decode (Data : Stream_Element_Array) return Stream_Element_Array is
      Result      : Stream_Element_Array (1 .. Data'Length);
      Read_Index  : Stream_Element_Offset := Data'First;
      Write_Index : Stream_Element_Offset := 1;
      Code        : Stream_Element;
      I           : Stream_Element;
   begin
      while Read_Index <= Data'Last loop
         Code := Data (Read_Index);
         Read_Index := Read_Index + 1;

         if Code = 0 then
            raise Decode_Error;
         end if;

         I := 1;
         while I < Code loop
            if Read_Index > Data'Last then
               raise Decode_Error;
            end if;

            Result (Write_Index) := Data (Read_Index);
            Write_Index := Write_Index + 1;
            Read_Index := Read_Index + 1;
            I := I + 1;
         end loop;

         if Code < 255 and then Read_Index <= Data'Last then
            Result (Write_Index) := 0;
            Write_Index := Write_Index + 1;
         end if;
      end loop;

      return Result (1 .. Write_Index - 1);
   end Decode;

   procedure Send_Packet
     (Port : in out Serial_Interface.Serial_Port'Class;
      Data : Stream_Element_Array)
   is
      Encoded : constant Stream_Element_Array := Encode (Data);
      Packet  : Stream_Element_Array (1 .. Encoded'Length + 1);
   begin
      Packet (1 .. Encoded'Length) := Encoded;
      Packet (Packet'Last) := 0;
      Port.Write (Packet);
   end Send_Packet;

   procedure Receive_Packet
     (Port : in out Serial_Interface.Serial_Port'Class;
      Data : out Stream_Element_Array;
      Last : out Stream_Element_Offset)
   is
      Raw_Buffer    : Stream_Element_Array (1 .. 1024);
      Raw_Last      : Stream_Element_Offset := Raw_Buffer'First - 1;
      Byte          : Stream_Element_Array (1 .. 1);
      L             : Stream_Element_Offset;
      Idx           : Stream_Element_Offset := Raw_Buffer'First;
      Timeout_Count : Integer := 0;
   begin
      -- Read until delimiter
      loop
         Port.Read (Byte, L);
         if L > 0 then
            if Byte (1) = 0 then
               Raw_Last := Idx - 1;
               exit;
            end if;

            if Idx <= Raw_Buffer'Last then
               Raw_Buffer (Idx) := Byte (1);
               Idx := Idx + 1;
            else
               raise Buffer_Overflow;
            end if;
         else
            --  Simple timeout mechanism for tests
            Timeout_Count := Timeout_Count + 1;
            if Timeout_Count > 4 then
               Last := Data'First - 1;
               return;
            end if;
            delay 0.001;
         end if;
      end loop;

      if Raw_Last >= Raw_Buffer'First then
         declare
            Decoded : constant Stream_Element_Array :=
              Decode (Raw_Buffer (1 .. Raw_Last));
         begin
            if Decoded'Length > Data'Length then
               raise Buffer_Overflow;
            end if;
            Data (Data'First .. Data'First + Decoded'Length - 1) := Decoded;
            Last := Data'First + Decoded'Length - 1;
         end;
      else
         Last := Data'First - 1;
      end if;
   end Receive_Packet;

end Protocol;
