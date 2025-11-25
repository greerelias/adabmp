with GNAT.Serial_Communications;

package body Serial_Interface.Impl is

   package GSC renames GNAT.Serial_Communications;

   overriding
   procedure Open (Port : in out Com_Port; Name : String) is
      Name_Id : constant GSC.Port_Name := GSC.Port_Name (Name);
   begin
      GSC.Open (Port.Port, Name_Id);
      GSC.Set (Port.Port, Rate => GSC.B115200, Timeout => 4.0);
   end Open;

   overriding
   procedure Close (Port : in out Com_Port) is
   begin
      GSC.Close (Port.Port);
   end Close;

   -- Write String
   overriding
   procedure Write (Port : in out Com_Port; Data : String) is
      Buffer : Stream_Element_Array (1 .. Data'Length);
   begin
      for I in Data'Range loop
         Buffer (Stream_Element_Offset (I - Data'First + 1)) :=
           Character'Pos (Data (I));
      end loop;
      GSC.Write (Port.Port, Buffer);
   end Write;

   overriding
   procedure Write (Port : in out Com_Port; Data : Stream_Element_Array) is
   begin
      GSC.Write (Port.Port, Data);
   end Write;

   -- Read String
   overriding
   procedure Read
     (Port : in out Com_Port; Buffer : out String; Last : out Natural)
   is
      SEA_Buffer : Stream_Element_Array (1 .. Buffer'Length);
      SEA_Last   : Stream_Element_Offset;
   begin
      GSC.Read (Port.Port, SEA_Buffer, SEA_Last);
      Last := Natural (SEA_Last);
      for I in 1 .. Last loop
         Buffer (Buffer'First + I - 1) :=
           Character'Val (SEA_Buffer (Stream_Element_Offset (I)));
      end loop;
   end Read;

   overriding
   procedure Read
     (Port   : in out Com_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      GSC.Read (Port.Port, Buffer, Last);
   end Read;

end Serial_Interface.Impl;
