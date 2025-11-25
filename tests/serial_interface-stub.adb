with Ada.Streams; use Ada.Streams;

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
   begin
      if Port.Loopback_Enabled then
         if Data'Length <= Port.Read_Stream'Length then
            Port.Read_Stream (1 .. Data'Length) := Data;
            Port.Read_Count := Data'Length;
            Port.Read_Index := 1;
         end if;
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
