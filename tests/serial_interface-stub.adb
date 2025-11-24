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

   overriding
   procedure Write (Port : in out Mock_Port; Data : String) is
   begin
      Append (Port.Written_Data, Data);
   end Write;

   overriding
   procedure Read
     (Port : in out Mock_Port; Buffer : out String; Last : out Natural)
   is
      Available : constant Natural :=
        Length (Port.Read_Data) - Port.Read_Index + 1;
      To_Read   : constant Natural := Natural'Min (Available, Buffer'Length);
   begin
      if To_Read = 0 then
         Last := 0;
         return;
      end if;

      Buffer (Buffer'First .. Buffer'First + To_Read - 1) :=
        Slice (Port.Read_Data, Port.Read_Index, Port.Read_Index + To_Read - 1);

      Port.Read_Index := Port.Read_Index + To_Read;
      Last := To_Read;
   end Read;

   procedure Set_Input (Port : in out Mock_Port; Data : String) is
   begin
      Port.Read_Data := To_Unbounded_String (Data);
      Port.Read_Index := 1;
   end Set_Input;

   function Get_Output (Port : Mock_Port) return String is
   begin
      return To_String (Port.Written_Data);
   end Get_Output;

end Serial_Interface.Stub;
