with GNAT.Serial_Communications;

package body Serial_Interface.Impl is

   package GSC renames GNAT.Serial_Communications;

   overriding
   procedure Open (Port : in out Com_Port; Name : String) is
      Name_Id : constant GSC.Port_Name := GSC.Port_Name (Name);
   begin
      GSC.Open (Port.Port, Name_Id);
      GSC.Set (Port.Port, Rate => GSC.B115200, Timeout => 5.0);
   end Open;

   overriding
   procedure Close (Port : in out Com_Port) is
   begin
      GSC.Close (Port.Port);
   end Close;

   overriding
   procedure Write (Port : in out Com_Port; Data : Stream_Element_Array) is
   begin
      GSC.Write (Port.Port, Data);
   end Write;

   overriding
   procedure Read
     (Port   : in out Com_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      GSC.Read (Port.Port, Buffer, Last);
   end Read;

end Serial_Interface.Impl;
