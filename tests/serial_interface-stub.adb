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

   -- Write String
   overriding
   procedure Write (Port : in out Mock_Port; Data : String) is
   begin
      Port.Read_String := To_Unbounded_String (Data);
   end Write;

   -- Write Stream
   overriding
   procedure Write (Port : in out Mock_Port; Data : Stream_Element_Array) is
   begin
      Port.Read_Stream := Data;
   end Write;

   -- Read String
   overriding
   procedure Read
     (Port : in out Mock_Port; Buffer : out String; Last : out Natural) is
   begin
      Buffer := To_String (Port.Read_String);
      Last := Length (Port.Read_String);
   end Read;

   -- Read Stream
   overriding
   procedure Read
     (Port   : in out Mock_Port;
      Buffer : in out Stream_Element_Array;
      Last   : out Stream_Element_Offset) is
   begin
      Buffer := Port.Read_Stream;
      Last := Port.Read_Stream'Length;
   end Read;

   procedure Set_Input (Port : in out Mock_Port; Data : String) is
   begin
      Port.Read_String := To_Unbounded_String (Data);
   end Set_Input;

   procedure Set_Input (Port : in out Mock_Port; Data : Stream_Element_Array)
   is
   begin
      Port.Read_Stream := Data;
   end Set_Input;

   function Get_Output (Port : Mock_Port) return String is
   begin
      return To_String (Port.Written_Data);
   end Get_Output;

end Serial_Interface.Stub;
