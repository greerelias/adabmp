with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Exceptions;        use Ada.Exceptions;
with Packet_Formatter;      use Packet_Formatter;
with Commands;
with Protocol;

package body Debug is

   procedure Debug
     (Port : in out Serial_Interface.Serial_Port'Class)
   is begin
      Put_Line("Hello World!");
   end Debug;

end Debug;