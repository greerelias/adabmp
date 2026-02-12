with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Serial_Interface;

package Configure_Target is
   procedure Load_Bitstream
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Path    : in String;
      Success : in out Boolean);
end Configure_Target;
