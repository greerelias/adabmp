with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;

package Configure_Target is
   procedure Load_Bitstream
     (Port    : Serial_Interface.Serial_Port'Class;
      Path    : String;
      Success : Boolean);
end Configure_Target;
