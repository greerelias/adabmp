with Serial_Interface;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package body Board_Info is

   procedure Get_Board_Info
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Info    : out Unbounded_String) is
   begin
      null;  -- stub, just enough for compilation
   end Get_Board_Info;

end Board_Info;
