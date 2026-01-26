with Serial_Interface;
with Ada.Strings.Unbounded;

package Board_Info is

   type Board_Info_Record is record
      Board_Type : String (1 .. 16);
      Revision   : String (1 .. 8);
   end record;

   Board_Not_Found  : exception;
   Board_Bad_Format : exception;
   Communication_Error : exception;
   
   procedure Get_Board_Info
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Info    : out Ada.Strings.Unbounded.Unbounded_String);

end Board_Info;