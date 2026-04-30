with Ada.Streams;
with Serial_Interface;
with Interfaces;       use Interfaces;
with Ada.Streams;      use Ada.Streams;

package Jtag_Test is

   Board_Not_Found     : exception;
   Board_Bad_Format    : exception;
   Communication_Error : exception;

   procedure Halt_CPU
     (Port : in out Serial_Interface.Serial_Port'Class);

   procedure Resume_CPU
     (Port : in out Serial_Interface.Serial_Port'Class);

   procedure Get_Target_DM_Status
     (Port : in out Serial_Interface.Serial_Port'Class);


end Jtag_Test;
