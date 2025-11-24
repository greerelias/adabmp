with Serial_Interface;
with Ada.Strings.Unbounded;

package Connection_Tester is

   procedure Run_Test
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Command : String;
      Expect  : String;
      Success : out Boolean;
      Message : out Ada.Strings.Unbounded.Unbounded_String);

end Connection_Tester;
