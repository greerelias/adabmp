with Serial_Interface;
with Ada.Strings.Unbounded;

package Connection_Tester is

   Number_Of_Tests : constant Integer := 1000;
   procedure Get_Programmer_Info
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : out Boolean;
      Info    : out Ada.Strings.Unbounded.Unbounded_String);

   procedure Run_Test
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : out Boolean;
      Message : out Ada.Strings.Unbounded.Unbounded_String);

end Connection_Tester;
