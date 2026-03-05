with Serial_Interface;

package UART is
   -- Puts programmer in UART mode
   procedure Start_UART
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : in out Boolean);
end UART;
