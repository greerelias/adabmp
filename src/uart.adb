with GNAT.Spitbol;
with Protocol;
with Commands;

package body UART is
   procedure Start_UART
     (Port    : in out Serial_Interface.Serial_Port'Class;
      Success : in out Boolean) is
   begin
      Protocol.Send_Command_Packet (Port, Commands.Start_UART);
      delay (0.002);
      Success := Protocol.Receive_Ready_Packet (Port);
   exception
      when others =>
         Success := False;
   end Start_UART;
end UART;
