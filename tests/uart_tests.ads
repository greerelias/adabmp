with AUnit.Test_Fixtures;

package UART_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_UART_Success (T : in out Test);
   procedure Test_UART_Bad_Response (T : in out Test);
   procedure Test_UART_No_Response (T : in out Test);
end UART_Tests;
