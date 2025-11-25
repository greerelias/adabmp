with AUnit.Test_Fixtures;

package Connection_Tester_Tests is

   type Test is new AUnit.Test_Fixtures.Test_Fixture with null record;

   procedure Test_Successful_Communication (T : in out Test);
   procedure Test_Failure_Response (T : in out Test);
   procedure Test_No_Response (T : in out Test);

end Connection_Tester_Tests;
