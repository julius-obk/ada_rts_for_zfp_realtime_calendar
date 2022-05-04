with My_Unittests;

package body My_Test_Suite is
   use AUnit.Test_Suites;
   Result : aliased Test_Suite;

   Test_1 : aliased My_Unittests.cal_test;

   -----------
   -- Suite --
   -----------

   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      Add_Test (Result'Access, Test_1'Access);
      return Result'Access;
   end Suite;

end My_Test_Suite;
