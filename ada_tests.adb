with Ada.Text_IO; use Ada.Text_IO;
with My_Test_Suite;
with AUnit.Run;
with AUnit.Reporter.Text;

procedure Ada_Tests is
   procedure Run is new AUnit.Run.Test_Runner (My_Test_Suite.Suite);
   Reporter : AUnit.Reporter.Text.Text_Reporter;
begin
   Run (Reporter);

end Ada_Tests;
