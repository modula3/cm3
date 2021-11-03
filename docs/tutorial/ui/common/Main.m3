MODULE Main;
IMPORT Trestle, VBT;
FROM Picture IMPORT StampVBT;

BEGIN
  Trestle.Install(StampVBT);
  Trestle.AwaitDelete(StampVBT);
END Main.
