<html>
<body>
<?php
  $nl = chr(13) . chr(10);
  $usr_download = "usr_download.csv";
  if ($email == "" ||
      $Category == "" ||
      $Synopsis == "" ||
      $Originator == "" ||
      $Severity == "" ||
      $Release  == "" ||
      $Description  == "" ||
      $Priority == "" 
     )
  {
     echo "Please fill out <B>all</B> mandatory fields.<p>";
     echo "Return to problem report form";
     echo "<form>";
     echo "<input type='button' value ='OK' onClick=history.go(-1)>";
     echo "</form>";
  } else {
     //$emailaddr = "wagner@elego.de";
     $emailaddr = "bugs@elego.de";
     $subject = "$Synopsis";

     $msg = 
       ">Submitter-Id: "   . $SubmitterId  . $nl .
       ">Originator: "     . $Originator   . $nl .
       ">Synopsis: "       . $Synopsis     . $nl .
       ">Confidential: "   . $Confidential . $nl .
       ">Category: "       . $Category     . $nl .
       ">Severity: "       . $Severity     . $nl .
       ">Priority: "       . $Priority     . $nl .
       ">Class: "          . $Class        . $nl .
       ">Release: "        . $Release      . $nl .
       ">Environment: "    . $nl . $Environment   . $nl .
       ">Description: "    . $nl . $Description   . $nl .
       ">How-To-Repeat: "  . $nl . $HowToRepeat   . $nl .
       ">Fix: "            . $nl . $Fix           . $nl;

     mail(
       $emailaddr, $subject, $msg, 
       "From: "           . $email        . $nl .
       "Cc: "             . $cc           . $nl
       //"X-GNATS-Notify: " . $Notify       . $nl
     ); 
     echo "Thank you for your problem report. It has been sent to ";
     echo "$emailaddr.<br><br>";
     echo "<a href=\"index.html\">back to index page</a>";
     // echo "<br>";
     // echo "<pre>";
     // echo "$msg";
     // echo "</pre>";
  }
?>
</body>
</html>

