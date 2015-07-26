<?php
$to      = 'ukoehler@techoverflow.net';
$subject = '[KATranslationCheck-Report] ' . $_POST["reason"];
$url = $_POST['url'];
$msgid = $_POST['msgid'];
$msgstr = $_POST['msgstr'];
$hit = $_POST['hit'];
$filename = $_POST['filename'];
$reason = $_POST['reason'];
$message = "URL: $url\n\nmsgid: $msgid\n\nmsgstr: $msgstr\n\nHit: $hit\n\nFilename: $filename";
$headers = 'From: webmaster@localgrid.de' . "\r\n" .
    'Reply-To: webmaster@localgrid.de' . "\r\n" .
    'X-Mailer: PHP/' . phpversion();

mail($to, $subject, $message, $headers);

echo '{"status":"success"}';
?> 
