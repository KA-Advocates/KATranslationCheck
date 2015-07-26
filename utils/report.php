<?php
$to      = 'ukoehler@techoverflow.net';
$subject = '[KATranslationCheck-Report] ' + $_POST["reason"];
$message = "URL: $_POST['url']\n\nmsgid: $_POST['msgid']\n\n$_POST['msgstr']\n\nHit: $_POST['hit']\n\nFilename: $_POST['filename']";
$headers = 'From: webmaster@localgrid.de' . "\r\n" .
    'Reply-To: webmaster@localgrid.de' . "\r\n" .
    'X-Mailer: PHP/' . phpversion();

mail($to, $subject, $message, $headers);

echo '{"status":"success"}'
?> 