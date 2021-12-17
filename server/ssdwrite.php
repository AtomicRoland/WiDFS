<?php

$headers = apache_request_headers();
$file = basename($_SERVER['REQUEST_URI']);

$data = file_get_contents('php://input');
$log = fopen("./widfs.log", "a");
fwrite($log, date('Y-m-d H:i:s') . PHP_EOL);
//fwrite($log, print_r($_POST, true));
fwrite($log, 'Data length: ' . strlen($data) . PHP_EOL);
fwrite($log, PHP_EOL . PHP_EOL);

if (is_file($file)) {
//    $data = isset($_POST['data']) ? $_POST['data'] : '';
    $fh = fopen($file, 'rb+');
    if ($fh) {
        if (isset($headers['X-Write-Range'])) {
            $range = preg_split('/=/', $headers['X-Write-Range']);
            list($start, $end) = preg_split('/-/', $range[1]);
            $length = intval($end) - intval($start);
            fseek($fh, $start, SEEK_SET);
            fwrite($fh, $data, $length);
            header('X-Write-Status: 0 OK');
        } else {
            header('X-Write-Status: 1 ERROR Missing or invalid range');
            // Send the catalog of the image
            $fh = fopen($file, 'rb');
            echo fread($fh, 512);
        }
        fclose ($fh);
    } else {
        header('X-Write-Status: 2 ERROR not writable');
        // Send the catalog of the image
        $fh = fopen($file, 'rb');
        echo fread($fh, 512);
        fclose ($fh);
    }
} else {
    header('X-Write-Status: 3 ERROR not file');
}
