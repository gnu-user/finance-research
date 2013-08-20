<?php
/*
 *  Registration Page
 *
 *  Copyright (C) 2013 Jonathan Gillett, Computer Science Club
 *  All rights reserved.
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as
 *  published by the Free Software Foundation, either version 3 of the
 *  License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

require_once 'inc/db_interface.php';
require 'inc/auth.php';

session_start();


/* Connect to the database */
$mysqli = new mysqli("localhost", $db_user, $db_pass, $db_name);

/* check connection */
if (mysqli_connect_errno()) {
	printf("Connect failed: %s\n", mysqli_connect_error());
	exit();
}


 /* Display the template for registration */
include 'templates/header.php';


/* Get a list of the banned symbols for the default plot */
$banned_symbols = banned_symbols($mysqli);

/* The interactive plot demonstrating the matching process */
include 'templates/matching_interactive.php';


/* Include the footer */
include 'templates/footer.php';

/* Reset the session state */
session_unset();
?>
