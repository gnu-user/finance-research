<?php 
/*
 * Vignette
 *
 * Copyright (C) 2013 Jonathan Gillett
 * All rights reserved.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
require_once '../inc/db_interface.php';
require '../inc/auth.php';
require 'Slim/Slim.php';
\Slim\Slim::registerAutoloader();


/*
 * The REST web service api which provides access to statistical and other
* useful data, which can be retrieved from the database using a GET operation
*/
$app = new \Slim\Slim();

/* Gets the list of banned symbols */
$app->get('/banned/:quartile', 'getBanned');
$app->get('/banned', 'getBannedAll');

/* Get the symbol ban and expire date */
$app->get('/banperiod/:symbol', 'getBanPeriod');

/* Get the mean price and volume for the banned and matched symbol */
$app->get('/pricevol/:symbol', 'getPriceVol');

$app->run();


/** 
 * Get the list of banned symbols for the quartile specified.
 * 
 * @package api
 *
 * @param string $quartile The market cap quartile to select symbols from.
 */
function getBanned($quartile)
{
	global $db_user, $db_pass, $db_name;

	/* Connect to the database */
	$mysqli = new mysqli("localhost", $db_user, $db_pass, $db_name);

	/* check connection */
	if (mysqli_connect_errno()) {
		printf("Connect failed: %s\n", mysqli_connect_error());
		exit();
	}

	/* Encode the results as JSON */
	echo json_encode(banned_symbols($mysqli, $quartile));
}


/** 
 * Get the list of all banned symbols.
 * 
 * @package api
 *
 */
function getBannedAll()
{
	global $db_user, $db_pass, $db_name;

	/* Connect to the database */
	$mysqli = new mysqli("localhost", $db_user, $db_pass, $db_name);

	/* check connection */
	if (mysqli_connect_errno()) {
		printf("Connect failed: %s\n", mysqli_connect_error());
		exit();
	}

	/* Encode the results as JSON */
	echo json_encode(banned_symbols($mysqli));
}


/** 
 * Get the ban period for the symbol.
 * 
 * @package api
 *
 */
function getBanPeriod($symbol)
{
	global $db_user, $db_pass, $db_name;

	/* Connect to the database */
	$mysqli = new mysqli("localhost", $db_user, $db_pass, $db_name);

	/* check connection */
	if (mysqli_connect_errno()) {
		printf("Connect failed: %s\n", mysqli_connect_error());
		exit();
	}

	/* Encode the results as JSON */
	echo json_encode(ban_period($mysqli, $symbol));
}


/** 
 * Get the mean price and volume for each symbol.
 * 
 * @package api
 *
 */
function getPriceVol($symbol)
{
	global $db_user, $db_pass, $db_name;

	/* Connect to the database */
	$mysqli = new mysqli("localhost", $db_user, $db_pass, $db_name);

	/* check connection */
	if (mysqli_connect_errno()) {
		printf("Connect failed: %s\n", mysqli_connect_error());
		exit();
	}

	/* Encode the results as JSON */
	echo json_encode(mean_price_vol($mysqli, $symbol));
}
?>
