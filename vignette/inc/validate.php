<?php
/*
 *  Vignette
 *
 *  Copyright (C) 2013 Jonathan Gillett
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

/**
 * Contains a collection of methods that are used to validate POST data, ensure
 * that all POST data has been validated before processing any of the data.
 */


/**
 * Validate the mysql connection is successful.
 * @package validate
 *
 * @param mysqli $mysqli_connection The mysqli connection object
 */
function valid_mysqli_connect($mysqli_connection)
{
    if ($mysqli_connection->connect_errno)
    {
        return FALSE;
    }
    return TRUE;
}
?>
