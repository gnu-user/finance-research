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
 * Contains a collection of the functions that directly interact with the database
 * to provide a convenient database abstraction layer, in the future support could
 * be added to support other databases. At the moment the implementations are
 * specific to MySQL (5.1 is the version tested) and prepared statements are
 * used for all queries to provide a layer of protection against SQL injection.
 */


/** 
 * Gets a list of banned symbols, if the quartile is provided gets the list
 * of banned symbols for that quartile.
 *
 * @param mysqli $mysqli The mysqli connection object
 * @param string $quartile (Optional) the market cap quartile
 *
 * @return array The list of symbols that were banned
 */
function banned_symbols($mysqli, $quartile="ALL")
{
    $symbols = array();

    if ($quartile !== "ALL")
    { 
        /* Get the banned symbols for the quartile specified */
        if ($stmt = $mysqli->prepare("SELECT
                                          DISTINCT(m.symbol)
                                      FROM 
                                          market_data AS d INNER JOIN 
                                          matching AS m 
                                      ON 
                                          d.match_id = m.match_id 
                                      WHERE 
                                          quartile = ?"))
        {
            /* bind parameters for markers */
            $stmt->bind_param('s', $quartile);

            /* execute query */
            $stmt->execute();
            
			      /* bind result variables */
			      $stmt->bind_result($symbol);

			      while ($stmt->fetch())
			      {
                array_push($symbols, $symbol);
			      }

            /* close statement */
            $stmt->close();
        }
    }
    else
    {
        /* Get all banned symbols */
        if ($stmt = $mysqli->prepare("SELECT symbol FROM matching"))
        {
            /* execute query */
            $stmt->execute();

			      /* bind result variables */
			      $stmt->bind_result($symbol);

			      while ($stmt->fetch())
			      {
                array_push($symbols, $symbol);
			      }

            /* close statement */
            $stmt->close();
        }
    }

    return $symbols;
}


/** 
 * Gets the mean price and volume for the banned symbol specified.
 *
 * @param mysqli $mysqli The mysqli connection object
 * @param string $symbol the banned symbol
 *
 * @return array The list of symbols that were banned
 */
function mean_price_vol($mysqli, $symbol)
{
    $price_vol = array();

   /* Get all banned symbols */
    if ($stmt = $mysqli->prepare("SELECT matched FROM matching WHERE symbol = ?"))
    {
        /* bind parameters for markers */
        $stmt->bind_param('s', $symbol);

        /* execute query */
        $stmt->execute();

        /* bind result variables */
        $stmt->bind_result($matched);

        $stmt->fetch();

        /* close statement */
        $stmt->close();
    }

    /* Initialize the price and vol array for each symbol */
    $price_vol[$symbol] = array();
    $price_vol[$matched] = array();

    foreach ($price_vol as $entry => $value)
    {
        /* Get the banned symbols for the quartile specified */
        if ($stmt = $mysqli->prepare("SELECT 
                                          UNIX_TIMESTAMP(STR_TO_DATE(time, '%Y-%m-%d')) * 1000, 
                                          mean_price, 
                                          mean_vol 
                                      FROM 
                                          market_data 
                                      WHERE 
                                          symbol = ?"))
        {
            /* bind parameters for markers */
            $stmt->bind_param('s', $entry);

            /* execute query */
            $stmt->execute();
            
			      /* bind result variables */
			      $stmt->bind_result($time, $mean_price, $mean_vol);

			      while ($stmt->fetch())
			      {
                array_push($price_vol[$entry], array($time, $mean_price, $mean_vol));
			      }

            /* close statement */
            $stmt->close();
        }
    }
    return $price_vol;
}
?>
