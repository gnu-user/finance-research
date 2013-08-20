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


/* 
 * Displays an interactive plot to demonstrate the results of the matching
 * process for any banned symbol selected.
 *
 * DEPENDENCIES
 * --------------
 *
 * - The template depends on the variable $banned_symbols being set, which is
 *   an array containing the list of banned symbols for the quartile.
 */
?>
<div class="hero-unit">
  <div class="page-header">
	  <h1>Matching Process</h1>
  </div>
  <h2>Banned Symbol Matching Results</h2>
  <div class="row">
    <div id="matching_plot"></div>
  </div>
  <div class="row">
    <div class="span8">
      <form class="form-horizontal" accept-charset="UTF-8">
        <div class="control-group">
          <label for="mkt_quartile" class="control-label">Market Cap Quartile</label>
          <div class="controls">
            <select id="mkt_quartile" name="mkt_quartile" class="input-xlarge">
              <option selected="selected">ALL</option>
              <option>Q1 (Smallest)</option>
              <option>Q2</option>
              <option>Q3</option>
              <option>Q4 (Largest)</option>
            </select>
          </div>
        </div>
        <div class="control-group">
          <label for="banned_symbol" class="control-label">Banned Symbol</label>
          <div class="controls">
            <select id="banned_symbol" name="banned_symbol" class="input-xlarge">
              <option selected="selected">UBSI</option>
              <?php
                  foreach ($banned_symbols as $symbol)
                  {
                    echo '<option>' . $symbol . '</option>';
                  }
              ?>
            </select>
          </div>
        </div>
        <div class="control-group">
          <div class="controls">
            <button class="btn btn-inverse" id="match">Show Matching</button>
          </div>
        </div>
      </form>
    </div>
  </div>
</div>
