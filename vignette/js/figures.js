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

/* The root URL for REST api */
var rootURL = "http://vignette.dom/api";


$(document).ready(function () {
    /* Set the default symbol to ban on load */
    var symbol = $('#banned_symbol').val();

    /* Display the default plot */
    $.ajax({
      type: 'GET',
      url: rootURL + '/pricevol/' + symbol,
      dataType: 'json', // data type of response
      success: function(data) {
          plotMatch(data);
      }
    });

    /* Update the list of symbols after selecting a quartile */
    $('#mkt_quartile').click(function(event) {
        var value = $('#mkt_quartile').val();
        var quartile = value.match(/(Q[0-9])/)[1];
        var list = '';

        $.ajax({
            type: 'GET',
            url: rootURL + '/banned/' + quartile,
            dataType: 'json', 
            success: function(data) {
                length = data.length;

                for (var i = 0; i < length; i++)
                {
                    list += "<option>"+data[i]+"</option>";
                }

                //console.log(list);
                $('#banned_symbol').html(list);
            }
        });
    });

    /* Plot the banned symbol and match when clicking match */
    $('#match').click(function(event) {
        /* Get the banned symbol */
        var symbol = $('#banned_symbol').val();

        /* Display the plot */
        $.ajax({
          type: 'GET',
          url: rootURL + '/pricevol/' + symbol,
          dataType: 'json', // data type of response
          success: function(data) {
              plotMatch(data);
          }
        });

        event.preventDefault();
    });
});


/* Plot the price and volume matching of the banned to the unbanned symbol */
function plotMatch(data) {
	var priceSeries = [],
      volumeSeries = [],
      yAxisOptions = [],
	    i = 0

		// set the allowed units for data grouping
		var groupingUnits = [[
			  'week',                         // unit name
			  [1]                             // allowed multiples
		], [
			  'month',
			  [1, 2, 3, 4, 6]
		]];

    // Create the price and volume datasets
    $.each(data, function(symbol, daily_values) {
        priceSeries[i] = {
            name: symbol, 
            price: []
        }
        volumeSeries[i] = {
            name: symbol, 
            volume: []
        }

		    for (j = 0; j < daily_values.length; j++) {	  
          priceSeries[i].price.push([
              daily_values[j][0], // date
              daily_values[j][1] // price
          ]);
          volumeSeries[i].volume.push([
              daily_values[j][0], // date
              daily_values[j][2] // volume
          ]);
		    };

        i++;
	  });


		$('#matching_plot').highcharts('StockChart', {
        chart: {
            backgroundColor: 'rgba(255, 255, 255, 0.1)'
        },

		    rangeSelector: {
		        selected: 4
		    },

		    title: {
		        text: 'Banned Symbol ' + priceSeries[0].name + 
                  ' Matched to ' +  priceSeries[1].name
		    },

		    yAxis: [{
		        title: {
		            text: 'Price'
		        },
            labels: {
                formatter: function() {
                    return '$' + this.value;
                }
            },
		        height: 200,
		        lineWidth: 2
		    }, {
		        title: {
		            text: 'Volume'
		        },
            labels: {
                formatter: function() {
                    return '$' + this.value;
                }
            },
		        top: 300,
		        height: 200,
		        offset: 0,
		        lineWidth: 2
		    }],
		    
        xAxis : {
				    plotLines : [{
					    value : Date.UTC(2008,8,19),
					    color : '#555555',
					    dashStyle : 'shortdash',
					    width : 2.5,
					    label : {
						    text : 'Ban Started'
					    }
				    }, {
					    value : Date.UTC(2008,9,9),
					    color : '#555555',
					    dashStyle : 'shortdash',
					    width : 2.5,
					    label : {
						    text : 'Ban Ended'
					    }
				    }]
			  },

        tooltip: {
            shared: true,
            pointFormat: '<span style="color:{series.color}">{series.name}</span>: <b>${point.y}</b><br/>',
            changeDecimals: 2,
            valueDecimals: 2
        },

		    series: [{
            id: 'Banned Price',
		        type: 'spline',
            color: '#377EB8',
            lineWidth: 2.5,
		        name: priceSeries[0].name + ' Price',
		        data: priceSeries[0].price,
            yAxis: 0,
		        dataGrouping: {
			          units: groupingUnits
		        }
		    }, 
        {
            id: 'Matched Price', 
		        type: 'spline',
            color: '#E41A1C',
            lineWidth: 2.5,
		        name: priceSeries[1].name + ' Price',
		        data: priceSeries[1].price,
            yAxis: 0,
		        dataGrouping: {
			          units: groupingUnits
		        }
		    },
        {
            id: 'Banned Volume',
		        type: 'spline',
            color: '#377EB8',
            lineWidth: 2,
		        name: volumeSeries[0].name + ' Volume',
		        data: volumeSeries[0].volume,
		        yAxis: 1,
            linkedTo: 'Banned Price', 
		        dataGrouping: {
					      units: groupingUnits
		        }
		    },
        {
            id: 'Matched Volue',
		        type: 'spline',
            color: '#E41A1C',
            lineWidth: 2,
		        name: volumeSeries[1].name + ' Volume',
		        data: volumeSeries[1].volume,
		        yAxis: 1,
            linkedTo: 'Matched Price',
		        dataGrouping: {
					      units: groupingUnits
		        }
		    }]
		});
};
