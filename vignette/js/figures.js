/*
 * CS-CLUB Elections Website
 *
 * Copyright (C) 2013 Jonathan Gillett, Joseph Heron, Computer Science Club at DC and UOIT
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
    $.ajax({
      type: 'GET',
      url: rootURL + '/pricevol/' + "UBSI",
      dataType: "json", // data type of response
      success: function(data) {
          plotMatch(data);
      }
    });
});


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
		        text: 'AAPL Historical'
		    },

        legend:{
            enabled: true
        },

		    yAxis: [{
		        title: {
		            text: 'Price'
		        },
		        height: 200,
		        lineWidth: 2
		    }, {
		        title: {
		            text: 'Volume'
		        },
		        top: 300,
		        height: 200,
		        offset: 0,
		        lineWidth: 2
		    }],
		    
        xAxis : {
				    plotLines : [{
					    value : Date.UTC(2008,8,19),
					    color : '#1aadce',
					    dashStyle : 'shortdash',
					    width : 2.5,
					    label : {
						    text : 'Ban Started'
					    }
				    }, {
					    value : Date.UTC(2008,9,9),
					    color : '#1aadce',
					    dashStyle : 'shortdash',
					    width : 2.5,
					    label : {
						    text : 'Ban Ended'
					    }
				    }]
			  },

		    series: [{
            id: 'Banned Price',
		        type: 'spline',
            color: '#377EB8',
            lineWidth: 2.5,
		        name: priceSeries[0].name,
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
		        name: priceSeries[1].name,
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
		        name: volumeSeries[0].name,
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
		        name: volumeSeries[1].name,
		        data: volumeSeries[1].volume,
		        yAxis: 1,
            linkedTo: 'Matched Price',
		        dataGrouping: {
					      units: groupingUnits
		        }
		    }]
		});
};


/* Get the postfix identifier based on the position */
function getId(prefix, position) {
    /* Get the name of the id based on position */
    switch (position.toLowerCase()) {
        case 'president':
            prefix += 'president';
            break;
        case 'vice president':
            prefix += 'vicepresident';
            break;
        case 'coordinator':
            prefix += 'coordinator';
            break;
        case 'treasurer':
            prefix += 'treasurer';
            break;
        default:
            console.log('No matching position for: ' + position);
            prefix = '';
            break;
    }
    return prefix;
}

/* Get the vote breakdown for an election for all positions */
function voteBreakdownAll(type) {
    $.ajax({
        type: 'GET',
        url: rootURL + '/results/' + type,
        dataType: "json", // data type of response
        success: function(data) {
            plotVoteBreakdown(data, type);
        }
    });
}

/* Creates a pie chart for the vote break down of each position */
function plotVoteBreakdown(data, type) {
    /* Set the id prefix */
    var idPrefix = 'pie_';
    switch (type.toLowerCase()) {
        case 'election':
            idPrefix += 'elec_';
            break;
        case 'nomination':
            idPrefix += 'nom_';
            break;
        default:
            console.log('No matching type for: ' + type);
            break;
    }

    /* Create a separate pie chart for each position */
    $.each(data, function(position, value) {
        var results = value == null ? [] : (value instanceof Array ? value : [value]);

        if (results.length > 0)
        {
            plotPieChart(getId(idPrefix, position), position, results);
        }
    });
}

/* Plot the results as a pie chart */
function plotPieChart(id, title, results) {
    var chart;
    var options = {
        chart: {
            renderTo: id,
            plotBackgroundColor: null,
            plotBorderWidth: null,
            plotShadow: false,
            backgroundColor:'rgba(255, 255, 255, 0.1)'
        },
        title: {
            text: title
        },
        tooltip: {
            formatter: function() {
                var s;
                if (this.point.name) { // the pie chart
                    s = this.point.name + ': <b>' + this.y + ' Votes</b>';
                } 
                else {
                    s = this.x  + ': ' + this.y;
                }
                return s;
            }
        },
        plotOptions: {
            pie: {
                allowPointSelect: true,
                cursor: 'pointer',
                dataLabels: {
                    enabled: true,
                    color: '#000000',
                    connectorColor: '#000000',
                    formatter: function() {
                        return '<b>' + this.point.name +'</b><br />' + '<b>' + this.percentage.toFixed(1) +' %</b>';
                    }
                }
            }
        },
        series: [{
            type: 'pie',
            name: 'Vote Breakdown',
            data: []
        }]
    };

    /* Sort the results based on the number of votes */
    results = results.sort(function(a, b) {
        return b.value - a.value;
    });

    var first = true;
    $.each(results, function(index, candidate) {
        /* Add a cut-out in the pie chart for the first entry */
        if (first) {
            options.series[0].data.push({
                name: candidate.name,
                y: candidate.votes,
                sliced: true,
                selected: true
            })
            first = false;
        } 
        else {
            options.series[0].data.push([candidate.name, candidate.votes]);
        }
    });

    chart = new Highcharts.Chart(options);
}
