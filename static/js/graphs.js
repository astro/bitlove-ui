$(function() {
    $('.parent-menu').each(function() {
        var a = $(this).outerWidth();
        var sub = $('.sub-menu', this);
        sub.css({'left': (-(sub.outerWidth() - a)/2) + 'px'});
    })
});

function timeToMS(t) {
    var d = new Date(t);
    return d.getTime() - d.getTimezoneOffset() * 60 * 1000;
}

function humanSize(value) {
    var units = ["", "K", "M", "G", "T"];
    var u = 0;
    while(value >= 1024 && u < units.length - 1) {
	value /= 1024;
	u++;
    }
    var v = (value < 10) ?
	Math.round(value * 10) / 10 :
	Math.round(value);
    return v + " " + units[u] + "B";
};


/**
 * TODO: inherit for the three graph types
 */
function Graph(basepath, type, published) {
    this.basepath = basepath;
    this.type = type;

    this.el = $('<li class="graph"><select class="timeselect"><option value="day">Day</option><option value="week">Week</option><option value="month" selected>Month</option><option value="year">Year</option></select><h4></h4><div class="placeholder"><img src="/static/throbber.gif"></div></li>');
    switch(type) {
	case 'swarm':
	    this.el.find('h4').text("Swarm");
	    break;
	case 'traffic':
	    this.el.find('h4').text("Peer Traffic");
	    break;
	case 'downloads':
	    this.el.find('h4').text("Complete Downloads");
	    break;
    }

    var timeselect = this.el.find('.timeselect');
    var that = this;
    timeselect.change(function() { that.loadGraph(); });
    var age = new Date().getTime() - new Date(published).getTime();
    if (age <= 24 * 60 * 60 * 1000)
	timeselect.val('day');
    else if (age <= 7 * 24 * 60 * 60 * 1000)
	timeselect.val('week');
    else if (age <= 31 * 24 * 60 * 60 * 1000)
	timeselect.val('month');
    else
	timeselect.val('year');
    this.loadGraph();
}

Graph.prototype = {};

Graph.prototype.loadGraph = function() {
    var timespec = this.el.find('.timeselect').val();
    var url = this.basepath + "/g/" + timespec + "/" + this.type + ".json";
    var that = this;
    $.ajax({ url: url,
	     success: function(data) { that.setData(data); },
	     error: function() { that.el.text("Error retrieving " + url); }
	   });
};

Graph.prototype.setData = function(response) {
    var placeholder = this.el.find('.placeholder');

    if (this.plot) {
	this.plot.shutdown();
	placeholder.empty();
	delete this.plot;
    }

    if (response.up && response.up_seeder) {
	/* Stack up on top of up_seeder */
	for(var k in response.up) {
	    var v = response.up[k];
	    if (v) {
		var base = response.up_seeder[k] || 0;
		response.up[k] = [base + v, base];
	    }
	}
    }

    /* Prepare data */
    var i;
    var data = [];
    var type = this.type;
    var interval = response.interval * 1000;
    ["torrent", "downloads",
     "down", "up_seeder", "up",
     "leechers", "seeders",
     "torrent_w", "downloads_w",
     "down_w", "up_seeder_w", "up_w",
     "leechers_w", "seeders_w"
    ].forEach(function(name) {
	if (!response.hasOwnProperty(name))
	    return;

	var line = response[name];
	var d = Object.keys(line).sort().map(function(k) {
	    return [timeToMS(k)].concat(line[k]);
	});

	var label = name;
	var series = {
	    label: name,
	    legend: {
		show: true,
                position: 'sw'
	    },
	    data: d
	};
	switch(type) {
	    case 'swarm':
		series.label = "Downloads";
		series.lines = {
		    show: true,
		    lineWidth: 3
		};
		switch(name) {
		    case 'seeders':
			series.label = "Seeders";
			series.color = '#4faf1f';
			break;
		    case 'seeders_w':
			series.label = "WebTorrent Seeders";
			series.color = '#6fcf3f';
			break;
		    case 'leechers':
			series.label = "Leechers";
			series.color = '#1f4faf';
			break;
		    case 'leechers_w':
			series.label = "Leechers";
			series.color = '#3f6fcf';
			break;
		}
		break;
	    case 'traffic':
		series.bars = { show: true,
				barWidth: interval
			      };
		switch(name) {
		    case 'down':
			series.label = "Leeched by peers";
			series.color = '#1f4faf';
			series.bars.fillColor = '#3fafef';
			break;
		    case 'down_w':
			series.label = "Leeched by WebTorrent";
			series.color = '#3f6fcf';
			series.bars.fillColor = '#5fcfff';
			break;
		    case 'up':
			series.label = "Seeded by peers";
			series.color = '#4faf1f';
			series.bars.fillColor = '#afef3f';
			break;
		    case 'up_w':
			series.label = "Seeded by WebTorrent";
			series.color = '#6fcf3f';
			series.bars.fillColor = '#cfff5f';
			break;
		    case 'up_seeder':
			series.label = "Webseeded by Bitlove";
			series.color = '#afaf1f';
			series.bars.fillColor = '#efef3f';
			break;
		    case 'up_seeder_w':
			series.label = "Webseeded to WebTorrent";
			series.color = '#cfcf3f';
			series.bars.fillColor = '#ffff5f';
			break;
		}
		break;
	    case 'downloads':
		series.bars = { show: true,
				barWidth: interval
			      };
		switch(name) {
		    case 'downloads':
			series.label = "Complete";
			series.color = '#5faf1f';
			series.bars.fillColor = '#afef3f';
			break;
		    case 'downloads_w':
			series.label = "WebTorrent complete";
			series.color = '#7fcf3f';
			series.bars.fillColor = '#cfff5f';
			break;
		    case 'torrent':
		    	series.label = ".torrent files";
		    	series.color = '#1f5faf';
		    	series.bars.fillColor = '#3fafef';
		    	break;
		    case 'torrent_w':
		        series.label = "WebTorrent .torrent";
		        series.color = '#3f5fcf';
		        series.bars.fillColor = '#5fcfff';
		        break;
		}
		break;
	}

	data.push(series);
    });

    var tickFormatter = (type != 'traffic') ?
	function(value) {
	    return "" + Math.round(value);
	} :
	humanSize;

    /* Attach */
    var width = this.el.parent().innerWidth() || 400;
    placeholder.attr('style', "width: " + width + "px; height: 200px");
    this.plot = $.plot(placeholder, data, {
	xaxis: {
	    mode: "time",
	    timeformat: interval >= 86400000 ? "%m-%d" : "%m-%d\n%H:%M",
	    tickLength: 7,
	    min: timeToMS(response.start),
	    max: timeToMS(response.stop)
	},
	yaxis: {
	    tickFormatter: tickFormatter
	},
	grid: {
	    hoverable: true
	}
    });

    var that = this;
    var floater;
    placeholder.bind("plothover", function (event, pos, item) {
	if (floater)
	    floater.remove();

        if (item) {
	    var o = that.plot.pointOffset({
		x: item.datapoint[0],
		y: item.datapoint[1]
	    });
	    floater = $('<div class="floater" style="left: ' + o.left + 'px; top: ' + (o.top - 16) + 'px;"></div>');
	    var v = item.datapoint[1];
	    if (item.datapoint[2])
		v -= item.datapoint[2];
	    floater.text(type == 'traffic' ? humanSize(v) : v);
	    placeholder.prepend(floater);
        }
    });

    this.el.fadeIn(200);
};

Graph.prototype.remove = function() {
    var el = this.el;
    this.el.slideUp(300, function() { el.remove(); });
};

function StatsHook(basepath, stats) {
    this.basepath = basepath;
    this.stats = stats;
    this.graphs = {};

    this.attach('.seeders', 'swarm');
    this.attach('.leechers', 'traffic');
    this.attach('.downloads', 'downloads');
}

StatsHook.prototype = {};

StatsHook.prototype.attach = function(sel, type) {
    var toggle = this.stats.find(sel);
    toggle.addClass('toggleable');
    var that = this;
    toggle.click(function() {
	that.toggleGraph(type, toggle);
    });
};

var TZ_OFFSET = "+02:00";
StatsHook.prototype.getPublished = function() {
    return this.stats.parents('article').find('.published').
	text().replace(/\n/, "T") + ":00" + TZ_OFFSET;
};

StatsHook.prototype.toggleGraph = function(type, toggle) {
    if (this.graphs.hasOwnProperty(type)) {
	toggle.removeClass('toggled');
	this.graphs[type].remove();
	delete this.graphs[type];
    } else {
	toggle.addClass('toggled');
	var graph = this.graphs[type] =
	    new Graph(this.basepath, type, this.getPublished());
	graph.el.slideDown(200);
	this.stats.after(graph.el);
    }
};

$('.download').each(function() {
    var download = $(this);
    var m = download.find('.torrent a').attr('href').
	 match(/^(\/.+?)\.torrent$/);
    var basepath = m && m[1];
    var stats = download.find('.stats');
    if (basepath && stats)
	new StatsHook(basepath, stats);
});
