window.bitlovePowerpress = false;

document.addEventListener('DOMContentLoaded', function(){
    if (window.bitlovePowerpress)
	return;
    window.bitlovePowerpress = true;

    var i, ps = document.getElementsByClassName('powerpress_links');
    for(i = 0; i < ps.length; i++) {
	var p = ps[i];
	var j, as = p.getElementsByClassName('powerpress_link_d');
	for(j = 0; j < as.length; j++) {
	    var a = as[j];
	    var url = a.getAttribute('href');

	    if (/.torrent$/.test(url))
		return;

	    (function(p) {
	        resolve(url, function(info) {
		    if (window.console && console.log)
			console.log("Bitlove torrent",url,info);
	            var torrent = info && info.sources && info.sources[0] && info.sources[0].torrent;
	            if (info && torrent) {
		        var t1 = document.createTextNode(" | ");
		        p.appendChild(t1);

		        var a1 = document.createElement('a');
		        a1.textContent = "Torrent";
		        a1.setAttribute('href', torrent);
		        a1.setAttribute('type', "application/x-bittorrent");
                        var title = info.seeders + " Seeder, " + info.leechers + " Leecher";
                        if (info.downloaded == 1)
                            title += ", 1 Download";
                        else if (info.downloaded > 1)
                            title += ", " + info.downloaded + " Downloads";
		        a1.setAttribute('title', title);
		        p.appendChild(a1);

		        var link = info.sources[0].permalink;
		        if (link) {
		            var t2 = document.createTextNode(" on ");
		            p.appendChild(t2);

		            var a2 = document.createElement('a');
		            a2.textContent = "Bitlove";
		            a2.setAttribute('href', link);
		            p.appendChild(a2);
		        }
	            }
	        });
	    })(p);
	}
    }
}, false);
